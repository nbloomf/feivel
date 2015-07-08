{---------------------------------------------------------------------}
{- Copyright 2015 Nathan Bloomfield                                  -}
{-                                                                   -}
{- This file is part of Feivel.                                      -}
{-                                                                   -}
{- Feivel is free software: you can redistribute it and/or modify    -}
{- it under the terms of the GNU General Public License version 3,   -}
{- as published by the Free Software Foundation.                     -}
{-                                                                   -}
{- Feivel is distributed in the hope that it will be useful, but     -}
{- WITHOUT ANY WARRANTY; without even the implied warranty of        -}
{- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      -}
{- GNU General Public License for more details.                      -}
{-                                                                   -}
{- You should have received a copy of the GNU General Public License -}
{- along with Feivel. If not, see <http://www.gnu.org/licenses/>.    -}
{---------------------------------------------------------------------}

{-# OPTIONS_GHC -XTypeSynonymInstances #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module Feivel.EvalM (
  EvalM, runEvalM, runE, attempt, attemptWith, attemptWith', attemptsWith, testEvalM,
  tryEvalM,

  -- IO and Parsing
  readAndParseDoc, readAndParseStates, parseDoc, parsePaths, readAndParseDocFromLib,

  -- State
  defineKey, lookupKey, getState, putState, clearState, toState,
  undefineKeys, undefineKey, isKeyDefined,
  lookupLibPaths,
  addKeyToStore,

  -- Randomness
  randomElementEvalM, shuffleEvalM, sampleEvalM,
  observeIntegerUniform, observeBinomial, observeIntegerPoisson,

  -- Call Stack
  --pushTrace, popTrace, getTrace,
  
  mergeState'
) where

{------------}
{- Contents -}
{-  :EvalM  -}
{-  :Errors -}
{-  :Random -}
{-  :State  -}
{------------}

import Feivel.Error
import Feivel.Expr (Doc, Expr(), DocLeaf(DocText))
import Feivel.Key (Key)
import Feivel.Locus (Locus(NullLocus), AtLocus((:@)))
import Feivel.Store
import Feivel.Parse (pDoc, pRecords)
import Feivel.Format
import Feivel.Parse.ParseM


import Control.Monad.Trans.Error
import Control.Monad.Trans.State.Lazy hiding (state)
import Control.Monad.State.Lazy (lift)

import Data.RVar (sampleRVarT)
import Data.Random (RVarT())
import Data.Random.List (randomElementT, shuffleT, shuffleNofMT)
import Data.Random.Distribution.Uniform (integralUniform)
import Data.Random.Distribution.Binomial (integralBinomial)
import Data.Random.Distribution.Poisson (integralPoisson)

import Control.Monad.IO.Class (liftIO)
import Control.Exception
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO
import System.Directory (getHomeDirectory)


{----------}
{- :EvalM -}
{----------}

type EvalM = ErrorT Goof (StateT (Store Expr) (RVarT IO))

runEvalM :: (Store Expr) -> EvalM t -> IO (Either Goof t, Store Expr)
runEvalM state = sampleRVarT . ($ state) . runStateT . runErrorT

runE :: EvalM t -> IO (Either Goof t, Store Expr)
runE = runEvalM emptyStore

-- Run an EvalM t and report any errors; if none, inject the result to IO.
attemptWith :: Store Expr -> EvalM t -> IO t
attemptWith state x = do
  (result, _) <- runEvalM state x
  case result of
    Left err -> hPutStrLn stderr (show err) >> exitWith (ExitFailure 1)
    Right t  -> return t

attemptWith' :: Store Expr -> EvalM t -> IO (t, Store Expr)
attemptWith' state x = do
  (result, st) <- runEvalM state x
  case result of
    Left err -> hPutStrLn stderr (show err) >> exitWith (ExitFailure 1)
    Right t  -> return (t, st)

attemptsWith :: [Store Expr] -> EvalM t -> IO [t]
attemptsWith states x = sequence $ map (\state -> attemptWith state x) states

attempt :: EvalM t -> IO t
attempt = attemptWith emptyStore


testEvalM :: (Show t) => EvalM t -> IO ()
testEvalM expr = do
  (result, _) <- runEvalM emptyStore expr
  case result of
    Left err -> putStrLn $ show err
    Right d  -> putStrLn $ show d

tryEvalM :: (PromoteError err) => Locus -> Either err a -> EvalM a
tryEvalM loc (Left err) = reportErr loc err
tryEvalM _   (Right a)  = return a



{-------}
{- :IO -}
{-------}

readPath :: FilePath -> EvalM String
readPath path = do
  content <- liftIO $ try $ readFile path :: EvalM (Either IOError String)
  case content of
    Left  err -> reportErr NullLocus err
    Right str -> return str

findFileInPaths :: FilePath -> [FilePath] -> EvalM (String, FilePath)
findFileInPaths file [] = do
  home <- liftIO getHomeDirectory
  let current = home ++ "/fvl.lib/" ++ file
  content <- liftIO $ try $ readFile current :: EvalM (Either IOError String)
  case content of
    Left  err -> reportErr NullLocus err
    Right str -> return (str, current)

findFileInPaths file (p:ps) = do
  let current = p ++ file
  content <- liftIO $ try $ readFile current :: EvalM (Either IOError String)
  case content of
    Left  err -> findFileInPaths file ps
    Right str -> return (str, current)


-- Message then string being parsed
parseWith :: ParseM (a,b) -> String -> String -> EvalM a
parseWith p path str = case runParseM p path str of
  Left  goof  -> throwError goof
  Right (x,_) -> return x

parseDoc :: String -> String -> EvalM Doc
parseDoc = parseWith pDoc

parsePaths :: String -> EvalM [FilePath]
parsePaths ""  = return []
parsePaths str = case runParseM pPaths "" str of
  Left goof -> throwError goof
  Right ps -> return ps

parseStringDoc :: String -> EvalM Doc
parseStringDoc str = parseWith pDoc "" str

readAndParseDoc :: FilePath -> EvalM Doc
readAndParseDoc path = do
  file <- if path == "" then liftIO getContents else readPath path
  parseDoc path file

readAndParseDocFromLib :: FilePath -> EvalM Doc
readAndParseDocFromLib file = do
  lib <- lookupLibPaths
  (str, path) <- findFileInPaths file lib
  parseDoc path str

parseStates :: String -> String -> DataFormat -> EvalM [Store Expr]
parseStates _ "" _ = return [emptyStore]
parseStates msg str format = do
  case runParseM (pRecords format) msg str of
    Left goof -> throwError goof
    Right rs -> case sequence $ map fromKeyValLocList rs of
      Left err  -> reportErr NullLocus err
      Right sts -> return sts

readAndParseStates :: FilePath -> DataFormat -> EvalM [Store Expr]
readAndParseStates "" _ = return [emptyStore]
readAndParseStates path format = do
  file <- readPath path
  parseStates path file format

{- :Library -}



{-----------}
{- :Random -}
{-----------}

randomElementEvalM :: [t] -> EvalM t
randomElementEvalM = lift . lift . randomElementT

shuffleEvalM :: [t] -> EvalM [t]
shuffleEvalM = lift . lift . shuffleT

sampleEvalM :: Int -> [t] -> EvalM [t]
sampleEvalM k xs = lift $ lift $ shuffleNofMT k n xs
  where n = length xs


observeIntegerUniform :: Locus -> (Integer, Integer) -> EvalM Integer
observeIntegerUniform loc (lo, hi) = do
  if hi < lo
    then reportErr loc EmptySampleSpace
    else lift $ lift $ integralUniform lo hi

observeBinomial :: Locus -> Integer -> Double -> EvalM Integer
observeBinomial loc n p = do
  if n <= 0
    then reportErr loc EmptySampleSpace
    else if p < 0 || p > 1
      then reportErr loc $ InvalidProbability p
      else lift $ lift $ integralBinomial n p

observeIntegerPoisson :: Locus -> Double -> EvalM Integer
observeIntegerPoisson loc lambda = do
  if lambda <= 0
    then reportErr loc $ NonpositiveParameter lambda
    else lift $ lift $ integralPoisson lambda



{----------}
{- :State -}
{----------}

{- Primitives -}

getState :: EvalM (Store Expr)
getState = lift get

putState :: (Store Expr) -> EvalM ()
putState st = lift $ put st

clearState :: EvalM ()
clearState = putState emptyStore

toState :: Locus -> [(Key, Expr)] -> EvalM (Store Expr)
toState loc vs = case fromKeyValList vs of
  Left err -> reportErr loc err
  Right st -> return st



defineKey :: Key -> Expr -> Locus -> EvalM ()
defineKey key a loc = do
  old <- getState
  case addKey key a loc old of
    Left err  -> reportErr loc err
    Right new -> putState new

addKeyToStore :: Key -> Expr -> Locus -> Store Expr -> EvalM (Store Expr)
addKeyToStore key expr loc st = do
  case addKey key expr loc st of
    Left err -> reportErr loc err
    Right new -> return new


{- Querying -}

isKeyDefined :: Key -> EvalM Bool
isKeyDefined key = do
  state <- getState
  return $ isDefinedKey state key

lookupKey :: Locus -> Key -> EvalM Expr
lookupKey loc key = do
  state <- getState
  case valueOf state key of
    Left err   -> reportErr loc err
    Right expr -> return expr




mergeState' :: (Store Expr) -> EvalM ()
mergeState' new = do
  old <- getState
  putState $ mergeState old new






lookupLibPaths :: EvalM [FilePath]
lookupLibPaths = do
  state <- getState
  return $ getLibPaths state


undefineKeys :: EvalM ()
undefineKeys = do
  state <- getState
  putState $ clearKeys state

undefineKey :: Key -> EvalM ()
undefineKey k = do
  state <- getState
  putState $ removeKey k state
{-
{-
pushTrace :: String -> Locus -> EvalM ()
pushTrace str loc = do
  state <- getState
  putState $ push str loc state

popTrace :: EvalM ()
popTrace = do
  state <- getState
  putState $ pop state

getTrace :: EvalM [(String, Locus)]
getTrace = do
  state <- getState
  return $ getStack state
-}
-}

