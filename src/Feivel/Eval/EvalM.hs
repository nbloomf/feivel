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
{-# OPTIONS_GHC -XFlexibleInstances    #-}

module Feivel.Eval.EvalM (
  EvalM, runEvalM,  attempt, attemptWith, attemptsWith, tryEvalM, getVal, putVal, putTypeVal,

  -- IO and Parsing
  readAndParseDocFromLib, parseAsAt,
  readAndParseDoc, readAndParseStates, parsePaths,

  -- State
  defineKey, lookupKey, getState, putState, clearState, toState,
  isKeyDefined, undefineKey, lookupLibPaths, mergeStateEvalM, addKeyToStore, 

  -- Randomness
  randomElementEvalM, shuffleEvalM, sampleEvalM,
  observeIntegerUniform, observeBinomial, observeIntegerPoisson
) where


{-------------}
{- Contents  -}
{-  :EvalM   -}
{-  :IO      -}
{-  :Parsing -}
{-  :Random  -}
{-  :State   -}
{-------------}

import Feivel.Store
import Feivel.Grammar
import Feivel.Parse

import Feivel.Error

import Control.Monad.Trans.Error
import qualified Control.Monad.Trans.State.Lazy as MTS
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

getVal :: (ToExpr a, Get b, HasLocus a) => a -> EvalM b
getVal x = tryEvalM (locusOf x) $ get (toExpr x)

putVal :: (Put a) => Locus -> a -> EvalM Expr
putVal loc a = return $ put loc a

putTypeVal :: (Put a) => Type -> Locus -> a -> EvalM Expr
putTypeVal typ loc a = return $ putType typ loc a

{----------}
{- :EvalM -}
{----------}

type EvalM = ErrorT Goof (MTS.StateT (Store Expr) (RVarT IO))

runEvalM :: (Store Expr) -> EvalM t -> IO (Either Goof t, Store Expr)
runEvalM state = sampleRVarT . ($ state) . MTS.runStateT . runErrorT


-- Run an EvalM t and report any errors; if none, inject the result to IO.
attemptWith :: Store Expr -> EvalM t -> IO t
attemptWith state x = do
  (result, _) <- runEvalM state x
  case result of
    Left err -> hPutStrLn stderr (show err) >> exitWith (ExitFailure 1)
    Right t  -> return t

attemptsWith :: [Store Expr] -> EvalM t -> IO [t]
attemptsWith states x = sequence $ map (\state -> attemptWith state x) states

attempt :: EvalM t -> IO t
attempt = attemptWith emptyStore


tryEvalM :: (PromoteError err) => Locus -> Either err a -> EvalM a
tryEvalM loc (Left err) = reportErr loc err
tryEvalM _   (Right a)  = return a



{-------}
{- :IO -}
{-------}

-- Read contents of a file, reporting any errors.
-- In case path is an empty string, read from stdin.
readPath :: FilePath -> EvalM String
readPath ""   = liftIO getContents
readPath path = do
  content <- liftIO $ try $ readFile path :: EvalM (Either IOError String)
  case content of
    Left  err -> reportErr NullLocus err
    Right str -> return str


-- Look for a file among a list of paths and read its contents, reporting any errors
-- (Defaults to fvl.lib/ in home directory)
findFileInPaths :: FilePath -> [FilePath] -> EvalM (String, FilePath)
findFileInPaths file [] = do
  home <- liftIO getHomeDirectory
  let defaultPath = home ++ "/fvl.lib/" ++ file
  str <- readPath defaultPath
  return (str, defaultPath)

findFileInPaths file (p:ps) = do
  let current = p ++ file
  content <- liftIO $ try $ readFile current :: EvalM (Either IOError String)
  case content of
    Left  _   -> findFileInPaths file ps
    Right str -> return (str, current)



{------------}
{- :Parsing -}
{------------}

-- Message then string being parsed
parseWith :: ParseM a -> String -> String -> EvalM a
parseWith p path str = case runParseM p path str of
  Left  goof -> throwError goof
  Right x    -> return x

parseAsAt :: ParseM a -> Locus -> String -> EvalM a
parseAsAt p _ str = case runParseM p "" str of
  Left  goof -> throwError goof
  Right x    -> return x

parsePaths :: String -> EvalM [FilePath]
parsePaths ""  = return []
parsePaths str = case runParseM pPaths "" str of
  Left goof -> throwError goof
  Right ps -> return ps

readAndParseDoc :: FilePath -> EvalM Doc
readAndParseDoc path = do
  file <- readPath path
  parseWith pDOC path file

readAndParseDocFromLib :: FilePath -> EvalM Doc
readAndParseDocFromLib file = do
  lib <- lookupLibPaths
  (str, path) <- findFileInPaths file lib
  parseWith pDOC path str


readAndParseStates :: FilePath -> DataFormat -> EvalM [Store Expr]
readAndParseStates "" _ = return [emptyStore]
readAndParseStates path format = do
  file <- readPath path
  parseStates path file format
    where
      parseStates :: String -> String -> DataFormat -> EvalM [Store Expr]
      parseStates _ "" _ = return [emptyStore]
      parseStates msg str fmt = do
        case runParseM (pRecords fmt) msg str of
          Left goof -> throwError goof
          Right rs  -> case sequence $ map fromKeyValLocList rs of
            Left err  -> reportErr NullLocus err
            Right sts -> return sts



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
getState = lift MTS.get

putState :: (Store Expr) -> EvalM ()
putState st = lift $ MTS.put st

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

mergeStateEvalM :: (Store Expr) -> EvalM ()
mergeStateEvalM new = do
  old <- getState
  putState $ mergeState old new

lookupLibPaths :: EvalM [FilePath]
lookupLibPaths = do
  state <- getState
  return $ getLibPaths state


undefineKey :: Key -> EvalM ()
undefineKey k = do
  state <- getState
  putState $ removeKey k state
