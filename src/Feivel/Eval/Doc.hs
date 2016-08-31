{---------------------------------------------------------------------}
{- Copyright 2015, 2016 Nathan Bloomfield                            -}
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

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feivel.Eval.Doc (evalToGlyph) where

import Feivel.Eval.Util

import Data.List (intersperse)
import System.Process (readProcessWithExitCode)
import Control.Monad.IO.Class (liftIO)
import System.Exit (ExitCode(ExitSuccess))


evalToGlyph :: (Eval Expr, Glyph Expr, ToExpr a) => a -> EvalM String
evalToGlyph x = eval (toExpr x) >>= toGlyph


instance Glyph Doc where
  toGlyph (Doc (Empty            :@ _)) = return ""
  toGlyph (Doc (DocText (Text s) :@ _)) = return s
  toGlyph x = error $ "toGlyph: Doc: " ++ show x


instance (Eval Expr, Glyph Expr) => Eval Doc where
  eval (Doc (Empty :@ loc))     = return (Doc $ Empty :@ loc)
  eval (Doc (DocText s :@ loc)) = return (Doc $ DocText s :@ loc)
  eval (Doc (Escaped c :@ loc)) = return (Doc $ DocText (Text [c]) :@ loc)

  eval (Doc (ShowState :@ loc)) = do
    st <- getState
    return $ Doc $ DocText (Text $ show st) :@ loc

  eval (Doc (NakedKey k :@ loc)) = do
    expr <- lookupKey loc k
    s <- evalToGlyph expr
    return $ Doc $ DocText (Text s) :@ loc

  eval (Doc (DocMacro vals mac :@ loc)) = eMacro vals mac loc

  eval (Doc (Scope body :@ _)) = do
    --pushTrace "scope" loc
    current <- getState
    result <- evalWith body current
    --popTrace
    return result

  eval (Doc (IfThenElse b true false :@ _)) = do
    --pushTrace "if-then-else" loc
    x <- eval b >>= getVal
    result <- case x of
                True  -> eval true
                False -> eval false
    --popTrace
    return result

  eval (Doc (NakedExpr expr :@ loc)) = do
    x <- evalToGlyph expr
    return $ Doc $ DocText (Text x) :@ loc

  eval (Doc (Import file Nothing rest :@ _)) = do
    oldSt <- getState
    clearState
    _ <- readAndParseDocFromLib file >>= eval
    newSt <- getState
    putState oldSt
    mergeStateEvalM newSt
    eval rest

  eval (Doc (Import file (Just prefix) rest :@ _)) = do
    oldSt <- getState
    clearState
    _ <- readAndParseDocFromLib file >>= eval
    newSt <- getState
    putState oldSt
    mergeStateEvalM (qualify prefix newSt)
    eval rest

  eval (Doc (Cat [] :@ loc)) = return $ Doc $ Empty :@ loc
  eval (Doc (Cat ts :@ loc)) = do
    exprs <- sequence [eval t >>= getVal | t <- ts]
    return $ Doc $ DocText (concatText exprs) :@ loc

  eval (Doc (CatPar [] :@ loc)) = return $ Doc $ Empty :@ loc
  eval (Doc (CatPar ts :@ loc)) = do
    let foo x = do
          st <- getState
          y <- eval x >>= getVal
          putState st
          return y
    exprs <- sequence $ map foo ts
    return $ Doc $ DocText (concatText exprs) :@ loc


  eval (Doc (Cond [] defa :@ _)) = do
    --pushTrace "cond" loc
    result <- eval defa
    --popTrace
    return result

  eval (Doc (Cond ((c,t):ds) defa :@ loc)) = do
    x <- eval c >>= getVal
    if x
      then eval t
      else eval $ Doc $ Cond ds defa :@ loc


  eval (Doc (Define t k v rest :@ loc)) = do
    w <- eval v
    let tw = typeOf w
    if t == tw
      then do
        defineKey k w loc
        eval rest
      else reportErr loc $ TypeMismatch t tw


  eval (Doc (LetIn key val expr :@ loc)) = do
    defineKey key val loc
    t <- eval expr
    undefineKey key
    return t

  eval (Doc (Bail s :@ loc)) = (eval s) >>= getVal >>= (reportErr loc . BailMessage . unText)

  eval (Doc (Alt [] :@ loc)) = return $ Doc $ Empty :@ loc
  eval (Doc (Alt ts :@ _))   = randomElementEvalM ts >>= eval

  eval (Doc (Shuffle xs :@ loc)) = do
    x <- shuffleEvalM xs
    eval $ Doc $ Cat x :@ loc


  eval (Doc (ForSay k ls body Nothing :@ loc)) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    eval $ Doc $ CatPar [Doc (LetIn k x body :@ loc) | x <- xs] :@ loc

  eval (Doc (ForSay k ls body (Just sep) :@ loc)) = do
    xs <- eval ls  >>= getVal
    b  <- eval sep >>= getVal
    eval $ Doc $ CatPar (intersperse b [Doc (LetIn k x body :@ loc) | x <- xs]) :@ loc


  eval (Doc (Select k ls body :@ loc)) = do
    xs <- eval ls >>= getVal
    x  <- randomElementEvalM xs
    eval $ Doc $ LetIn k x body :@ loc


  eval (Doc (Shell c as Nothing :@ loc)) = do
    args <- sequence $ map evalToGlyph as
    (code,stdout,stderr) <- liftIO $ readProcessWithExitCode c args ""
    if code == ExitSuccess
      then return (Doc $ DocText (Text stdout) :@ loc)
      else reportErr loc $ ExitFail code stdout stderr

  eval (Doc (Shell c as (Just x) :@ loc)) = do
    args <- sequence $ map evalToGlyph as
    st <- getState
    stdin <- evalToGlyph x
    putState st
    (code,stdout,stderr) <- liftIO $ readProcessWithExitCode c args stdin
    if code == ExitSuccess
      then return (Doc $ DocText (Text stdout) :@ loc)
      else reportErr loc $ ExitFail code stdout stderr
