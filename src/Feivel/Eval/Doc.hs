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

{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}

module Feivel.Eval.Doc where

import Feivel.EvalM
import Feivel.Eval.Eval
import Feivel.Eval.Util
import Feivel.Expr
import Feivel.Type
import Feivel.Key
import Feivel.Get
import Feivel.Typed
import Feivel.Lib
import Feivel.Locus
import Feivel.Error
import Feivel.Parse (pInteger)
import Feivel.Store

import Data.List (intersperse)

{-------------}
{- :Eval:Doc -}
{-------------}

evalToGlyph :: (Eval Expr, Glyph Expr, ToExpr a) => a -> EvalM String
evalToGlyph x = eval (toExpr x) >>= toGlyph

instance Glyph (Doc Expr) where
  toGlyph (Empty            :@ _) = return ""
  toGlyph (DocText (Text s) :@ _) = return s
  toGlyph x = error $ "toGlyph: Doc: " ++ show x

instance (Eval Expr, Glyph Expr) => Eval (Doc Expr) where
  eval (Empty :@ loc)     = return (Empty :@ loc)
  eval (DocText s :@ loc) = return (DocText s :@ loc)
  eval (Escaped c :@ loc) = return (DocText (Text [c]) :@ loc)

  eval (ShowState :@ loc) = do
    st <- getState
    return $ DocText (Text $ show st) :@ loc

  eval (NakedKey k :@ loc) = do
    expr <- lookupKey loc k
    s <- evalToGlyph expr
    return $ DocText (Text s) :@ loc

  eval (DocMacro vals mac :@ loc) = eMacro vals mac loc

  eval (Scope body :@ _) = do
    --pushTrace "scope" loc
    current <- getState
    result <- evalWith body current
    --popTrace
    return result

  eval (IfThenElse b true false :@ _) = do
    --pushTrace "if-then-else" loc
    x <- eval b >>= getVal
    result <- case x of
                True  -> eval true
                False -> eval false
    --popTrace
    return result

  eval (NakedExpr expr :@ loc) = do
    x <- evalToGlyph expr
    return $ DocText (Text x) :@ loc

  eval (Import file Nothing rest :@ _) = do
    oldSt <- getState
    clearState
    _ <- readAndParseDocFromLib file >>= eval
    newSt <- getState
    putState oldSt
    mergeStateEvalM newSt
    eval rest

  eval (Import file (Just prefix) rest :@ _) = do
    oldSt <- getState
    clearState
    _ <- readAndParseDocFromLib file >>= eval
    newSt <- getState
    putState oldSt
    mergeStateEvalM (qualify prefix newSt)
    eval rest

  eval (Cat [] :@ loc) = return $ Empty :@ loc
  eval (Cat ts :@ loc) = do
    exprs <- sequence [eval t >>= getVal | t <- ts]
    return $ DocText (concatText exprs) :@ loc

  eval (CatPar [] :@ loc) = return $ Empty :@ loc
  eval (CatPar ts :@ loc) = do
    let foo x = do
          st <- getState
          y <- eval x >>= getVal
          putState st
          return y
    exprs <- sequence $ map foo ts
    return $ DocText (concatText exprs) :@ loc


  eval (Cond [] defa :@ _) = do
    --pushTrace "cond" loc
    result <- eval defa
    --popTrace
    return result

  eval (Cond ((c,t):ds) defa :@ loc) = do
    x <- eval c >>= getVal
    if x
      then eval t
      else eval $ Cond ds defa :@ loc


  eval (Define t k v rest :@ loc) = do
    w <- eval v
    let tw = typeOf w
    if t == tw
      then do
        defineKey k w loc
        eval rest
      else reportErr loc $ TypeMismatch t tw


  eval (LetIn key val expr :@ loc) = do
    defineKey key val loc
    t <- eval expr
    undefineKey key
    return t

  eval (Bail s :@ loc) = (eval s) >>= getVal >>= (reportErr loc . BailMessage . unText)

  eval (Alt [] :@ loc) = return $ Empty :@ loc
  eval (Alt ts :@ _)   = randomElementEvalM ts >>= eval

  eval (Shuffle xs :@ loc) = do
    x <- shuffleEvalM xs
    eval $ Cat x :@ loc


  eval (ForSay k ls body Nothing :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    eval $ CatPar [LetIn k x body :@ loc | x <- xs] :@ loc

  eval (ForSay k ls body (Just sep) :@ loc) = do
    xs <- eval ls  >>= getVal
    b  <- eval sep >>= getVal
    eval $ CatPar (intersperse b [LetIn k x body :@ loc | x <- xs]) :@ loc


  eval (Select k ls body :@ loc) = do
    xs <- eval ls >>= getVal
    x  <- randomElementEvalM xs
    eval $ LetIn k x body :@ loc