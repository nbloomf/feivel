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

{-# LANGUAGE FlexibleContexts #-}

module Feivel.Eval.Util where

import Feivel.EvalM
import Feivel.Eval.Eval
import Feivel.Get
import Feivel.Put
import Feivel.Locus
import Feivel.Key
import Feivel.Store
import Feivel.Expr
import Feivel.Type
import Feivel.Lib (Matrix, mEntryOf)

-- eval with a specified store
evalWith :: (Eval t) => t -> Store Expr -> EvalM t
evalWith t st = do
  old <- getState
  putState st
  u <- eval t
  putState old
  return u

eKey :: (Eval a, Get a) => Key -> Locus -> EvalM a
eKey key loc = lookupKey loc key >>= getVal >>= eval

eIfThenElse :: (ToExpr a, Get a, Eval a, Eval b, ToExpr b) => b -> a -> a -> EvalM a
eIfThenElse b t f = do
  test  <- eval b >>= getVal
  true  <- eval t >>= getVal
  false <- eval f >>= getVal
  if test then (eval true) else (eval false)

eAtIdx :: (ToExpr a, ToExpr b, ToExpr c, Get (Matrix d), Eval a, Eval b, Eval c)
  => c -> a -> b -> Locus -> EvalM d
eAtIdx m h k loc = do
  i <- eval h >>= getVal
  j <- eval k >>= getVal
  p <- eval m >>= getVal
  tryEvalM loc $ mEntryOf (i,j) p


eMacro :: (Get b, Eval b, Eval Expr) => [(Type, Key, Expr)] -> Expr -> Locus -> EvalM b
eMacro vals mac loc = do
  old <- getState
  ctx <- toStateT loc vals
  (defaultVals, e) <- evalWith mac (mergeStores [ctx, old]) >>= getVal :: EvalM (Store Expr, Expr)
  let newSt = mergeStores [ctx, defaultVals, old]
  evalWith e newSt >>= getVal >>= (`evalWith` newSt)