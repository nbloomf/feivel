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

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feivel.Eval.Bool () where

import Feivel.Eval.Util


instance Glyph BoolExpr where
  toGlyph (BoolExpr (BoolConst True  :@ _)) = return "#t"
  toGlyph (BoolExpr (BoolConst False :@ _)) = return "#f"
  toGlyph x = error $ "toGlyph: BoolExpr: " ++ show x


instance (Eval Expr, Eval IntExpr, Eval ListExpr, Eval MatExpr) => Eval BoolExpr where
  eval (BoolExpr (zappa :@ loc)) = case zappa of
    BoolConst b -> return (BoolExpr $ BoolConst b :@ loc)

    {- :Common -}
    BoolVar        key      -> eKey key loc
    BoolAtIdx      m h k    -> eAtIdx m h k loc
    BoolIfThenElse b t f    -> eIfThenElse b t f
    BoolMacro      vals mac -> eMacro vals mac loc

    BoolAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [BoolExpr] -> Integer -> Either ListErr BoolExpr

    IsDefined key -> do
      p <- isKeyDefined key
      putVal loc p >>= getVal

    BoolEq a b -> do
      x <- eval a >>= getVal :: EvalM Expr
      y <- eval b >>= getVal :: EvalM Expr
      putVal loc (x == y) >>= getVal

    BoolNEq a b -> do
      x <- eval a >>= getVal :: EvalM Expr
      y <- eval b >>= getVal :: EvalM Expr
      putVal loc (x /= y) >>= getVal

    BoolLT a b -> do
      case unify (typeOf a) (typeOf b) of
        Right ZZ -> do
          x <- eval a >>= getVal :: EvalM Integer
          y <- eval b >>= getVal :: EvalM Integer
          putVal loc (x < y) >>= getVal
        Right SS -> do
          x <- eval a >>= getVal :: EvalM Text
          y <- eval b >>= getVal :: EvalM Text
          putVal loc (x < y) >>= getVal
        Right QQ -> do
          x <- eval a >>= getVal :: EvalM Rat
          y <- eval b >>= getVal :: EvalM Rat
          putVal loc (x < y) >>= getVal
        Right u -> reportErr loc $ SortableExpected u
        Left err -> reportErr loc err

    BoolLEq a b -> do
      case unify (typeOf a) (typeOf b) of
        Right ZZ -> do
          x <- eval a >>= getVal :: EvalM Integer
          y <- eval b >>= getVal :: EvalM Integer
          putVal loc (x <= y) >>= getVal
        Right SS -> do
          x <- eval a >>= getVal :: EvalM Text
          y <- eval b >>= getVal :: EvalM Text
          putVal loc (x <= y) >>= getVal
        Right QQ -> do
          x <- eval a >>= getVal :: EvalM Rat
          y <- eval b >>= getVal :: EvalM Rat
          putVal loc (x <= y) >>= getVal
        Right u -> reportErr loc $ SortableExpected u
        Left err -> reportErr loc err

    BoolGT a b -> do
      case unify (typeOf a) (typeOf b) of
        Right ZZ -> do
          x <- eval a >>= getVal :: EvalM Integer
          y <- eval b >>= getVal :: EvalM Integer
          putVal loc (x > y) >>= getVal
        Right SS -> do
          x <- eval a >>= getVal :: EvalM Text
          y <- eval b >>= getVal :: EvalM Text
          putVal loc (x > y) >>= getVal
        Right QQ -> do
          x <- eval a >>= getVal :: EvalM Rat
          y <- eval b >>= getVal :: EvalM Rat
          putVal loc (x > y) >>= getVal
        Right u -> reportErr loc $ SortableExpected u
        Left err -> reportErr loc err

    BoolGEq a b -> do
      case unify (typeOf a) (typeOf b) of
        Right ZZ -> do
          x <- eval a >>= getVal :: EvalM Integer
          y <- eval b >>= getVal :: EvalM Integer
          putVal loc (x >= y) >>= getVal
        Right SS -> do
          x <- eval a >>= getVal :: EvalM Text
          y <- eval b >>= getVal :: EvalM Text
          putVal loc (x >= y) >>= getVal
        Right QQ -> do
          x <- eval a >>= getVal :: EvalM Rat
          y <- eval b >>= getVal :: EvalM Rat
          putVal loc (x >= y) >>= getVal
        Right u -> reportErr loc $ SortableExpected u
        Left err -> reportErr loc err

    BoolRand ls -> do
      xs <- eval ls >>= getVal
      r  <- randomElementEvalM xs :: EvalM Bool
      putVal loc r >>= getVal

    ListElem x xs -> do
      a <- eval x >>= getVal :: EvalM Expr
      as <- eval xs >>= getVal :: EvalM [Expr]
      putVal loc (elem a as) >>= getVal

    ListIsEmpty xs -> do
      as <- eval xs >>= getVal :: EvalM [Expr]
      putVal loc (null as) >>= getVal

    MatIsRow m -> do
      p <- eval m >>= getVal :: EvalM (Matrix Expr)
      q <- tryEvalM loc $ mIsRow p
      putVal loc q >>= getVal

    MatIsCol m -> do
      p <- eval m >>= getVal :: EvalM (Matrix Expr)
      q <- tryEvalM loc $ mIsCol p
      putVal loc q >>= getVal

    MatIsGJForm m -> do
      case typeOf m of
        MatOf QQ -> do
          p <- eval m >>= getVal :: EvalM (Matrix Rat)
          q <- tryEvalM loc $ mIsGaussJordanForm p
          putVal loc q >>= getVal
        MatOf BB -> do
          p <- eval m >>= getVal :: EvalM (Matrix Bool)
          q <- tryEvalM loc $ mIsGaussJordanForm p
          putVal loc q >>= getVal
        t -> reportErr loc $ NumericMatrixExpected t

    -- Bool
    Neg  a   -> lift1 loc a   (boolNot)
    Conj a b -> lift2 loc a b (boolAnd)
    Disj a b -> lift2 loc a b (boolOr)
    Imp  a b -> lift2 loc a b (boolImp)

    -- Int
    IntSqFree a -> lift1 loc a   (rIsSqFreeT zeroZZ)
    IntDiv a b -> lift2 loc a b (rDividesT  zeroZZ)

    -- Str
    Matches a b -> lift2 loc a b (strMatch)
