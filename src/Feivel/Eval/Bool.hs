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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feivel.Eval.Bool () where

import Feivel.Eval.Util
import Carl.Bool
import Carl.String
import Carl.List


instance Glyph BoolExpr where
  toGlyph (BoolExpr (BoolConst True  :@ _)) = return "#t"
  toGlyph (BoolExpr (BoolConst False :@ _)) = return "#f"
  toGlyph x = error $ "toGlyph: BoolExpr: " ++ show x


instance (Eval Expr, Eval IntExpr, Eval ListExpr, Eval MatExpr, Eval TupleExpr) => Eval BoolExpr where
  eval (BoolExpr (zappa :@ loc)) = case zappa of
    BoolConst b -> return (BoolExpr $ BoolConst b :@ loc)

    {- :Common -}
    BoolVar        key      -> eKey key loc
    BoolAtIdx      m h k    -> eAtIdx m h k loc
    BoolIfThenElse b t f    -> eIfThenElse b t f
    BoolMacro      vals mac -> eMacro vals mac loc

    BoolAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [BoolExpr] -> Integer -> Either ListErr BoolExpr

    BoolAtSlot t i -> do
      x <- eval t >>= getVal :: EvalM (Tuple Expr)
      n <- eval i >>= getVal :: EvalM Integer
      k <- tryEvalM loc $ project x n
      putVal loc k >>= getVal

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

    BoolLT typ a b -> do
      let checkLT z = do
            x <- eval a >>= getVal
            suchThat $ x `hasSameTypeAs` z
            y <- eval b >>= getVal
            putVal loc (x < y) >>= getVal
      case typ of
        ZZ -> checkLT zeroZZ
        SS -> checkLT (Text "")
        QQ -> checkLT zeroQQ
        _ -> reportErr loc $ SortableExpected typ

    BoolLEq typ a b -> do
      let checkLEq z = do
            x <- eval a >>= getVal
            suchThat $ x `hasSameTypeAs` z
            y <- eval b >>= getVal
            putVal loc (x <= y) >>= getVal
      case typ of
        ZZ -> checkLEq zeroZZ
        SS -> checkLEq (Text "")
        QQ -> checkLEq zeroQQ
        _ -> reportErr loc $ SortableExpected typ

    BoolGT typ a b -> do
      let checkGT z = do
            x <- eval a >>= getVal
            suchThat $ x `hasSameTypeAs` z
            y <- eval b >>= getVal
            putVal loc (x > y) >>= getVal
      case typ of
        ZZ -> checkGT zeroZZ
        SS -> checkGT (Text "")
        QQ -> checkGT zeroQQ
        _ -> reportErr loc $ SortableExpected typ

    BoolGEq typ a b -> do
      let checkGEq z = do
            x <- eval a >>= getVal
            suchThat $ x `hasSameTypeAs` z
            y <- eval b >>= getVal
            putVal loc (x >= y) >>= getVal
      case typ of
        ZZ -> checkGEq zeroZZ
        SS -> checkGEq (Text "")
        QQ -> checkGEq zeroQQ
        _ -> reportErr loc $ SortableExpected typ

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
