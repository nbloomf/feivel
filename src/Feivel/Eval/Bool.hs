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

{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}

module Feivel.Eval.Bool () where

import Feivel.Eval.Util


instance Glyph BoolExpr where
  toGlyph (BoolExpr (BoolConst True  :@ _)) = return "#t"
  toGlyph (BoolExpr (BoolConst False :@ _)) = return "#f"
  toGlyph x = error $ "toGlyph: BoolExpr: " ++ show x


instance (Eval Expr) => Eval BoolExpr where
  eval (BoolExpr (BoolConst b :@ loc)) = return (BoolExpr $ BoolConst b :@ loc)

  {- :Common -}
  eval (BoolExpr (BoolVar key :@ loc))        = eKey key loc
  eval (BoolExpr (BoolAtIdx m h k :@ loc))    = eAtIdx m h k loc
  eval (BoolExpr (BoolIfThenElse b t f :@ _)) = eIfThenElse b t f
  eval (BoolExpr (BoolMacro vals mac :@ loc)) = eMacro vals mac loc

  eval (BoolExpr (BoolAtPos a t :@ loc)) = lift2 loc a t (foo)
    where foo = listAtPos :: [BoolExpr] -> Integer -> Either ListErr BoolExpr

  eval (BoolExpr (IsDefined key :@ loc)) = do
    p <- isKeyDefined key
    putVal loc p >>= getVal

  eval (BoolExpr (BoolEq a b :@ loc)) = do
    x <- eval a >>= getVal :: EvalM Expr
    y <- eval b >>= getVal :: EvalM Expr
    putVal loc (x == y) >>= getVal

  eval (BoolExpr (BoolNEq a b :@ loc)) = do
    x <- eval a >>= getVal :: EvalM Expr
    y <- eval b >>= getVal :: EvalM Expr
    putVal loc (x /= y) >>= getVal

  eval (BoolExpr (BoolLT a b :@ loc)) = do
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

  eval (BoolExpr (BoolLEq a b :@ loc)) = do
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

  eval (BoolExpr (BoolGT a b :@ loc)) = do
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

  eval (BoolExpr (BoolGEq a b :@ loc)) = do
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

  eval (BoolExpr (BoolRand ls :@ loc)) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs :: EvalM Bool
    putVal loc r >>= getVal

  eval (BoolExpr (ListElem x xs :@ loc)) = do
    a <- eval x >>= getVal :: EvalM Expr
    as <- eval xs >>= getVal :: EvalM [Expr]
    putVal loc (elem a as) >>= getVal

  eval (BoolExpr (ListIsEmpty xs :@ loc)) = do
    as <- eval xs >>= getVal :: EvalM [Expr]
    putVal loc (null as) >>= getVal

  eval (BoolExpr (MatIsRow m :@ loc)) = do
    p <- eval m >>= getVal :: EvalM (Matrix Expr)
    q <- tryEvalM loc $ mIsRow p
    putVal loc q >>= getVal

  eval (BoolExpr (MatIsCol m :@ loc)) = do
    p <- eval m >>= getVal :: EvalM (Matrix Expr)
    q <- tryEvalM loc $ mIsCol p
    putVal loc q >>= getVal

  eval (BoolExpr (MatIsGJForm m :@ loc)) = do
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
  eval (BoolExpr (Neg    a   :@ loc)) = lift1 loc a   (boolNot)
  eval (BoolExpr (Conj   a b :@ loc)) = lift2 loc a b (boolAnd)
  eval (BoolExpr (Disj   a b :@ loc)) = lift2 loc a b (boolOr)
  eval (BoolExpr (Imp    a b :@ loc)) = lift2 loc a b (boolImp)

  -- Int
  eval (BoolExpr (IntSqFree a :@ loc)) = lift1 loc a   (rIsSqFreeT zeroZZ)
  eval (BoolExpr (IntDiv a b :@ loc))  = lift2 loc a b (rDividesT  zeroZZ)

  -- Str
  eval (BoolExpr (Matches a b :@ loc)) = lift2 loc a b (strMatch)
