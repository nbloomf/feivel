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

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Feivel.Eval.Rat () where

import Feivel.Eval.Util


instance Glyph (RatExpr Expr) where
  toGlyph (RatExpr (RatConst x :@ _)) = return $ show x
  toGlyph x = error $ "toGlyph: RatExpr: " ++ show x


instance (Eval Expr) => Eval (RatExpr Expr) where
  eval (RatExpr (RatConst p :@ loc)) = return $ RatExpr $ RatConst p :@ loc

  {- :Common -}
  eval (RatExpr (RatVar key :@ loc))        = eKey key loc
  eval (RatExpr (RatAtIdx m h k :@ loc))    = eAtIdx m h k loc
  eval (RatExpr (RatIfThenElse b t f :@ _)) = eIfThenElse b t f
  eval (RatExpr (RatMacro vals mac :@ loc)) = eMacro vals mac loc

  eval (RatExpr (RatAtPos a t :@ loc)) = lift2 loc a t (foo)
    where foo = listAtPos :: [RatExpr Expr] -> Integer -> Either ListErr (RatExpr Expr)

  eval (RatExpr (RatCast expr :@ loc)) = do
    n <- eval expr >>= getVal :: EvalM Integer
    putVal loc (n:/:1) >>= getVal

  eval (RatExpr (RatNeg  a :@ loc))   = lift1 loc a (rNegT zeroQQ)
  eval (RatExpr (RatAbs  a :@ loc))   = lift1 loc a (rAbsT zeroQQ)

  eval (RatExpr (RatAdd  a b :@ loc)) = lift2 loc a b (rAddT zeroQQ)
  eval (RatExpr (RatSub  a b :@ loc)) = lift2 loc a b (rSubT zeroQQ)
  eval (RatExpr (RatMult a b :@ loc)) = lift2 loc a b (rMulT zeroQQ)
  eval (RatExpr (RatMin  a b :@ loc)) = lift2 loc a b (rMinT zeroQQ)
  eval (RatExpr (RatMax  a b :@ loc)) = lift2 loc a b (rMaxT zeroQQ)
  eval (RatExpr (RatPow  a b :@ loc)) = lift2 loc a b (rPowT zeroQQ)
  eval (RatExpr (RatQuot a b :@ loc)) = lift2 loc a b (rDivT zeroQQ)

  eval (RatExpr (RatSqrt p k :@ loc)) = lift2 loc p k (ratSqt)

  eval (RatExpr (RatRand ls :@ loc)) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs :: EvalM Rat
    putVal loc r >>= getVal

  eval (RatExpr (RatSum   ls :@ loc)) = lift1 loc ls (rSumT   (0:/:1))
  eval (RatExpr (RatProd  ls :@ loc)) = lift1 loc ls (rUProdT (0:/:1))
  eval (RatExpr (RatMaxim ls :@ loc)) = lift1 loc ls (rMaximT (0:/:1))
  eval (RatExpr (RatMinim ls :@ loc)) = lift1 loc ls (rMinimT (0:/:1))

  {- Mean -}
  eval (RatExpr (RatMean ls :@ loc)) = do
    case typeOf ls of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ rIntMeanT (0:/:1) xs
        putVal loc m >>= getVal
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ rMeanT (0:/:1) xs
        putVal loc m >>= getVal
      u -> reportErr loc $ NumericListExpected u

  {- Mean Deviation -}
  eval (RatExpr (RatMeanDev ls :@ loc)) = do
    case typeOf ls of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ rIntMeanDevT (0:/:1) xs
        putVal loc m >>= getVal
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ rMeanDevT (0:/:1) xs
        putVal loc m >>= getVal
      u -> reportErr loc $ NumericListExpected u

  {- Standard Deviation -}
  eval (RatExpr (RatStdDev ls d :@ loc)) = do
    k <- eval d >>= getVal
    case typeOf ls of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ ratIntStdDev xs k
        putVal loc m >>= getVal
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ ratStdDev xs k
        putVal loc m >>= getVal
      u -> reportErr loc $ NumericListExpected u

  {- Z-Score -}
  eval (RatExpr (RatZScore x ls d :@ loc)) = do
    k <- eval d >>= getVal
    y <- eval x >>= getVal
    case typeOf ls of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ ratIntZScore y xs k
        putVal loc m >>= getVal
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ ratZScore y xs k
        putVal loc m >>= getVal
      u -> reportErr loc $ NumericListExpected u

  eval (RatExpr (RatCastStr str :@ loc)) = do
    Text x <- eval str >>= getVal
    n <- parseAsAt pRat loc x
    putVal loc n >>= getVal
