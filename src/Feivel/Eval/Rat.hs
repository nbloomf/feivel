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

module Feivel.Eval.Rat () where

import Feivel.Eval.Util
import Carl.List
import Carl.String


instance Glyph RatExpr where
  toGlyph (RatExpr (RatConst x :@ _)) = return $ show x
  toGlyph x = error $ "toGlyph: RatExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr, Eval ListExpr, Eval MatExpr) => Eval RatExpr where
  eval (RatExpr (zappa :@ loc)) = case zappa of
    RatConst p -> return $ RatExpr $ RatConst p :@ loc

    {- :Common -}
    RatVar key -> eKey key loc
    RatAtIdx m h k -> eAtIdx m h k loc
    RatIfThenElse b t f -> eIfThenElse b t f
    RatMacro vals mac -> eMacro vals mac loc

    RatAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [RatExpr] -> Integer -> Either ListErr RatExpr

    RatCast expr -> do
      n <- eval expr >>= getVal :: EvalM Integer
      putVal loc (n:/:1) >>= getVal

    RatNeg  a -> lift1 loc a (rNegT zeroQQ)
    RatAbs  a -> lift1 loc a (rAbsT zeroQQ)

    RatAdd  a b -> lift2 loc a b (rAddT zeroQQ)
    RatSub  a b -> lift2 loc a b (rSubT zeroQQ)
    RatMult a b -> lift2 loc a b (rMulT zeroQQ)
    RatMin  a b -> lift2 loc a b (rMinT zeroQQ)
    RatMax  a b -> lift2 loc a b (rMaxT zeroQQ)
    RatPow  a b -> lift2 loc a b (rPowT zeroQQ)
    RatQuot a b -> lift2 loc a b (rDivT zeroQQ)

    RatSqrt p k -> lift2 loc p k (ratSqt)

    RatRand ls -> do
      xs <- eval ls >>= getVal
      r  <- randomElementEvalM xs :: EvalM Rat
      putVal loc r >>= getVal

    RatSum   ls -> lift1 loc ls (rSumT   (0:/:1))
    RatProd  ls -> lift1 loc ls (rUProdT (0:/:1))
    RatMaxim ls -> lift1 loc ls (rMaximT (0:/:1))
    RatMinim ls -> lift1 loc ls (rMinimT (0:/:1))

    {- Mean -}
    RatMean ls -> do
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
    RatMeanDev ls -> do
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
    RatStdDev ls d -> do
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
    RatZScore x ls d -> do
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

    RatCastStr str -> do
      Text x <- eval str >>= getVal
      n <- parseAsAt pRat loc x
      putVal loc n >>= getVal
