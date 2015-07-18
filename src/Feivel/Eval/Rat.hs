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

module Feivel.Eval.Rat where

import Feivel.EvalM
import Feivel.Expr
import Feivel.Lib
import Feivel.Put
import Feivel.Get
import Feivel.Eval.Eval
import Feivel.Eval.Util
import Feivel.Locus
import Feivel.Parse (pRat)
import Feivel.Error
import Feivel.Type
import Feivel.Typed

{-----------------}
{- :Eval:RatExpr -}
{-----------------}

instance (Eval Expr) => Eval (RatExpr Expr) where
  eval (RatConst p :@ loc) = return $ RatConst p :@ loc

  {- :Common -}
  eval (RatVar key :@ loc)        = eKey key loc
  eval (RatAtIdx m h k :@ loc)    = eAtIdx m h k loc
  eval (RatIfThenElse b t f :@ _) = eIfThenElse b t f
  eval (RatMacro vals mac :@ loc) = eMacro vals mac loc

  eval (RatAtPos a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [RatExpr Expr] -> Integer -> Either ListErr (RatExpr Expr)

  eval (RatCast expr :@ loc) = do
    n <- eval expr >>= getVal :: EvalM Integer
    return $ RatConst (n:/:1) :@ loc

  eval (RatNeg  a :@ loc)   = lift1 loc a (rNegT zeroQQ)
  eval (RatAbs  a :@ loc)   = lift1 loc a (rAbsT zeroQQ)

  eval (RatAdd  a b :@ loc) = lift2 loc a b (rAddT zeroQQ)
  eval (RatSub  a b :@ loc) = lift2 loc a b (rSubT zeroQQ)
  eval (RatMult a b :@ loc) = lift2 loc a b (rMulT zeroQQ)
  eval (RatMin  a b :@ loc) = lift2 loc a b (rMinT zeroQQ)
  eval (RatMax  a b :@ loc) = lift2 loc a b (rMaxT zeroQQ)
  eval (RatPow  a b :@ loc) = lift2 loc a b (rPowT zeroQQ)
  eval (RatQuot a b :@ loc) = lift2 loc a b (rDivT zeroQQ)

  eval (RatSqrt p k :@ loc) = lift2 loc p k (ratSqt)

  eval (RatRand ls :@ loc) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs
    return $ RatConst r :@ loc

  eval (RatSum   ls :@ loc) = lift1 loc ls (rSumT   (0:/:1))
  eval (RatProd  ls :@ loc) = lift1 loc ls (rUProdT (0:/:1))
  eval (RatMaxim ls :@ loc) = lift1 loc ls (rMaximT (0:/:1))
  eval (RatMinim ls :@ loc) = lift1 loc ls (rMinimT (0:/:1))

  {- Mean -}
  eval (RatMean ls :@ loc) = do
    case typeOf ls of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ rIntMeanT (0:/:1) xs
        return $ RatConst m :@ loc
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ rMeanT (0:/:1) xs
        return $ RatConst m :@ loc
      u -> reportErr loc $ NumericListExpected u

  {- Mean Deviation -}
  eval (RatMeanDev ls :@ loc) = do
    case typeOf ls of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ rIntMeanDevT (0:/:1) xs
        return $ RatConst m :@ loc
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ rMeanDevT (0:/:1) xs
        return $ RatConst m :@ loc
      u -> reportErr loc $ NumericListExpected u

  {- Standard Deviation -}
  eval (RatStdDev ls d :@ loc) = do
    k <- eval d >>= getVal
    case typeOf ls of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ ratIntStdDev xs k
        return $ RatConst m :@ loc
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ ratStdDev xs k
        return $ RatConst m :@ loc
      u -> reportErr loc $ NumericListExpected u

  {- Z-Score -}
  eval (RatZScore x ls d :@ loc) = do
    k <- eval d >>= getVal
    y <- eval x >>= getVal
    case typeOf ls of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ ratIntZScore y xs k
        return $ RatConst m :@ loc
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ ratZScore y xs k
        return $ RatConst m :@ loc
      u -> reportErr loc $ NumericListExpected u

  eval (RatCastStr str :@ loc) = do
    Text x <- eval str >>= getVal
    n <- parseAsAt pRat loc x
    return $ RatConst n :@ loc
