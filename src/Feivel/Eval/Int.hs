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

module Feivel.Eval.Int () where

import Feivel.Eval.Util
import Carl.List


instance Glyph IntExpr where
  toGlyph (IntExpr (IntConst n :@ _)) = return $ show n
  toGlyph x = error $ "toGlyph: IntExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval ListExpr, Eval MatExpr, Eval TupleExpr, Eval RatExpr, Eval StrExpr, Eval PolyExpr) => Eval IntExpr where
  eval (IntExpr (zappa :@ loc)) = case zappa of
    IntConst n -> return (IntExpr (IntConst n :@ loc))

    {- :Common -}
    IntVar        key      -> eKey key loc
    IntAtIdx      m h k    -> eAtIdx m h k loc
    IntIfThenElse b t f    -> eIfThenElse b t f
    IntMacro      vals mac -> eMacro vals mac loc

    IntAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [IntExpr] -> Integer -> Either ListErr IntExpr

    IntAtSlot t i -> do
      x <- eval t >>= getVal :: EvalM (Tuple Expr)
      n <- eval i >>= getVal :: EvalM Integer
      k <- tryEvalM loc $ project x n
      putVal loc k >>= getVal

    IntNeg        a -> lift1 loc a (rNegT        zeroZZ)
    IntAbs        a -> lift1 loc a (rAbsT        zeroZZ)
    IntSqPart     a -> lift1 loc a (rSqPartT     zeroZZ)
    IntSqFreePart a -> lift1 loc a (rSqFreePartT zeroZZ)
    IntRad        a -> lift1 loc a (rRadT        zeroZZ)

    IntAdd  a b -> lift2 loc a b (rAddT zeroZZ)
    IntSub  a b -> lift2 loc a b (rSubT zeroZZ)
    IntMult a b -> lift2 loc a b (rMulT zeroZZ)
    IntMod  a b -> lift2 loc a b (rRemT zeroZZ)
    IntMin  a b -> lift2 loc a b (rMinT zeroZZ)
    IntMax  a b -> lift2 loc a b (rMaxT zeroZZ)
    IntGCD  a b -> lift2 loc a b (rGCDT zeroZZ)
    IntLCM  a b -> lift2 loc a b (rLCMT zeroZZ)
    IntQuo  a b -> lift2 loc a b (rQuoT zeroZZ)
    IntPow  a b -> lift2 loc a b (rPowT zeroZZ)

    IntChoose a b -> lift2 loc a b (rChooseT zeroZZ)

    RatNumer  p -> lift1 loc p (ratNum)
    RatDenom  p -> lift1 loc p (ratDen)
    RatFloor  p -> lift1 loc p (ratFlr)
    StrLength s -> lift1 loc s (strLen)

    MatNumRows m -> do
      n <- eval m >>= getVal :: EvalM (Matrix Expr)
      k <- tryEvalM loc $ mNumRows n
      putVal loc k >>= getVal

    MatNumCols m -> do
      n <- eval m >>= getVal :: EvalM (Matrix Expr)
      k <- tryEvalM loc $ mNumCols n
      putVal loc k >>= getVal

    IntRand ls -> do
      xs <- eval ls >>= getVal
      r  <- randomElementEvalM xs :: EvalM Integer
      putVal loc r >>= getVal

    ListLen ls -> do
      xs <- eval ls >>= getVal :: EvalM [Expr]
      let k = fromIntegral $ length xs :: Integer
      putVal loc k >>= getVal

    IntSum   ls -> lift1 loc ls (rSumT   zeroZZ)
    IntProd  ls -> lift1 loc ls (rUProdT zeroZZ)
    IntMaxim ls -> lift1 loc ls (rMaximT zeroZZ)
    IntMinim ls -> lift1 loc ls (rMinimT zeroZZ)
    IntGCDiv ls -> lift1 loc ls (rGCDsT  zeroZZ)
    IntLCMul ls -> lift1 loc ls (rLCMsT  zeroZZ)

    IntObserveUniform a b -> do
      x <- eval a >>= getVal
      y <- eval b >>= getVal
      t <- observeIntegerUniform loc (x,y)
      putVal loc t >>= getVal

    IntObserveBinomial n p -> do
      m <- eval n >>= getVal
      q <- eval p >>= getVal
      t <- observeBinomial loc m (toDouble q)
      putVal loc t >>= getVal

    IntObservePoisson lambda -> do
      q <- eval lambda >>= getVal
      t <- observeIntegerPoisson loc (toDouble q)
      putVal loc t >>= getVal

    IntCastStr str -> do
      Text x <- eval str >>= getVal
      n <- parseAsAt pInteger loc x
      putVal loc n >>= getVal

    MatRank m -> do
      case typeOf m of
        MatOf QQ -> do
          n <- eval m >>= getVal :: EvalM (Matrix Rat)
          r <- tryEvalM loc $ mRank n
          putVal loc r >>= getVal
        MatOf BB -> do
          n <- eval m >>= getVal :: EvalM (Matrix Bool)
          r <- tryEvalM loc $ mRank n
          putVal loc r >>= getVal
        MatOf u -> reportErr loc $ FieldMatrixExpected u
        u -> reportErr loc $ MatrixExpected u

    PolyDegree u p -> do
      let polyDeg z = do
            q <- eval p >>= getVal
            suchThat $ q `hasSameTypeAs` (constPoly z)
            Nat n <- tryEvalM loc $ leadingDegreeBy mGLex q
            putVal loc n >>= getVal
      case u of
        ZZ      -> polyDeg zeroZZ
        QQ      -> polyDeg zeroQQ
        BB      -> polyDeg zeroBB
        ZZMod n -> polyDeg (zeroMod n)
        _ -> reportErr loc $ NumericTypeExpected u

    IntContent p -> do
      q <- eval p >>= getVal :: EvalM (Poly Integer)
      c <- tryEvalM loc $ contentPoly q
      putVal loc c >>= getVal
