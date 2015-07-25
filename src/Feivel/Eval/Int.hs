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

module Feivel.Eval.Int () where

import Feivel.Eval.Util


instance Glyph (IntExpr Expr) where
  toGlyph (IntExpr (IntConst n :@ _)) = return $ show n
  toGlyph x = error $ "toGlyph: IntExpr: " ++ show x


instance (Eval Expr) => Eval (IntExpr Expr) where
  eval (IntExpr (IntConst n :@ loc)) = return (IntExpr (IntConst n :@ loc))

  {- :Common -}
  eval (IntExpr (IntVar key :@ loc))        = eKey key loc
  eval (IntExpr (IntAtIdx m h k :@ loc))    = eAtIdx m h k loc
  eval (IntExpr (IntIfThenElse b t f :@ _)) = eIfThenElse b t f
  eval (IntExpr (IntMacro vals mac :@ loc)) = eMacro vals mac loc

  eval (IntExpr (IntAtPos a t :@ loc)) = lift2 loc a t (foo)
    where foo = listAtPos :: [IntExpr Expr] -> Integer -> Either ListErr (IntExpr Expr)

  eval (IntExpr (IntNeg        a :@ loc)) = lift1 loc a (rNegT        zeroZZ)
  eval (IntExpr (IntAbs        a :@ loc)) = lift1 loc a (rAbsT        zeroZZ)
  eval (IntExpr (IntSqPart     a :@ loc)) = lift1 loc a (rSqPartT     zeroZZ)
  eval (IntExpr (IntSqFreePart a :@ loc)) = lift1 loc a (rSqFreePartT zeroZZ)
  eval (IntExpr (IntRad        a :@ loc)) = lift1 loc a (rRadT        zeroZZ)

  eval (IntExpr (IntAdd  a b :@ loc)) = lift2 loc a b (rAddT zeroZZ)
  eval (IntExpr (IntSub  a b :@ loc)) = lift2 loc a b (rSubT zeroZZ)
  eval (IntExpr (IntMult a b :@ loc)) = lift2 loc a b (rMulT zeroZZ)
  eval (IntExpr (IntMod  a b :@ loc)) = lift2 loc a b (rRemT zeroZZ)
  eval (IntExpr (IntMin  a b :@ loc)) = lift2 loc a b (rMinT zeroZZ)
  eval (IntExpr (IntMax  a b :@ loc)) = lift2 loc a b (rMaxT zeroZZ)
  eval (IntExpr (IntGCD  a b :@ loc)) = lift2 loc a b (rGCDT zeroZZ)
  eval (IntExpr (IntLCM  a b :@ loc)) = lift2 loc a b (rLCMT zeroZZ)
  eval (IntExpr (IntQuo  a b :@ loc)) = lift2 loc a b (rQuoT zeroZZ)
  eval (IntExpr (IntPow  a b :@ loc)) = lift2 loc a b (rPowT zeroZZ)

  eval (IntExpr (IntChoose a b :@ loc)) = lift2 loc a b (rChooseT zeroZZ)

  eval (IntExpr (RatNumer  p :@ loc)) = lift1 loc p (ratNum)
  eval (IntExpr (RatDenom  p :@ loc)) = lift1 loc p (ratDen)
  eval (IntExpr (RatFloor  p :@ loc)) = lift1 loc p (ratFlr)
  eval (IntExpr (StrLength s :@ loc)) = lift1 loc s (strLen)

  eval (IntExpr (MatNumRows m :@ loc)) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    k <- tryEvalM loc $ mNumRows n
    return $ IntExpr $ IntConst k :@ loc

  eval (IntExpr (MatNumCols m :@ loc)) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    k <- tryEvalM loc $ mNumCols n
    return $ IntExpr $ IntConst k :@ loc

  eval (IntExpr (IntRand ls :@ loc)) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs
    return $ IntExpr $ IntConst r :@ loc

  eval (IntExpr (ListLen ls :@ loc)) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    return $ IntExpr $ IntConst (fromIntegral $ length xs) :@ loc

  eval (IntExpr (IntSum   ls :@ loc)) = lift1 loc ls (rSumT   zeroZZ)
  eval (IntExpr (IntProd  ls :@ loc)) = lift1 loc ls (rUProdT zeroZZ)
  eval (IntExpr (IntMaxim ls :@ loc)) = lift1 loc ls (rMaximT zeroZZ)
  eval (IntExpr (IntMinim ls :@ loc)) = lift1 loc ls (rMinimT zeroZZ)
  eval (IntExpr (IntGCDiv ls :@ loc)) = lift1 loc ls (rGCDsT  zeroZZ)
  eval (IntExpr (IntLCMul ls :@ loc)) = lift1 loc ls (rLCMsT  zeroZZ)

  eval (IntExpr (IntObserveUniform a b :@ loc)) = do
    x  <- eval a >>= getVal
    y  <- eval b >>= getVal
    t <- observeIntegerUniform loc (x,y)
    return $ IntExpr $ IntConst t :@ loc

  eval (IntExpr (IntObserveBinomial n p :@ loc)) = do
    m <- eval n >>= getVal
    q <- eval p >>= getVal
    t <- observeBinomial loc m (toDouble q)
    return $ IntExpr $ IntConst t :@ loc

  eval (IntExpr (IntObservePoisson lambda :@ loc)) = do
    q <- eval lambda >>= getVal
    t <- observeIntegerPoisson loc (toDouble q)
    return $ IntExpr $ IntConst t :@ loc

  eval (IntExpr (IntCastStr str :@ loc)) = do
    Text x <- eval str >>= getVal
    n <- parseAsAt pInteger loc x
    return $ IntExpr $ IntConst n :@ loc

  eval (IntExpr (MatRank m :@ loc)) = do
    case typeOf m of
      MatOf QQ -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        r <- tryEvalM loc $ mRank n
        return $ IntExpr $ IntConst r :@ loc
      MatOf BB -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        r <- tryEvalM loc $ mRank n
        return $ IntExpr $ IntConst r :@ loc
      MatOf u -> reportErr loc $ FieldMatrixExpected u
      u -> reportErr loc $ MatrixExpected u

  eval (IntExpr (IntContent p :@ loc)) = do
    q <- eval p >>= getVal :: EvalM (Poly Integer)
    c <- tryEvalM loc $ contentP q
    return $ IntExpr $ IntConst c :@ loc


