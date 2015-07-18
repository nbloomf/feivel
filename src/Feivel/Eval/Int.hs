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

module Feivel.Eval.Int where

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

{-----------------}
{- :Eval:IntExpr -}
{-----------------}

instance Glyph (IntExpr Expr) where
  toGlyph (IntConst n :@ _) = return $ show n
  toGlyph x = error $ "toGlyph: IntExpr: " ++ show x

instance (Eval Expr) => Eval (IntExpr Expr) where
  eval (IntConst n :@ loc) = return (IntConst n :@ loc)

  {- :Common -}
  eval (IntVar key :@ loc)        = eKey key loc
  eval (IntAtIdx m h k :@ loc)    = eAtIdx m h k loc
  eval (IntIfThenElse b t f :@ _) = eIfThenElse b t f
  eval (IntMacro vals mac :@ loc) = eMacro vals mac loc

  eval (IntAtPos a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [IntExpr Expr] -> Integer -> Either ListErr (IntExpr Expr)

  eval (IntNeg        a :@ loc) = lift1 loc a (rNegT        zeroZZ)
  eval (IntAbs        a :@ loc) = lift1 loc a (rAbsT        zeroZZ)
  eval (IntSqPart     a :@ loc) = lift1 loc a (rSqPartT     zeroZZ)
  eval (IntSqFreePart a :@ loc) = lift1 loc a (rSqFreePartT zeroZZ)
  eval (IntRad        a :@ loc) = lift1 loc a (rRadT        zeroZZ)

  eval (IntAdd  a b :@ loc) = lift2 loc a b (rAddT zeroZZ)
  eval (IntSub  a b :@ loc) = lift2 loc a b (rSubT zeroZZ)
  eval (IntMult a b :@ loc) = lift2 loc a b (rMulT zeroZZ)
  eval (IntMod  a b :@ loc) = lift2 loc a b (rRemT zeroZZ)
  eval (IntMin  a b :@ loc) = lift2 loc a b (rMinT zeroZZ)
  eval (IntMax  a b :@ loc) = lift2 loc a b (rMaxT zeroZZ)
  eval (IntGCD  a b :@ loc) = lift2 loc a b (rGCDT zeroZZ)
  eval (IntLCM  a b :@ loc) = lift2 loc a b (rLCMT zeroZZ)
  eval (IntQuo  a b :@ loc) = lift2 loc a b (rQuoT zeroZZ)
  eval (IntPow  a b :@ loc) = lift2 loc a b (rPowT zeroZZ)

  eval (IntChoose a b :@ loc) = lift2 loc a b (rChooseT zeroZZ)

  eval (RatNumer  p :@ loc) = lift1 loc p (ratNum)
  eval (RatDenom  p :@ loc) = lift1 loc p (ratDen)
  eval (RatFloor  p :@ loc) = lift1 loc p (ratFlr)
  eval (StrLength s :@ loc) = lift1 loc s (strLen)

  eval (MatNumRows m :@ loc) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    k <- tryEvalM loc $ mNumRows n
    return $ IntConst k :@ loc

  eval (MatNumCols m :@ loc) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    k <- tryEvalM loc $ mNumCols n
    return $ IntConst k :@ loc

  eval (IntRand ls :@ loc) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs
    return $ IntConst r :@ loc

  eval (ListLen ls :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    return $ IntConst (fromIntegral $ length xs) :@ loc

  eval (IntSum   ls :@ loc) = lift1 loc ls (rSumT   zeroZZ)
  eval (IntProd  ls :@ loc) = lift1 loc ls (rUProdT zeroZZ)
  eval (IntMaxim ls :@ loc) = lift1 loc ls (rMaximT zeroZZ)
  eval (IntMinim ls :@ loc) = lift1 loc ls (rMinimT zeroZZ)
  eval (IntGCDiv ls :@ loc) = lift1 loc ls (rGCDsT  zeroZZ)
  eval (IntLCMul ls :@ loc) = lift1 loc ls (rLCMsT  zeroZZ)

  eval (IntObserveUniform a b :@ loc) = do
    x  <- eval a >>= getVal
    y  <- eval b >>= getVal
    t <- observeIntegerUniform loc (x,y)
    return $ IntConst t :@ loc

  eval (IntObserveBinomial n p :@ loc) = do
    m <- eval n >>= getVal
    q <- eval p >>= getVal
    t <- observeBinomial loc m (toDouble q)
    return $ IntConst t :@ loc

  eval (IntObservePoisson lambda :@ loc) = do
    q <- eval lambda >>= getVal
    t <- observeIntegerPoisson loc (toDouble q)
    return $ IntConst t :@ loc

  eval (IntCastStr str :@ loc) = do
    Text x <- eval str >>= getVal
    n <- parseAsAt pInteger loc x
    return $ IntConst n :@ loc

  eval (MatRank m :@ loc) = do
    case typeOf m of
      MatOf QQ -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        r <- tryEvalM loc $ mRank n
        return $ IntConst r :@ loc
      MatOf BB -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        r <- tryEvalM loc $ mRank n
        return $ IntConst r :@ loc
      MatOf u -> reportErr loc $ FieldMatrixExpected u
      u -> reportErr loc $ MatrixExpected u

  eval (IntContent p :@ loc) = do
    q <- eval p >>= getVal :: EvalM (Poly Integer)
    c <- tryEvalM loc $ contentP q
    return $ IntConst c :@ loc


