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

module Feivel.Eval.Mat () where

import Feivel.Eval.Util
import Carl.List


{-
-- This works with RankNTypes or Rank2Types enabled.
-- But is it safe? What is the downside?

dispatchMatrixRingType
  :: Locus
     -> Type
     -> (forall a.
          (Ringoid a, CRingoid a, URingoid a, Typed a, Put a)
            => a -> EvalM MatExpr)
     -> EvalM MatExpr
dispatchMatrixRingType loc u fun = case u of
  ZZ          -> fun zeroZZ
  QQ          -> fun zeroQQ
  BB          -> fun zeroBB
  ZZMod n     -> fun (zeroMod n)
  PolyOver ZZ -> fun (constPoly zeroZZ)
  PolyOver QQ -> fun (constPoly zeroQQ)
  PolyOver BB -> fun (constPoly zeroBB)
  _           -> reportErr loc $ NumericTypeExpected u
-}



instance (Glyph Expr) => Glyph MatExpr where
  toGlyph (MatExpr (MatConst m :# _ :@ _)) = do
    n <- mSeq $ fmap toGlyph m
    case mShowStr n of
      Left err -> reportErr (error "Glyph instance of MatExpr") err
      Right x  -> return x
  toGlyph x = error $ "toGlyph: MatExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr, Eval ListExpr, Eval TupleExpr) => Eval MatExpr where
  eval (MatExpr (zappa :# typ :@ loc)) = case zappa of
    MatConst m -> do
      n <- mSeq $ fmap eval m
      return $ MatExpr $ MatConst n :# typ :@ loc

    {- :Common -}
    MatVar key -> eKey key loc
    MatAtIdx m h k -> eAtIdx m h k loc
    MatMacro vals mac -> eMacro vals mac loc
    MatIfThenElse b t f -> eIfThenElse b t f

    MatAtSlot t i -> do
      x <- eval t >>= getVal :: EvalM (Tuple Expr)
      n <- eval i >>= getVal :: EvalM Integer
      k <- tryEvalM loc $ project x n
      putVal loc k >>= getVal

    MatAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [MatExpr] -> Integer -> Either ListErr MatExpr

    MatRand ls -> do
      xs <- eval ls >>= getVal :: EvalM [Expr]
      r  <- randomElementEvalM xs
      eval r >>= getVal

    MatRowFromList xs -> do
      as <- eval xs >>= getVal :: EvalM [Expr]
      m  <- tryEvalM loc $ mRowFromList as
      putTypeVal typ loc m >>= getVal

    MatColFromList xs -> do
      as <- eval xs >>= getVal :: EvalM [Expr]
      m  <- tryEvalM loc $ mColFromList as
      putTypeVal typ loc m >>= getVal

    MatId n -> do
      let makeIdMat x = lift1 loc n (mEIdT x)
      case typ of
        ZZ          -> makeIdMat zeroZZ
        QQ          -> makeIdMat zeroQQ
        BB          -> makeIdMat zeroBB
        ZZMod d     -> makeIdMat (zeroMod d)
        PolyOver ZZ -> makeIdMat (constPoly zeroZZ)
        PolyOver QQ -> makeIdMat (constPoly zeroQQ)
        _           -> reportErr loc $ NumericTypeExpected typ

    MatSwapE n h k -> do
      let makeSwapMat x = lift3 loc n h k (mESwapT x)
      case typ of
        ZZ          -> makeSwapMat zeroZZ
        QQ          -> makeSwapMat zeroQQ
        BB          -> makeSwapMat zeroBB
        ZZMod d     -> makeSwapMat (zeroMod d)
        PolyOver ZZ -> makeSwapMat (constPoly zeroZZ)
        PolyOver QQ -> makeSwapMat (constPoly zeroQQ)
        _           -> reportErr loc $ NumericTypeExpected typ

    MatScaleE n h r -> do
      let makeScaleMat x = lift3 loc n h r (mEScaleT x)
      case typ of
        ZZ          -> makeScaleMat zeroZZ
        QQ          -> makeScaleMat zeroQQ
        BB          -> makeScaleMat zeroBB
        ZZMod d     -> makeScaleMat (zeroMod d)
        PolyOver ZZ -> makeScaleMat (constPoly zeroZZ)
        PolyOver QQ -> makeScaleMat (constPoly zeroQQ)
        PolyOver BB -> makeScaleMat (constPoly zeroBB)
        _           -> reportErr loc $ NumericTypeExpected typ

    MatAddE n h k r -> do
      let makeAddMat x = lift4 loc n h k r (mEAddT x)
      case typ of
        ZZ          -> makeAddMat zeroZZ
        QQ          -> makeAddMat zeroQQ
        BB          -> makeAddMat zeroBB
        ZZMod d     -> makeAddMat (zeroMod d)
        PolyOver ZZ -> makeAddMat (constPoly zeroZZ)
        PolyOver QQ -> makeAddMat (constPoly zeroQQ)
        PolyOver BB -> makeAddMat (constPoly zeroBB)
        _           -> reportErr loc $ NumericTypeExpected typ

    MatShuffleRows m -> do
      x  <- eval m >>= getVal :: EvalM (Matrix Expr)
      rs <- tryEvalM loc $ mRowsOf x
      ts <- shuffleEvalM rs
      n  <- tryEvalM loc $ mVCats ts
      putTypeVal typ loc n >>= getVal

    MatShuffleCols m -> do
      x  <- eval m >>= getVal :: EvalM (Matrix Expr)
      rs <- tryEvalM loc $ mColsOf x
      ts <- shuffleEvalM rs
      n  <- tryEvalM loc $ mHCats ts
      putTypeVal typ loc n >>= getVal

    MatHCat a b -> do
      m <- eval a >>= getVal :: EvalM (Matrix Expr)
      n <- eval b >>= getVal :: EvalM (Matrix Expr)
      x <- tryEvalM loc $ mHCat m n
      putTypeVal typ loc x >>= getVal

    MatVCat a b -> do
      m <- eval a >>= getVal :: EvalM (Matrix Expr)
      n <- eval b >>= getVal :: EvalM (Matrix Expr)
      x <- tryEvalM loc $ mVCat m n
      putTypeVal typ loc x >>= getVal

    MatAdd a b -> do
      let addMat x = lift2 loc a b (rAddT (mCell x))
      case typ of
        ZZ          -> addMat zeroZZ
        QQ          -> addMat zeroQQ
        BB          -> addMat zeroBB
        ZZMod n     -> addMat (zeroMod n)
        PolyOver ZZ -> addMat (constPoly zeroZZ)
        PolyOver QQ -> addMat (constPoly zeroQQ)
        PolyOver BB -> addMat (constPoly zeroBB)
        _           -> reportErr loc $ NumericTypeExpected typ

    MatNeg a -> do
      let negMat x = lift1 loc a (rNegT (mCell x))
      case typ of
        ZZ          -> negMat zeroZZ
        QQ          -> negMat zeroQQ
        BB          -> negMat zeroBB
        ZZMod n     -> negMat (zeroMod n)
        PolyOver ZZ -> negMat (constPoly zeroZZ)
        PolyOver QQ -> negMat (constPoly zeroQQ)
        PolyOver BB -> negMat (constPoly zeroBB)
        _           -> reportErr loc $ NumericTypeExpected typ

    MatTrans a -> do
      m <- eval a >>= getVal :: EvalM (Matrix Expr)
      p <- tryEvalM loc $ mTranspose m
      putTypeVal typ loc p >>= getVal

    MatMul a b -> do
      let mulMat x = lift2 loc a b (rMulT (mCell x))
      case typ of
        ZZ          -> mulMat zeroZZ
        QQ          -> mulMat zeroQQ
        BB          -> mulMat zeroBB
        (ZZMod n)   -> mulMat (zeroMod n)
        PolyOver ZZ -> mulMat (constPoly zeroZZ)
        PolyOver QQ -> mulMat (constPoly zeroQQ)
        PolyOver BB -> mulMat (constPoly zeroBB)
        _           -> reportErr loc $ NumericTypeExpected typ

    MatPow m n -> do
      let powMat x = lift2 loc m n (rPosPowT (mCell x))
      case typ of
        ZZ          -> powMat zeroZZ
        QQ          -> powMat zeroQQ
        BB          -> powMat zeroBB
        (ZZMod k)   -> powMat (zeroMod k)
        PolyOver ZZ -> powMat (constPoly zeroZZ)
        PolyOver QQ -> powMat (constPoly zeroQQ)
        PolyOver BB -> powMat (constPoly zeroBB)
        _           -> reportErr loc $ NumericTypeExpected typ

    MatSwapRows m a b -> do
      n <- eval m >>= getVal :: EvalM (Matrix Expr)
      i <- eval a >>= getVal
      j <- eval b >>= getVal
      p <- tryEvalM loc $ mSwapRows i j n
      putTypeVal typ loc p >>= getVal

    MatSwapCols m a b -> do
      n <- eval m >>= getVal :: EvalM (Matrix Expr)
      i <- eval a >>= getVal
      j <- eval b >>= getVal
      p <- tryEvalM loc $ mSwapCols i j n
      putTypeVal typ loc p >>= getVal

    MatScaleRow m a h -> do
      let scaleRowMat x = lift3 loc a h m (mScaleRowT x)
      case typ of
        ZZ          -> scaleRowMat zeroZZ
        QQ          -> scaleRowMat zeroQQ
        BB          -> scaleRowMat zeroBB
        (ZZMod n)   -> scaleRowMat (zeroMod n)
        PolyOver ZZ -> scaleRowMat (constPoly zeroZZ)
        PolyOver QQ -> scaleRowMat (constPoly zeroQQ)
        PolyOver BB -> scaleRowMat (constPoly zeroBB)
        _           -> reportErr loc $ NumericTypeExpected typ

    MatScaleCol m a h -> do
      let scaleColMat x = lift3 loc a h m (mScaleColT x)
      case typ of
        ZZ          -> scaleColMat zeroZZ
        QQ          -> scaleColMat zeroQQ
        BB          -> scaleColMat zeroBB
        (ZZMod n)   -> scaleColMat (zeroMod n)
        PolyOver ZZ -> scaleColMat (constPoly zeroZZ)
        PolyOver QQ -> scaleColMat (constPoly zeroQQ)
        PolyOver BB -> scaleColMat (constPoly zeroBB)
        _           -> reportErr loc $ NumericTypeExpected typ

    MatAddRow m a h k -> do
      i <- eval h >>= getVal
      j <- eval k >>= getVal
      let makeAddRow zer = do
            n <- eval m >>= getVal
            r <- eval a >>= getVal
            suchThat $ r `hasSameTypeAs` zer
            p <- tryEvalM loc $ mAddRow r i j n
            putTypeVal typ loc p >>= getVal
      case typ of
        ZZ      -> makeAddRow zeroZZ
        QQ      -> makeAddRow zeroQQ
        BB      -> makeAddRow zeroBB
        ZZMod n -> makeAddRow (zeroMod n)
        _ -> reportErr loc $ NumericMatrixExpected typ

    MatAddCol m a h k -> do
      i <- eval h >>= getVal
      j <- eval k >>= getVal
      let makeAddCol zer = do
            n <- eval m >>= getVal
            r <- eval a >>= getVal
            suchThat $ r `hasSameTypeAs` zer
            p <- tryEvalM loc $ mAddCol r i j n
            putTypeVal typ loc p >>= getVal
      case typ of
        ZZ      -> makeAddCol zeroZZ
        QQ      -> makeAddCol zeroQQ
        BB      -> makeAddCol zeroBB
        ZZMod n -> makeAddCol (zeroMod n)
        _ -> reportErr loc $ NumericMatrixExpected typ

    MatDelRow m a -> do
      n <- eval m >>= getVal :: EvalM (Matrix Expr)
      i <- eval a >>= getVal
      p <- tryEvalM loc $ mDelRow n i
      putTypeVal typ loc p >>= getVal

    MatDelCol m a -> do
      n <- eval m >>= getVal :: EvalM (Matrix Expr)
      i <- eval a >>= getVal
      p <- tryEvalM loc $ mDelCol n i
      putTypeVal typ loc p >>= getVal

    MatGJForm m -> do
      let gjFormMat x = lift1 loc m (mGJFormT x)
      case typ of
        QQ -> gjFormMat zeroQQ
        BB -> gjFormMat zeroBB
        _  -> reportErr loc $ FieldTypeExpected typ

    MatGJFactor m -> do
      let gjFactorMat x = lift1 loc m (mGJFactorT x)
      case typ of
        QQ -> gjFactorMat zeroQQ
        BB -> gjFactorMat zeroBB
        _  -> reportErr loc $ FieldTypeExpected typ

    MatGetRow k m -> do
      i <- eval k >>= getVal :: EvalM Integer
      n <- eval m >>= getVal :: EvalM (Matrix Expr)
      r <- tryEvalM loc $ mRowOf i n
      putTypeVal typ loc r >>= getVal

    MatGetCol k m -> do
      i <- eval k >>= getVal :: EvalM Integer
      n <- eval m >>= getVal :: EvalM (Matrix Expr)
      c <- tryEvalM loc $ mColOf i n
      putTypeVal typ loc c >>= getVal

    MatBuilder e kr lr kc lc -> do
      st <- getState
      rs <- eval lr >>= getVal :: EvalM [Expr]
      cs <- eval lc >>= getVal :: EvalM [Expr]
      let foo r c = do
            st' <- addKeyToStore kr r loc st >>= addKeyToStore kc c loc
            evalWith e st' >>= getVal
      es <- sequence [sequence [foo r c | c <- cs] | r <- rs] :: EvalM [[Expr]]
      m  <- tryEvalM loc $ mFromRowList es
      putTypeVal typ loc m >>= getVal
