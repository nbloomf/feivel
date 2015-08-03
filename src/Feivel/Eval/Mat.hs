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

module Feivel.Eval.Mat () where

import Feivel.Eval.Util


instance (Glyph Expr) => Glyph MatExpr where
  toGlyph (MatExpr (MatConst _ m :@ _)) = do
    n <- mSeq $ fmap toGlyph m
    case mShowStr n of
      Left err -> reportErr (error "Glyph instance of MatExpr") err
      Right x  -> return x
  toGlyph x = error $ "toGlyph: MatExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr) => Eval MatExpr where
  eval (MatExpr (MatConst t m :@ loc)) = do
    n <- mSeq $ fmap eval m
    return $ MatExpr $ MatConst t n :@ loc

  {- :Common -}
  eval (MatExpr (MatVar _ key :@ loc))        = eKey key loc
  eval (MatExpr (MatAtIdx _ m h k :@ loc))    = eAtIdx m h k loc
  eval (MatExpr (MatMacro _ vals mac :@ loc)) = eMacro vals mac loc
  eval (MatExpr (MatIfThenElse _ b t f :@ _)) = eIfThenElse b t f

  eval (MatExpr (MatAtPos _ a t :@ loc)) = lift2 loc a t (foo)
    where foo = listAtPos :: [MatExpr] -> Integer -> Either ListErr MatExpr

  eval (MatExpr (MatRand _ ls :@ _)) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    eval r >>= getVal

  eval (MatExpr (MatRowFromList t xs :@ loc)) = do
    as <- eval xs >>= getVal :: EvalM [Expr]
    m  <- tryEvalM loc $ mRowFromList as
    putTypeVal t loc m >>= getVal

  eval (MatExpr (MatColFromList t xs :@ loc)) = do
    as <- eval xs >>= getVal :: EvalM [Expr]
    m  <- tryEvalM loc $ mColFromList as
    putTypeVal t loc m >>= getVal

  eval (MatExpr (MatId u n :@ loc)) = do
    let makeIdMat x = lift1 loc n (mEIdT x)
    case u of
      ZZ          -> makeIdMat zeroZZ
      QQ          -> makeIdMat zeroQQ
      BB          -> makeIdMat zeroBB
      ZZMod k     -> makeIdMat (zeroMod k)
      PolyOver ZZ -> makeIdMat (constPoly zeroZZ)
      PolyOver QQ -> makeIdMat (constPoly zeroQQ)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatExpr (MatSwapE u n h k :@ loc)) = do
    let makeSwapMat x = lift3 loc n h k (mESwapT x)
    case u of
      ZZ          -> makeSwapMat zeroZZ
      QQ          -> makeSwapMat zeroQQ
      BB          -> makeSwapMat zeroBB
      ZZMod n     -> makeSwapMat (zeroMod n)
      PolyOver ZZ -> makeSwapMat (constPoly zeroZZ)
      PolyOver QQ -> makeSwapMat (constPoly zeroQQ)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatExpr (MatScaleE u n h r :@ loc)) = do
    let makeScaleMat x = lift3 loc n h r (mEScaleT x)
    case u of
      ZZ          -> makeScaleMat zeroZZ
      QQ          -> makeScaleMat zeroQQ
      BB          -> makeScaleMat zeroBB
      ZZMod n     -> makeScaleMat (zeroMod n)
      PolyOver ZZ -> makeScaleMat (constPoly zeroZZ)
      PolyOver QQ -> makeScaleMat (constPoly zeroQQ)
      PolyOver BB -> makeScaleMat (constPoly zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatExpr (MatAddE u n h k r :@ loc)) = do
    let makeAddMat x = lift4 loc n h k r (mEAddT x)
    case u of
      ZZ          -> makeAddMat zeroZZ
      QQ          -> makeAddMat zeroQQ
      BB          -> makeAddMat zeroBB
      ZZMod d     -> makeAddMat (zeroMod d)
      PolyOver ZZ -> makeAddMat (constPoly zeroZZ)
      PolyOver QQ -> makeAddMat (constPoly zeroQQ)
      PolyOver BB -> makeAddMat (constPoly zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatExpr (MatShuffleRows u m :@ loc)) = do
    x  <- eval m >>= getVal :: EvalM (Matrix Expr)
    rs <- tryEvalM loc $ mRowsOf x
    ts <- shuffleEvalM rs
    n  <- tryEvalM loc $ mVCats ts
    putTypeVal u loc n >>= getVal

  eval (MatExpr (MatShuffleCols u m :@ loc)) = do
    x  <- eval m >>= getVal :: EvalM (Matrix Expr)
    rs <- tryEvalM loc $ mColsOf x
    ts <- shuffleEvalM rs
    n  <- tryEvalM loc $ mHCats ts
    putTypeVal u loc n >>= getVal

  eval (MatExpr (MatHCat u a b :@ loc)) = do
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    n <- eval b >>= getVal :: EvalM (Matrix Expr)
    x <- tryEvalM loc $ mHCat m n
    putTypeVal u loc x >>= getVal

  eval (MatExpr (MatVCat u a b :@ loc)) = do
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    n <- eval b >>= getVal :: EvalM (Matrix Expr)
    x <- tryEvalM loc $ mVCat m n
    putTypeVal u loc x >>= getVal

  eval (MatExpr (MatAdd u a b :@ loc)) = do
    let addMat x = lift2 loc a b (rAddT (mCell x))
    case u of
      ZZ          -> addMat zeroZZ
      QQ          -> addMat zeroQQ
      BB          -> addMat zeroBB
      ZZMod n     -> addMat (zeroMod n)
      PolyOver ZZ -> addMat (constPoly zeroZZ)
      PolyOver QQ -> addMat (constPoly zeroQQ)
      PolyOver BB -> addMat (constPoly zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatExpr (MatNeg u a :@ loc)) = do
    let negMat x = lift1 loc a (rNegT (mCell x))
    case u of
      ZZ          -> negMat zeroZZ
      QQ          -> negMat zeroQQ
      BB          -> negMat zeroBB
      ZZMod n     -> negMat (zeroMod n)
      PolyOver ZZ -> negMat (constPoly zeroZZ)
      PolyOver QQ -> negMat (constPoly zeroQQ)
      PolyOver BB -> negMat (constPoly zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatExpr (MatTrans u a :@ loc)) = do
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    p <- tryEvalM loc $ mTranspose m
    putTypeVal u loc p >>= getVal

  eval (MatExpr (MatMul u a b :@ loc)) = do
    let mulMat x = lift2 loc a b (rMulT (mCell x))
    case u of
      ZZ          -> mulMat zeroZZ
      QQ          -> mulMat zeroQQ
      BB          -> mulMat zeroBB
      (ZZMod n)   -> mulMat (zeroMod n)
      PolyOver ZZ -> mulMat (constPoly zeroZZ)
      PolyOver QQ -> mulMat (constPoly zeroQQ)
      PolyOver BB -> mulMat (constPoly zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatExpr (MatPow u m n :@ loc)) = do
    let powMat x = lift2 loc m n (rPosPowT (mCell x))
    case u of
      ZZ          -> powMat zeroZZ
      QQ          -> powMat zeroQQ
      BB          -> powMat zeroBB
      (ZZMod k)   -> powMat (zeroMod k)
      PolyOver ZZ -> powMat (constPoly zeroZZ)
      PolyOver QQ -> powMat (constPoly zeroQQ)
      PolyOver BB -> powMat (constPoly zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatExpr (MatSwapRows u m a b :@ loc)) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    j <- eval b >>= getVal
    p <- tryEvalM loc $ mSwapRows i j n
    putTypeVal u loc p >>= getVal

  eval (MatExpr (MatSwapCols u m a b :@ loc)) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    j <- eval b >>= getVal
    p <- tryEvalM loc $ mSwapCols i j n
    putTypeVal u loc p >>= getVal

  eval (MatExpr (MatScaleRow u m a h :@ loc)) = do
    let scaleRowMat x = lift3 loc a h m (mScaleRowT x)
    case u of
      ZZ          -> scaleRowMat zeroZZ
      QQ          -> scaleRowMat zeroQQ
      BB          -> scaleRowMat zeroBB
      (ZZMod n)   -> scaleRowMat (zeroMod n)
      PolyOver ZZ -> scaleRowMat (constPoly zeroZZ)
      PolyOver QQ -> scaleRowMat (constPoly zeroQQ)
      PolyOver BB -> scaleRowMat (constPoly zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatExpr (MatScaleCol u m a h :@ loc)) = do
    let scaleColMat x = lift3 loc a h m (mScaleColT x)
    case u of
      ZZ          -> scaleColMat zeroZZ
      QQ          -> scaleColMat zeroQQ
      BB          -> scaleColMat zeroBB
      (ZZMod n)   -> scaleColMat (zeroMod n)
      PolyOver ZZ -> scaleColMat (constPoly zeroZZ)
      PolyOver QQ -> scaleColMat (constPoly zeroQQ)
      PolyOver BB -> scaleColMat (constPoly zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatExpr (MatAddRow u m a h k :@ loc)) = do
    i <- eval h >>= getVal
    j <- eval k >>= getVal
    let makeAddRow zer = do
          n <- eval m >>= getVal
          r <- eval a >>= getVal
          suchThat $ r `hasSameTypeAs` zer
          p <- tryEvalM loc $ mAddRow r i j n
          putTypeVal u loc p >>= getVal
    case u of
      ZZ      -> makeAddRow zeroZZ
      QQ      -> makeAddRow zeroQQ
      BB      -> makeAddRow zeroBB
      ZZMod n -> makeAddRow (zeroMod n)
      _ -> reportErr loc $ NumericMatrixExpected u

  eval (MatExpr (MatAddCol u m a h k :@ loc)) = do
    i <- eval h >>= getVal
    j <- eval k >>= getVal
    let makeAddCol zer = do
          n <- eval m >>= getVal
          r <- eval a >>= getVal
          suchThat $ r `hasSameTypeAs` zer
          p <- tryEvalM loc $ mAddCol r i j n
          getVal (put loc p)
    case u of
      ZZ      -> makeAddCol zeroZZ
      QQ      -> makeAddCol zeroQQ
      BB      -> makeAddCol zeroBB
      ZZMod n -> makeAddCol (zeroMod n)
      _ -> reportErr loc $ NumericMatrixExpected u

  eval (MatExpr (MatDelRow u m a :@ loc)) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    p <- tryEvalM loc $ mDelRow n i
    putTypeVal u loc p >>= getVal

  eval (MatExpr (MatDelCol u m a :@ loc)) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    p <- tryEvalM loc $ mDelCol n i
    putTypeVal u loc p >>= getVal

  eval (MatExpr (MatGJForm u m :@ loc)) = do
    let gjFormMat x = lift1 loc m (mGJFormT x)
    case u of
      QQ -> gjFormMat zeroQQ
      BB -> gjFormMat zeroBB
      _  -> reportErr loc $ FieldTypeExpected u

  eval (MatExpr (MatGJFactor u m :@ loc)) = do
    let gjFactorMat x = lift1 loc m (mGJFactorT x)
    case u of
      QQ -> gjFactorMat zeroQQ
      BB -> gjFactorMat zeroBB
      _  -> reportErr loc $ FieldTypeExpected u

  eval (MatExpr (MatGetRow u k m :@ loc)) = do
    i <- eval k >>= getVal :: EvalM Integer
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    r <- tryEvalM loc $ mRowOf i n
    putTypeVal u loc r >>= getVal

  eval (MatExpr (MatGetCol u k m :@ loc)) = do
    i <- eval k >>= getVal :: EvalM Integer
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    c <- tryEvalM loc $ mColOf i n
    putTypeVal u loc c >>= getVal

  eval (MatExpr (MatBuilder typ e kr lr kc lc :@ loc)) = do
    st <- getState
    rs <- eval lr >>= getVal :: EvalM [Expr]
    cs <- eval lc >>= getVal :: EvalM [Expr]
    let foo r c = do
          st' <- addKeyToStore kr r loc st >>= addKeyToStore kc c loc
          evalWith e st' >>= getVal
    es <- sequence [sequence [foo r c | c <- cs] | r <- rs] :: EvalM [[Expr]]
    m  <- tryEvalM loc $ mFromRowList es
    putTypeVal typ loc m >>= getVal
