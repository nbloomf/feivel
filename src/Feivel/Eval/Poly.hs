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

module Feivel.Eval.Poly () where

import Feivel.Eval.Util
import Carl.List


instance (Glyph Expr) => Glyph PolyExpr where
  toGlyph (PolyExpr (PolyConst px :# _ :@ loc)) = do
    qx <- polySeq $ mapCoef toGlyph px
    tryEvalM loc $ showStrP qx
  toGlyph x = error $ "toGlyph: PolyExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr,
  Eval ListExpr, Eval MatExpr)
  => Eval PolyExpr where

  eval (PolyExpr (zappa :# typ :@ loc)) = case zappa of
    PolyConst p -> do
      q <- polySeq $ mapCoef eval p
      return $ PolyExpr $ PolyConst q :# typ :@ loc
  
    {- :Common -}
    PolyVar        key      -> eKey key loc
    PolyAtIdx      m h k    -> eAtIdx m h k loc
    PolyMacro      vals mac -> eMacro vals mac loc
    PolyIfThenElse b t f    -> eIfThenElse b t f
  
    PolyAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [PolyExpr] -> Integer -> Either ListErr PolyExpr
  
    PolyRand ls -> do
      xs <- eval ls >>= getVal
      r  <- randomElementEvalM xs :: EvalM PolyExpr
      putTypeVal typ loc r >>= getVal
  
    PolyAdd a b -> do
      let addPoly x = lift2 loc a b (rAddT (constPoly x))
      case typ of
        ZZ          -> addPoly zeroZZ
        QQ          -> addPoly zeroQQ
        BB          -> addPoly zeroBB
        (ZZMod n)   -> addPoly (zeroMod n)
        PolyOver ZZ -> addPoly (constPoly zeroZZ)
        PolyOver QQ -> addPoly (constPoly zeroQQ)
        PolyOver BB -> addPoly (constPoly zeroBB)
        _ -> reportErr loc $ NumericTypeExpected typ
  
    PolySub a b -> do
      let subPoly x = lift2 loc a b (rSubT (constPoly x))
      case typ of
        ZZ          -> subPoly zeroZZ
        QQ          -> subPoly zeroQQ
        BB          -> subPoly zeroBB
        (ZZMod n)   -> subPoly (zeroMod n)
        PolyOver ZZ -> subPoly (constPoly zeroZZ)
        PolyOver QQ -> subPoly (constPoly zeroQQ)
        PolyOver BB -> subPoly (constPoly zeroBB)
        _ -> reportErr loc $ NumericTypeExpected typ
  
    PolyMul a b -> do
      let mulPoly x = lift2 loc a b (rMulT (constPoly x))
      case typ of
        ZZ          -> mulPoly zeroZZ
        QQ          -> mulPoly zeroQQ
        BB          -> mulPoly zeroBB
        (ZZMod n)   -> mulPoly (zeroMod n)
        PolyOver ZZ -> mulPoly (constPoly zeroZZ)
        PolyOver QQ -> mulPoly (constPoly zeroQQ)
        PolyOver BB -> mulPoly (constPoly zeroBB)
        _ -> reportErr loc $ NumericTypeExpected typ

    PolyQuo a b -> do
      let quoPoly x = lift2 loc a b (rQuoT (constPoly x))
      case typ of
        ZZ      -> quoPoly zeroZZ
        QQ      -> quoPoly zeroQQ
        BB      -> quoPoly zeroBB
        ZZMod n -> quoPoly (zeroMod n)
        _ -> reportErr loc $ NumericTypeExpected typ
  
    PolyRem a b -> do
      let remPoly x = lift2 loc a b (rRemT (constPoly x))
      case typ of
        ZZ      -> remPoly zeroZZ
        QQ      -> remPoly zeroQQ
        BB      -> remPoly zeroBB
        ZZMod n -> remPoly (zeroMod n)
        _ -> reportErr loc $ NumericTypeExpected typ

    PolySum ps -> do
      let sumPolys x = lift1 loc ps (rSumT (constPoly x))
      case typ of
        ZZ      -> sumPolys zeroZZ
        QQ      -> sumPolys zeroQQ
        BB      -> sumPolys zeroBB
        ZZMod n -> sumPolys (zeroMod n)
        _ -> reportErr loc $ NumericTypeExpected typ
  
    PolyNeg a -> do
      let negPoly x = lift1 loc a (rNegT (constPoly x))
      case typ of
        ZZ      -> negPoly zeroZZ
        QQ      -> negPoly zeroQQ
        BB      -> negPoly zeroBB
        ZZMod n -> negPoly (zeroMod n)
        _ -> reportErr loc $ NumericPolynomialExpected typ
  
    PolyPow a b -> do
      let powPoly x = lift2 loc a b (rPowT (constPoly x))
      case typ of
        ZZ      -> powPoly zeroZZ
        QQ      -> powPoly zeroQQ
        BB      -> powPoly zeroBB
        ZZMod n -> powPoly (zeroMod n)
        _ -> reportErr loc $ NumericPolynomialExpected typ

    PolyFromCoefs x as -> do
      let coefPoly z = do
            bs <- eval as >>= getVal
            suchThat $ bs `hasSameTypeAs` [z]
            p <- tryEvalM loc $ fromCoefficients (variable x) bs
            let q = mapCoef (put loc) p
            putTypeVal typ loc q >>= getVal
      case typ of
        ZZ      -> coefPoly zeroZZ
        QQ      -> coefPoly zeroQQ
        BB      -> coefPoly zeroBB
        ZZMod n -> coefPoly (zeroMod n)
        _       -> reportErr loc $ NumericPolynomialExpected typ

    PolyFromRoots x cs -> do
      let rootPoly z = do
            as <- eval cs >>= getVal
            suchThat $ as `hasSameTypeAs` [z]
            p <- tryEvalM loc $ fromRoots x as
            let q = mapCoef (put loc) p
            putTypeVal typ loc q >>= getVal
      case typ of
        ZZ      -> rootPoly zeroZZ
        QQ      -> rootPoly zeroQQ
        BB      -> rootPoly zeroBB
        ZZMod n -> rootPoly (zeroMod n)
        _ -> reportErr loc $ NumericListExpected typ
  
    PolyEvalPoly p qs -> do
      let evalPoly z = do
            a <- eval p >>= getVal
            suchThat $ a `hasSameTypeAs` (constPoly z)
            let foo (x,h) = do
                  b <- eval h >>= getVal
                  suchThat $ b `hasSameTypeAs` (constPoly z)
                  return (x,b)
            ks <- sequence $ map foo qs
            c  <- tryEvalM loc $ evalPolyAtPolys ks a
            putTypeVal typ loc (mapCoef (put loc) c) >>= getVal
      case typ of
        ZZ      -> evalPoly zeroZZ
        QQ      -> evalPoly zeroQQ
        BB      -> evalPoly zeroBB
        ZZMod n -> evalPoly (zeroMod n)
        _ -> reportErr loc $ NumericPolynomialExpected typ
