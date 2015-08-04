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


instance (Glyph Expr) => Glyph PolyExpr where
  toGlyph (PolyExpr (PolyConst _ px :@ loc)) = do
    qx <- polySeq $ mapCoef toGlyph px
    tryEvalM loc $ showStrP qx
  toGlyph x = error $ "toGlyph: PolyExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr, Eval ListExpr) => Eval PolyExpr where
  eval (PolyExpr (zappa :@ loc)) = case zappa of
    PolyConst t p -> do
      q <- polySeq $ mapCoef eval p
      return $ PolyExpr $ PolyConst t q :@ loc
  
    {- :Common -}
    PolyVar        _ key      -> eKey key loc
    PolyAtIdx      _ m h k    -> eAtIdx m h k loc
    PolyMacro      _ vals mac -> eMacro vals mac loc
    PolyIfThenElse _ b t f    -> eIfThenElse b t f
  
    PolyAtPos _ a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [PolyExpr] -> Integer -> Either ListErr PolyExpr
  
    PolyRand u ls -> do
      xs <- eval ls >>= getVal
      r  <- randomElementEvalM xs :: EvalM PolyExpr
      putTypeVal u loc r >>= getVal
  
    PolyAdd u a b -> do
      let addPoly x = lift2 loc a b (rAddT (constPoly x))
      case u of
        ZZ          -> addPoly zeroZZ
        QQ          -> addPoly zeroQQ
        BB          -> addPoly zeroBB
        (ZZMod n)   -> addPoly (zeroMod n)
        PolyOver ZZ -> addPoly (constPoly zeroZZ)
        PolyOver QQ -> addPoly (constPoly zeroQQ)
        PolyOver BB -> addPoly (constPoly zeroBB)
        _ -> reportErr loc $ NumericTypeExpected u
  
    PolySub u a b -> do
      let subPoly x = lift2 loc a b (rSubT (constPoly x))
      case u of
        ZZ          -> subPoly zeroZZ
        QQ          -> subPoly zeroQQ
        BB          -> subPoly zeroBB
        (ZZMod n)   -> subPoly (zeroMod n)
        PolyOver ZZ -> subPoly (constPoly zeroZZ)
        PolyOver QQ -> subPoly (constPoly zeroQQ)
        PolyOver BB -> subPoly (constPoly zeroBB)
        _ -> reportErr loc $ NumericTypeExpected u
  
    PolyMul u a b -> do
      let mulPoly x = lift2 loc a b (rMulT (constPoly x))
      case u of
        ZZ          -> mulPoly zeroZZ
        QQ          -> mulPoly zeroQQ
        BB          -> mulPoly zeroBB
        (ZZMod n)   -> mulPoly (zeroMod n)
        PolyOver ZZ -> mulPoly (constPoly zeroZZ)
        PolyOver QQ -> mulPoly (constPoly zeroQQ)
        PolyOver BB -> mulPoly (constPoly zeroBB)
        _ -> reportErr loc $ NumericTypeExpected u

    PolyQuo u a b -> do
      let quoPoly x = lift2 loc a b (rQuoT (constPoly x))
      case u of
        ZZ      -> quoPoly zeroZZ
        QQ      -> quoPoly zeroQQ
        BB      -> quoPoly zeroBB
        ZZMod n -> quoPoly (zeroMod n)
        _ -> reportErr loc $ NumericTypeExpected u
  
    PolyRem u a b -> do
      let remPoly x = lift2 loc a b (rRemT (constPoly x))
      case u of
        ZZ      -> remPoly zeroZZ
        QQ      -> remPoly zeroQQ
        BB      -> remPoly zeroBB
        ZZMod n -> remPoly (zeroMod n)
        _ -> reportErr loc $ NumericTypeExpected u

    PolySum u ps -> do
      let sumPolys x = lift1 loc ps (rSumT (constPoly x))
      case u of
        ZZ      -> sumPolys zeroZZ
        QQ      -> sumPolys zeroQQ
        BB      -> sumPolys zeroBB
        ZZMod n -> sumPolys (zeroMod n)
        _ -> reportErr loc $ NumericTypeExpected u
  
    PolyNeg u a -> do
      let negPoly x = lift1 loc a (rNegT (constPoly x))
      case u of
        ZZ      -> negPoly zeroZZ
        QQ      -> negPoly zeroQQ
        BB      -> negPoly zeroBB
        ZZMod n -> negPoly (zeroMod n)
        _ -> reportErr loc $ NumericPolynomialExpected u
  
    PolyPow u a b -> do
      let powPoly x = lift2 loc a b (rPowT (constPoly x))
      case u of
        ZZ      -> powPoly zeroZZ
        QQ      -> powPoly zeroQQ
        BB      -> powPoly zeroBB
        ZZMod n -> powPoly (zeroMod n)
        _ -> reportErr loc $ NumericPolynomialExpected u

    PolyFromRoots u x cs -> do
      let rootPoly z = do
            as <- eval cs >>= getVal
            suchThat $ as `hasSameTypeAs` [z]
            p <- tryEvalM loc $ fromRoots x as
            let q = mapCoef (put loc) p
            putTypeVal u loc q >>= getVal
      case u of
        ZZ      -> rootPoly zeroZZ
        QQ      -> rootPoly zeroQQ
        BB      -> rootPoly zeroBB
        ZZMod n -> rootPoly (zeroMod n)
        _ -> reportErr loc $ NumericListExpected u
  
    PolyEvalPoly u p qs -> do
      let evalPoly z = do
            a <- eval p >>= getVal
            suchThat $ a `hasSameTypeAs` (constPoly z)
            let foo (x,h) = do
                  b <- eval h >>= getVal
                  suchThat $ b `hasSameTypeAs` (constPoly z)
                  return (x,b)
            ks <- sequence $ map foo qs
            c  <- tryEvalM loc $ evalPolyAtPolys ks a
            putTypeVal u loc (mapCoef (put loc) c) >>= getVal
      case u of
        ZZ      -> evalPoly zeroZZ
        QQ      -> evalPoly zeroQQ
        BB      -> evalPoly zeroBB
        ZZMod n -> evalPoly (zeroMod n)
        _ -> reportErr loc $ NumericPolynomialExpected u
