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

module Feivel.Eval.Poly () where

import Feivel.Eval.Util


instance (Glyph Expr) => Glyph PolyExpr where
  toGlyph (PolyExpr (PolyConst _ px :@ _)) = do
    qx <- polySeq $ mapCoef toGlyph px
    return $ showStrP qx
  toGlyph x = error $ "toGlyph: PolyExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr) => Eval PolyExpr where
  eval (PolyExpr (PolyConst t p :@ loc)) = do
    q <- polySeq $ fmap eval p
    return $ PolyExpr $ PolyConst t q :@ loc

  {- :Common -}
  eval (PolyExpr (PolyVar _ key :@ loc))        = eKey key loc
  eval (PolyExpr (PolyAtIdx _ m h k :@ loc))    = eAtIdx m h k loc
  eval (PolyExpr (PolyMacro _ vals mac :@ loc)) = eMacro vals mac loc
  eval (PolyExpr (PolyIfThenElse _ b t f :@ _)) = eIfThenElse b t f

  eval (PolyExpr (PolyAtPos _ a t :@ loc)) = lift2 loc a t (foo)
    where foo = listAtPos :: [PolyExpr] -> Integer -> Either ListErr PolyExpr

  eval (PolyExpr (PolyRand _ ls :@ loc)) = do
    let t = typeOf ls
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    s  <- eval r >>= getVal :: EvalM PolyExpr
    case t of
      ListOf (PolyOver _) -> return s
      _ -> reportErr loc $ ListExpected t

  eval (PolyExpr (PolyAdd u a b :@ loc)) = do
    let addPoly x = lift2 loc a b (rAddT (constP x))
    case u of
      ZZ          -> addPoly zeroZZ
      QQ          -> addPoly zeroQQ
      BB          -> addPoly zeroBB
      (ZZMod n)   -> addPoly (zeroMod n)
      PolyOver ZZ -> addPoly (constP zeroZZ)
      PolyOver QQ -> addPoly (constP zeroQQ)
      PolyOver BB -> addPoly (constP zeroBB)
      _ -> reportErr loc $ NumericTypeExpected u

  eval (PolyExpr (PolySub u a b :@ loc)) = do
    let subPoly x = lift2 loc a b (rSubT (constP x))
    case u of
      ZZ          -> subPoly zeroZZ
      QQ          -> subPoly zeroQQ
      BB          -> subPoly zeroBB
      (ZZMod n)   -> subPoly (zeroMod n)
      PolyOver ZZ -> subPoly (constP zeroZZ)
      PolyOver QQ -> subPoly (constP zeroQQ)
      PolyOver BB -> subPoly (constP zeroBB)
      _ -> reportErr loc $ NumericTypeExpected u

  eval (PolyExpr (PolyMul u a b :@ loc)) = do
    let mulPoly x = lift2 loc a b (rMulT (constP x))
    case u of
      ZZ          -> mulPoly zeroZZ
      QQ          -> mulPoly zeroQQ
      BB          -> mulPoly zeroBB
      (ZZMod n)   -> mulPoly (zeroMod n)
      PolyOver ZZ -> mulPoly (constP zeroZZ)
      PolyOver QQ -> mulPoly (constP zeroQQ)
      PolyOver BB -> mulPoly (constP zeroBB)
      _ -> reportErr loc $ NumericTypeExpected u

  eval (PolyExpr (PolyNeg u a :@ loc)) = do
    let negPoly x = lift1 loc a (rNegT (constP x))
    case u of
      ZZ      -> negPoly zeroZZ
      QQ      -> negPoly zeroQQ
      BB      -> negPoly zeroBB
      ZZMod n -> negPoly (zeroMod n)
      _ -> reportErr loc $ NumericPolynomialExpected u

  eval (PolyExpr (PolyPow u a b :@ loc)) = do
    let powPoly x = lift2 loc a b (rPowT (constP x))
    case u of
      ZZ      -> powPoly zeroZZ
      QQ      -> powPoly zeroQQ
      BB      -> powPoly zeroBB
      ZZMod n -> powPoly (zeroMod n)
      _ -> reportErr loc $ NumericPolynomialExpected u

  eval (PolyExpr (PolyFromRoots _ x cs :@ loc)) = do
    let t = typeOf cs
    case t of
      ListOf ZZ -> do
        as <- eval cs >>= getVal :: EvalM [Integer]
        p  <- tryEvalM loc $ fromRootsP x as
        let q = fmap (put loc) p
        putTypeVal ZZ loc q >>= getVal
      ListOf QQ -> do
        as <- eval cs >>= getVal :: EvalM [Rat]
        p  <- tryEvalM loc $ fromRootsP x as
        let q = fmap (put loc) p
        putTypeVal QQ loc q >>= getVal
      ListOf (ZZMod n) -> do
        as <- eval cs >>= getVal :: EvalM [ZZModulo]
        p  <- tryEvalM loc $ fromRootsP x as
        let q = fmap (put loc) p
        putTypeVal (ZZMod n) loc q >>= getVal
      _ -> reportErr loc $ NumericListExpected t

  eval (PolyExpr (PolyEvalPoly u p qs :@ loc)) = do
    case u of
      ZZ -> do
        a <- eval p >>= getVal :: EvalM (Poly Integer)
        let foo (x,h) = do
              b <- eval h >>= getVal :: EvalM (Poly Integer)
              return (x,b)
        ks <- sequence $ map foo qs
        c  <- tryEvalM loc $ evalPolyAtPolysP ks a
        putTypeVal ZZ loc (fmap (put loc) c) >>= getVal
      QQ -> do
        a <- eval p >>= getVal :: EvalM (Poly Rat)
        let foo (x,h) = do
              b <- eval h >>= getVal :: EvalM (Poly Rat)
              return (x,b)
        ks <- sequence $ map foo qs
        c  <- tryEvalM loc $ evalPolyAtPolysP ks a
        putTypeVal QQ loc (fmap (put loc) c) >>= getVal
      ZZMod n -> do
        a <- eval p >>= getVal :: EvalM (Poly ZZModulo)
        let foo (x,h) = do
              b <- eval h >>= getVal :: EvalM (Poly ZZModulo)
              return (x,b)
        ks <- sequence $ map foo qs
        c  <- tryEvalM loc $ evalPolyAtPolysP ks a
        putTypeVal (ZZMod n) loc (fmap (put loc) c) >>= getVal
      _ -> reportErr loc $ NumericPolynomialExpected u
