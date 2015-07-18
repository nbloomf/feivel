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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}

module Feivel.Eval.Poly where

import Feivel.EvalM
import Feivel.Expr
import Feivel.Type
import Feivel.Locus
import Feivel.Lib
import Feivel.Get
import Feivel.Put
import Feivel.Typed
import Feivel.Eval.Eval
import Feivel.Eval.Util
import Feivel.Error

{------------------}
{- :Eval:PolyExpr -}
{------------------}

instance (Glyph Expr) => Glyph (PolyExpr Expr) where
  toGlyph (PolyConst _ px :@ _) = do
    qx <- polySeq $ mapCoef toGlyph px
    return $ showStrP qx
  toGlyph x = error $ "toGlyph: PolyExpr: " ++ show x

instance (Eval Expr) => Eval (PolyExpr Expr) where
  eval (PolyConst t p :@ loc) = do
    q <- polySeq $ fmap eval p
    return $ PolyConst t q :@ loc

  {- :Common -}
  eval (PolyVar _ key :@ loc)        = eKey key loc
  eval (PolyAtIdx _ m h k :@ loc)    = eAtIdx m h k loc
  eval (PolyMacro _ vals mac :@ loc) = eMacro vals mac loc
  eval (PolyIfThenElse _ b t f :@ _) = eIfThenElse b t f

  eval (PolyAtPos _ a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [PolyExpr Expr] -> Integer -> Either ListErr (PolyExpr Expr)

  eval (PolyRand _ ls :@ loc) = do
    let t = typeOf ls
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    s  <- eval r >>= getVal :: EvalM (PolyExpr Expr)
    case t of
      ListOf (PolyOver _) -> return s
      _ -> reportErr loc $ ListExpected t

  eval (PolyAdd u a b :@ loc) = do
    let addPoly x = lift2 loc a b (rAddT (constP x))
    case u of
      ZZ          -> addPoly zeroZZ
      QQ          -> addPoly zeroQQ
      BB          -> addPoly zeroBB
      (ZZMod n)   -> addPoly (zeroMod n)
      PolyOver ZZ -> addPoly (constP zeroZZ)
      PolyOver QQ -> addPoly (constP zeroQQ)
      PolyOver BB -> addPoly (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (PolySub u a b :@ loc) = do
    let subPoly x = lift2 loc a b (rSubT (constP x))
    case u of
      ZZ          -> subPoly zeroZZ
      QQ          -> subPoly zeroQQ
      BB          -> subPoly zeroBB
      (ZZMod n)   -> subPoly (zeroMod n)
      PolyOver ZZ -> subPoly (constP zeroZZ)
      PolyOver QQ -> subPoly (constP zeroQQ)
      PolyOver BB -> subPoly (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (PolyMul u a b :@ loc) = do
    let mulPoly x = lift2 loc a b (rMulT (constP x))
    case u of
      ZZ          -> mulPoly zeroZZ
      QQ          -> mulPoly zeroQQ
      BB          -> mulPoly zeroBB
      (ZZMod n)   -> mulPoly (zeroMod n)
      PolyOver ZZ -> mulPoly (constP zeroZZ)
      PolyOver QQ -> mulPoly (constP zeroQQ)
      PolyOver BB -> mulPoly (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (PolyNeg _ a :@ loc) = do
    let t = typeOf a
    case t of
      PolyOver ZZ ->
        lift1 loc a (rNegT (constP (0::Integer)))
      PolyOver QQ ->
        lift1 loc a (rNegT (constP (0:/:1)))
      PolyOver BB ->
        lift1 loc a (rNegT (constP False))
      PolyOver (ZZMod n) -> do
        x <- eval a >>= getVal :: EvalM (Poly ZZModulo)
        let y = rNeg x
        return $ PolyConst (ZZMod n) (fmap toExpr y) :@ loc
      _ -> reportErr loc $ NumericPolynomialExpected t

  eval (PolyPow _ a b :@ loc) = do
    let t = typeOf a
    case t of
      PolyOver ZZ ->
        lift2 loc a b (rPowT (constP (0::Integer)))
      PolyOver QQ ->
        lift2 loc a b (rPowT (constP (0:/:1)))
      PolyOver BB ->
        lift2 loc a b (rPowT (constP False))
      PolyOver (ZZMod n) -> do
        x <- eval a >>= getVal :: EvalM (Poly ZZModulo)
        y <- eval b >>= getVal :: EvalM Integer
        z <- tryEvalM loc $ rPow x y
        return $ PolyConst (ZZMod n) (fmap toExpr z) :@ loc
      _ -> reportErr loc $ NumericPolynomialExpected t

  eval (PolyFromRoots _ x cs :@ loc) = do
    let t = typeOf cs
    case t of
      ListOf ZZ -> do
        as <- eval cs >>= getVal :: EvalM [Integer]
        p  <- tryEvalM loc $ fromRootsP x as
        let q = fmap toExpr p
        return (PolyConst ZZ q :@ loc)
      ListOf QQ -> do
        as <- eval cs >>= getVal :: EvalM [Rat]
        p  <- tryEvalM loc $ fromRootsP x as
        let q = fmap toExpr p
        return (PolyConst QQ q :@ loc)
      ListOf (ZZMod n) -> do
        as <- eval cs >>= getVal :: EvalM [ZZModulo]
        p  <- tryEvalM loc $ fromRootsP x as
        let q = fmap toExpr p
        return (PolyConst (ZZMod n) q :@ loc)
      _ -> reportErr loc $ NumericListExpected t

  eval (PolyEvalPoly _ p qs :@ loc) = do
    let t = typeOf p
    case t of
      PolyOver ZZ -> do
        a <- eval p >>= getVal :: EvalM (Poly Integer)
        let foo (x,h) = do
              b <- eval h >>= getVal :: EvalM (Poly Integer)
              return (x,b)
        ks <- sequence $ map foo qs
        c  <- tryEvalM loc $ evalPolyAtPolysP ks a
        return (PolyConst ZZ (fmap toExpr c) :@ loc)
      PolyOver QQ -> do
        a <- eval p >>= getVal :: EvalM (Poly Rat)
        let foo (x,h) = do
              b <- eval h >>= getVal :: EvalM (Poly Rat)
              return (x,b)
        ks <- sequence $ map foo qs
        c  <- tryEvalM loc $ evalPolyAtPolysP ks a
        return (PolyConst QQ (fmap toExpr c) :@ loc)
      PolyOver (ZZMod n) -> do
        a <- eval p >>= getVal :: EvalM (Poly ZZModulo)
        let foo (x,h) = do
              b <- eval h >>= getVal :: EvalM (Poly ZZModulo)
              return (x,b)
        ks <- sequence $ map foo qs
        c  <- tryEvalM loc $ evalPolyAtPolysP ks a
        return (PolyConst (ZZMod n) (fmap toExpr c) :@ loc)
      _ -> reportErr loc $ NumericPolynomialExpected t

