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

{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feivel.Eval.Str where

import Feivel.Eval.Util
import Carl.List
import Carl.String
import Carl.Write.LaTeX


instance (Glyph Expr) => Glyph StrExpr where
  toGlyph (StrExpr (StrConst (Text s) :@ _)) = return s
  toGlyph x = error $ "toGlyph: StrExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr, Eval ListExpr, Eval MatExpr, Eval TupleExpr, Glyph Expr) => Eval StrExpr where
  eval (StrExpr (zappa :@ loc)) = case zappa of
    StrConst s -> return (StrExpr $ StrConst s :@ loc)

    {- :Common -}
    StrVar        key      -> eKey key loc
    StrAtIdx      m h k    -> eAtIdx m h k loc
    StrIfThenElse b t f    -> eIfThenElse b t f
    StrMacro      vals mac -> eMacro vals mac loc

    StrAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [StrExpr] -> Integer -> Either ListErr StrExpr

    StrAtSlot t i -> do
      x <- eval t >>= getVal :: EvalM (Tuple Expr)
      n <- eval i >>= getVal :: EvalM Integer
      k <- tryEvalM loc $ project x n
      putVal loc k >>= getVal

    Concat   a b -> lift2 loc a b (strCat)
    StrStrip a b -> lift2 loc a b (strStrip)

    ToUpper   a -> lift1 loc a (strUpper)
    ToLower   a -> lift1 loc a (strLower)
    Reverse   a -> lift1 loc a (strRev)
    Rot13     a -> lift1 loc a (strRot13)
    StrHex    n -> lift1 loc n (strHex)
    StrRoman  n -> lift1 loc n (strRoman)
    StrBase36 n -> lift1 loc n (strBase36)

    StrDecimal p k -> do
      x <- eval p >>= getVal
      d <- eval k >>= getVal
      putVal loc (Text $ digits d x) >>= getVal

    StrRand ls -> do
      xs <- eval ls >>= getVal
      r  <- randomElementEvalM xs :: EvalM Text
      putVal loc r >>= getVal

    StrTab m -> do
      n <- eval m >>= getVal :: EvalM (Matrix Expr)
      tab <- tabulateWithM toGlyph n
      putVal loc (Text tab) >>= getVal

    StrTypeOf e -> do
      putVal loc (Text $ show $ typeOf e) >>= getVal

    StrFormat LaTeX e -> do
      case typeOf e of
        ZZ -> do
          x <- eval e >>= getVal :: EvalM Integer
          putVal loc (Text $ latex x) >>= getVal
        QQ -> do
          x <- eval e >>= getVal :: EvalM Rat
          putVal loc (Text $ latex x) >>= getVal
        MatOf ZZ -> do
          x <- eval e >>= getVal :: EvalM (Matrix Integer)
          putVal loc (Text $ latex x) >>= getVal
        MatOf QQ -> do
          x <- eval e >>= getVal :: EvalM (Matrix Rat)
          putVal loc (Text $ latex x) >>= getVal
        PolyOver ZZ -> do
          x <- eval e >>= getVal :: EvalM (Poly Integer)
          putVal loc (Text $ latex x) >>= getVal
        PolyOver QQ -> do
          x <- eval e >>= getVal :: EvalM (Poly Rat)
          putVal loc (Text $ latex x) >>= getVal
        _ -> error "StrFormat LaTeX"

    StrIntCast k -> do
      n <- eval k >>= getVal :: EvalM Integer
      putVal loc (Text $ show n) >>= getVal
