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

{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feivel.Eval.Str where

import Feivel.Eval.Util


instance (Glyph Expr) => Glyph StrExpr where
  toGlyph (StrExpr (StrConst (Text s) :@ _)) = return s
  toGlyph x = error $ "toGlyph: StrExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr, Glyph Expr) => Eval StrExpr where
  eval (StrExpr (StrConst s :@ loc)) = return (StrExpr $ StrConst s :@ loc)

  {- :Common -}
  eval (StrExpr (StrVar key :@ loc))        = eKey key loc
  eval (StrExpr (StrAtIdx m h k :@ loc))    = eAtIdx m h k loc
  eval (StrExpr (StrIfThenElse b t f :@ _)) = eIfThenElse b t f
  eval (StrExpr (StrMacro vals mac :@ loc)) = eMacro vals mac loc

  eval (StrExpr (StrAtPos a t :@ loc)) = lift2 loc a t (foo)
    where foo = listAtPos :: [StrExpr] -> Integer -> Either ListErr StrExpr

  eval (StrExpr (Concat   a b :@ loc)) = lift2 loc a b (strCat)
  eval (StrExpr (StrStrip a b :@ loc)) = lift2 loc a b (strStrip)

  eval (StrExpr (ToUpper   a :@ loc)) = lift1 loc a (strUpper)
  eval (StrExpr (ToLower   a :@ loc)) = lift1 loc a (strLower)
  eval (StrExpr (Reverse   a :@ loc)) = lift1 loc a (strRev)
  eval (StrExpr (Rot13     a :@ loc)) = lift1 loc a (strRot13)
  eval (StrExpr (StrHex    n :@ loc)) = lift1 loc n (strHex)
  eval (StrExpr (StrRoman  n :@ loc)) = lift1 loc n (strRoman)
  eval (StrExpr (StrBase36 n :@ loc)) = lift1 loc n (strBase36)

  eval (StrExpr (StrDecimal p k :@ loc)) = do
    x <- eval p >>= getVal
    d <- eval k >>= getVal
    putVal loc (Text $ digits d x) >>= getVal

  eval (StrExpr (StrRand ls :@ loc)) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs :: EvalM Text
    putVal loc r >>= getVal

  eval (StrExpr (StrTab m :@ loc)) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    tab <- tabulateWithM toGlyph n
    putVal loc (Text tab) >>= getVal

  eval (StrExpr (StrTypeOf e :@ loc)) = do
    putVal loc (Text $ show $ typeOf e) >>= getVal

  eval (StrExpr (StrFormat LaTeX e :@ loc)) = do
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

  eval (StrExpr (StrIntCast k :@ loc)) = do
    n <- eval k >>= getVal :: EvalM Integer
    putVal loc (Text $ show n) >>= getVal
