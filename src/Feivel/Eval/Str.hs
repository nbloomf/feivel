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

module Feivel.Eval.Str where

import Feivel.Eval.Util


instance (Glyph Expr) => Glyph (StrExpr Expr) where
  toGlyph (StrConst (Text s) :@ _) = return s
  toGlyph x = error $ "toGlyph: StrExpr: " ++ show x


instance (Eval Expr, Glyph Expr) => Eval (StrExpr Expr) where
  eval (StrConst s :@ loc) = return (StrConst s :@ loc)

  {- :Common -}
  eval (StrVar key :@ loc)        = eKey key loc
  eval (StrAtIdx m h k :@ loc)    = eAtIdx m h k loc
  eval (StrIfThenElse b t f :@ _) = eIfThenElse b t f
  eval (StrMacro vals mac :@ loc) = eMacro vals mac loc

  eval (StrAtPos a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [StrExpr Expr] -> Integer -> Either ListErr (StrExpr Expr)

  eval (Concat   a b :@ loc) = lift2 loc a b (strCat)
  eval (StrStrip a b :@ loc) = lift2 loc a b (strStrip)

  eval (ToUpper   a :@ loc) = lift1 loc a (strUpper)
  eval (ToLower   a :@ loc) = lift1 loc a (strLower)
  eval (Reverse   a :@ loc) = lift1 loc a (strRev)
  eval (Rot13     a :@ loc) = lift1 loc a (strRot13)
  eval (StrHex    n :@ loc) = lift1 loc n (strHex)
  eval (StrRoman  n :@ loc) = lift1 loc n (strRoman)
  eval (StrBase36 n :@ loc) = lift1 loc n (strBase36)

  eval (StrDecimal p k :@ loc) = do
    x <- eval p >>= getVal
    d <- eval k >>= getVal
    putVal loc (Text $ digits d x) >>= getVal

  eval (StrRand ls :@ loc) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs :: EvalM Text
    putVal loc r >>= getVal

  eval (StrTab m :@ loc) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    tab <- tabulateWithM toGlyph n
    putVal loc (Text tab) >>= getVal

  eval (StrTypeOf e :@ loc) = do
    putVal loc (Text $ show $ typeOf e) >>= getVal

  eval (StrFormat LaTeX e :@ loc) = do
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

  eval (StrIntCast n :@ loc) = do
    n <- eval n >>= getVal :: EvalM Integer
    putVal loc (Text $ show n) >>= getVal
