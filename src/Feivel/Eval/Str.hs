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
    return $ StrConst (Text $ digits d x) :@ loc

  eval (StrRand ls :@ loc) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs
    return $ StrConst r :@ loc

  eval (StrTab m :@ loc) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    tab <- tabulateWithM toGlyph n
    return $ StrConst (Text tab) :@ loc

  eval (StrTypeOf e :@ loc) = do
    return $ StrConst (Text $ show $ typeOf e) :@ loc

  eval (StrFormat LaTeX e :@ loc) = do
    case typeOf e of
      ZZ -> do
        x <- eval e >>= getVal :: EvalM Integer
        return $ StrConst (Text $ latex x) :@ loc
      QQ -> do
        x <- eval e >>= getVal :: EvalM Rat
        return $ StrConst (Text $ latex x) :@ loc
      MatOf ZZ -> do
        x <- eval e >>= getVal :: EvalM (Matrix Integer)
        return $ StrConst (Text $ latex x) :@ loc
      MatOf QQ -> do
        x <- eval e >>= getVal :: EvalM (Matrix Rat)
        return $ StrConst (Text $ latex x) :@ loc
      PolyOver ZZ -> do
        x <- eval e >>= getVal :: EvalM (Poly Integer)
        return $ StrConst (Text $ latex x) :@ loc
      PolyOver QQ -> do
        x <- eval e >>= getVal :: EvalM (Poly Rat)
        return $ StrConst (Text $ latex x) :@ loc
      _ -> error "StrFormat LaTeX"

  eval (StrIntCast n :@ loc) = do
    n <- eval n >>= getVal :: EvalM Integer
    return $ StrConst (Text $ show n) :@ loc