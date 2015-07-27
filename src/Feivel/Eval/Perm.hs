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

module Feivel.Eval.Perm () where

import Feivel.Eval.Util


instance (Glyph Expr) => Glyph PermExpr where
  toGlyph (PermExpr (PermConst _ px :@ _)) = do
    qx <- seqPerm $ mapPerm toGlyph px
    return $ showPerm qx
  toGlyph x = error $ "toGlyph: PermExpr: " ++ show x


instance (Eval Expr) => Eval PermExpr where
  eval (PermExpr (PermConst t p :@ loc)) = do
    q <- seqPerm $ mapPerm eval p
    putTypeVal t loc q >>= getVal

  eval (PermExpr (PermAtPos _ a t :@ loc)) = lift2 loc a t (foo)
    where foo = listAtPos :: [PermExpr] -> Integer -> Either ListErr PermExpr

  {- Common -}
  eval (PermExpr (PermVar _ key :@ loc))        = eKey key loc
  eval (PermExpr (PermAtIdx _ m h k :@ loc))    = eAtIdx m h k loc
  eval (PermExpr (PermMacro _ vals mac :@ loc)) = eMacro vals mac loc
  eval (PermExpr (PermIfThenElse _ b t f :@ _)) = eIfThenElse b t f

  eval (PermExpr (PermRand _ ls :@ loc)) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    eval r >>= getVal

  eval (PermExpr (PermCompose t p q :@ loc)) = do
    case t of
      ZZ -> do
        a <- eval p >>= getVal :: EvalM (Perm Integer)
        b <- eval q >>= getVal :: EvalM (Perm Integer)
        let c = compose a b
        let s = mapPerm (put loc) c
        putTypeVal ZZ loc s >>= getVal
      _ -> reportErr loc $ PolynomialExpected t

  eval (PermExpr (PermInvert t p :@ loc)) = do
    case t of
      ZZ -> do
        a <- eval p >>= getVal :: EvalM (Perm Integer)
        let c = inverse a
        let s = mapPerm (put loc) c
        putTypeVal ZZ loc s >>= getVal
      _ -> reportErr loc $ PolynomialExpected t
