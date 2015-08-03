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

module Feivel.Eval.Perm () where

import Feivel.Eval.Util


instance (Glyph Expr) => Glyph PermExpr where
  toGlyph (PermExpr (PermConst _ px :@ _)) = do
    qx <- seqPerm $ mapPerm toGlyph px
    return $ showPerm qx
  toGlyph x = error $ "toGlyph: PermExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr) => Eval PermExpr where
  eval (PermExpr (m :@ loc)) = case m of
    PermConst t p -> do
      q <- seqPerm $ mapPerm eval p
      putTypeVal t loc q >>= getVal

    PermAtPos _ a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [PermExpr] -> Integer -> Either ListErr PermExpr

    {- Common -}
    PermVar        _ key      -> eKey key loc
    PermAtIdx      _ m h k    -> eAtIdx m h k loc
    PermMacro      _ vals mac -> eMacro vals mac loc
    PermIfThenElse _ b t f    -> eIfThenElse b t f

    PermRand _ ls -> do
      xs <- eval ls >>= getVal :: EvalM [Expr]
      r  <- randomElementEvalM xs
      eval r >>= getVal

    PermCompose t p q -> do
      let comp z = lift2 loc p q (gOpT (idOn z))
      case t of
        ZZ -> comp zeroZZ
        QQ -> comp zeroQQ
        BB -> comp zeroBB
        _ -> error "Eval: PermCompose"

    PermInvert t p -> do
      let inver z = lift1 loc p (gInvT (idOn z))
      case t of
        ZZ -> inver zeroZZ
        QQ -> inver zeroQQ
        BB -> inver zeroBB
        _ -> error "Eval: PermCompose"
