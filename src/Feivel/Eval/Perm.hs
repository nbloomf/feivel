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

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feivel.Eval.Perm () where

import Feivel.Eval.Util
import Carl.List


instance (Glyph Expr) => Glyph PermExpr where
  toGlyph (PermExpr (PermConst px :# _ :@ _)) = do
    qx <- seqPerm $ mapPerm toGlyph px
    return $ showPerm qx
  toGlyph x = error $ "toGlyph: PermExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr, Eval ListExpr, Eval MatExpr, Eval TupleExpr) => Eval PermExpr where
  eval (PermExpr (m :# typ :@ loc)) = case m of
    PermConst p -> do
      q <- seqPerm $ mapPerm eval p
      putTypeVal typ loc q >>= getVal

    PermAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [PermExpr] -> Integer -> Either ListErr PermExpr

    {- Common -}
    PermVar        key      -> eKey key loc
    PermAtIdx      m h k    -> eAtIdx m h k loc
    PermMacro      vals mac -> eMacro vals mac loc
    PermIfThenElse b t f    -> eIfThenElse b t f

    PermAtSlot t i -> do
      x <- eval t >>= getVal :: EvalM (Tuple Expr)
      n <- eval i >>= getVal :: EvalM Integer
      k <- tryEvalM loc $ project x n
      putVal loc k >>= getVal

    PermRand ls -> do
      xs <- eval ls >>= getVal :: EvalM [Expr]
      r  <- randomElementEvalM xs
      eval r >>= getVal

    PermCompose p q -> do
      let comp z = lift2 loc p q (gOpT (idOn z))
      case typ of
        ZZ -> comp zeroZZ
        QQ -> comp zeroQQ
        BB -> comp zeroBB
        _ -> error "Eval: PermCompose"

    PermInvert p -> do
      let inver z = lift1 loc p (gInvT (idOn z))
      case typ of
        ZZ -> inver zeroZZ
        QQ -> inver zeroQQ
        BB -> inver zeroBB
        _ -> error "Eval: PermCompose"
