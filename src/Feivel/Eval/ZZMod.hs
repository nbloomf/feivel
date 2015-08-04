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

module Feivel.Eval.ZZMod () where

import Feivel.Eval.Util


instance (Glyph Expr) => Glyph ZZModExpr where
  toGlyph (ZZModExpr (ZZModConst a :# _ :@ _)) = return $ showZZMod a
  toGlyph x = error $ "toGlyph: ZZModExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr) => Eval ZZModExpr where
  eval (ZZModExpr (m :# (ZZMod n) :@ loc)) = case m of
    ZZModConst a -> return $ ZZModExpr $ ZZModConst a :# (ZZMod n) :@ loc

    {- :Common -}
    ZZModVar        key      -> eKey key loc
    ZZModAtIdx      m h k    -> eAtIdx m h k loc
    ZZModMacro      vals mac -> eMacro vals mac loc
    ZZModIfThenElse b t f    -> eIfThenElse b t f

    ZZModAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [ZZModExpr] -> Integer -> Either ListErr ZZModExpr

    ZZModCast a -> do
      res <- eval a >>= getVal :: EvalM Integer
      putTypeVal (ZZMod n) loc (res `zzmod` n) >>= getVal

    ZZModNeg  a   -> lift1 loc a   (rNegT (0 `zzmod` 0))
    ZZModInv  a   -> lift1 loc a   (rInvT (0 `zzmod` 0))
    ZZModAdd  a b -> lift2 loc a b (rAddT (0 `zzmod` 0))
    ZZModSub  a b -> lift2 loc a b (rSubT (0 `zzmod` 0))
    ZZModMult a b -> lift2 loc a b (rMulT (0 `zzmod` 0))
    ZZModPow  a b -> lift2 loc a b (rPowT (0 `zzmod` 0))

    ZZModSum  ls  -> lift1 loc ls (rSumT   (0 `zzmod` 0))
    ZZModProd ls  -> lift1 loc ls (rUProdT (0 `zzmod` 0))
