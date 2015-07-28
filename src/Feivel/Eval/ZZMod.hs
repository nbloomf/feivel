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

module Feivel.Eval.ZZMod () where

import Feivel.Eval.Util


instance (Glyph Expr) => Glyph ZZModExpr where
  toGlyph (ZZModExpr (ZZModConst _ a :@ _)) = return $ showZZMod a
  toGlyph x = error $ "toGlyph: ZZModExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr) => Eval ZZModExpr where
  eval (ZZModExpr (ZZModConst n a :@ loc)) = return $ ZZModExpr $ ZZModConst n a :@ loc

  {- :Common -}
  eval (ZZModExpr (ZZModVar _ key :@ loc))        = eKey key loc
  eval (ZZModExpr (ZZModAtIdx _ m h k :@ loc))    = eAtIdx m h k loc
  eval (ZZModExpr (ZZModMacro _ vals mac :@ loc)) = eMacro vals mac loc
  eval (ZZModExpr (ZZModIfThenElse _ b t f :@ _)) = eIfThenElse b t f

  eval (ZZModExpr (ZZModAtPos _ a t :@ loc)) = lift2 loc a t (foo)
    where foo = listAtPos :: [ZZModExpr] -> Integer -> Either ListErr ZZModExpr

  eval (ZZModExpr (ZZModCast (ZZMod n) a :@ loc)) = do
    res <- eval a >>= getVal :: EvalM Integer
    putTypeVal (ZZMod n) loc (res `zzmod` n) >>= getVal

  eval (ZZModExpr (ZZModNeg  _ a   :@ loc)) = lift1 loc a   (rNegT (0 `zzmod` 0))
  eval (ZZModExpr (ZZModInv  _ a   :@ loc)) = lift1 loc a   (rInvT (0 `zzmod` 0))
  eval (ZZModExpr (ZZModAdd  _ a b :@ loc)) = lift2 loc a b (rAddT (0 `zzmod` 0))
  eval (ZZModExpr (ZZModSub  _ a b :@ loc)) = lift2 loc a b (rSubT (0 `zzmod` 0))
  eval (ZZModExpr (ZZModMult _ a b :@ loc)) = lift2 loc a b (rMulT (0 `zzmod` 0))
  eval (ZZModExpr (ZZModPow  _ a b :@ loc)) = lift2 loc a b (rPowT (0 `zzmod` 0))

  eval (ZZModExpr (ZZModSum   _ ls :@ loc)) = lift1 loc ls (rSumT   (0 `zzmod` 0))
  eval (ZZModExpr (ZZModProd  _ ls :@ loc)) = lift1 loc ls (rUProdT (0 `zzmod` 0))

