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

module Feivel.Eval.ZZMod () where

import Feivel.Eval.Util


instance (Glyph Expr) => Glyph (ZZModExpr Expr) where
  toGlyph (ZZModConst _ a :@ _) = return $ showZZMod a
  toGlyph x = error $ "toGlyph: ZZModExpr: " ++ show x


instance (Eval Expr) => Eval (ZZModExpr Expr) where
  eval (ZZModConst n a :@ loc) = return $ ZZModConst n a :@ loc

  {- :Common -}
  eval (ZZModVar _ key :@ loc)        = eKey key loc
  eval (ZZModAtIdx _ m h k :@ loc)    = eAtIdx m h k loc
  eval (ZZModMacro _ vals mac :@ loc) = eMacro vals mac loc
  eval (ZZModIfThenElse _ b t f :@ _) = eIfThenElse b t f

  eval (ZZModAtPos _ a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [ZZModExpr Expr] -> Integer -> Either ListErr (ZZModExpr Expr)

  eval (ZZModCast (ZZMod n) a :@ loc) = do
    res <- eval a >>= getVal :: EvalM Integer
    return (ZZModConst (ZZMod n) (res `zzmod` n) :@ loc)

  eval (ZZModNeg  _ a   :@ loc) = lift1 loc a   (rNegT (0 `zzmod` 0))
  eval (ZZModInv  _ a   :@ loc) = lift1 loc a   (rInvT (0 `zzmod` 0))
  eval (ZZModAdd  _ a b :@ loc) = lift2 loc a b (rAddT (0 `zzmod` 0))
  eval (ZZModSub  _ a b :@ loc) = lift2 loc a b (rSubT (0 `zzmod` 0))
  eval (ZZModMult _ a b :@ loc) = lift2 loc a b (rMulT (0 `zzmod` 0))
  eval (ZZModPow  _ a b :@ loc) = lift2 loc a b (rPowT (0 `zzmod` 0))

  eval (ZZModSum   _ ls :@ loc) = lift1 loc ls (rSumT   (0 `zzmod` 0))
  eval (ZZModProd  _ ls :@ loc) = lift1 loc ls (rUProdT (0 `zzmod` 0))

