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

module Feivel.Grammar.ZZMod where

import Feivel.Grammar.Util


data ZZModExprLeaf a bool int zzmod
  = ZZModConst ZZModulo
  | ZZModVar   Key
  | ZZModCast  a

  | ZZModMacro [(Type, Key, a)] a -- Expr, MacTo ZZModulo
  | ZZModAtPos a int   -- ListOf ZZModulo
  | ZZModAtIdx a int int -- MatOf ZZModulo

  | ZZModIfThenElse bool zzmod zzmod

  -- Arithmetic
  | ZZModNeg   zzmod
  | ZZModInv   zzmod

  | ZZModAdd   zzmod zzmod
  | ZZModSub   zzmod zzmod
  | ZZModMult  zzmod zzmod
  | ZZModPow   zzmod int

  | ZZModSum   a -- ListOf ZZModulo
  | ZZModProd  a -- ListOf ZZModulo
  deriving (Eq, Show)
