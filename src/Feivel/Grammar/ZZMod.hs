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


data ZZModExprLeaf a zzmod
  = ZZModConst Type ZZModulo
  | ZZModVar   Type Key
  | ZZModCast  Type a

  | ZZModMacro Type [(Type, Key, a)] a -- Expr, MacTo ZZModulo
  | ZZModAtPos Type a a   -- ListOf ZZModulo, ZZ
  | ZZModAtIdx Type a a a -- MatOf ZZModulo, ZZ, ZZ

  | ZZModIfThenElse Type a zzmod zzmod -- BB
 
  -- Arithmetic
  | ZZModNeg   Type zzmod
  | ZZModInv   Type zzmod
 
  | ZZModAdd   Type zzmod zzmod
  | ZZModSub   Type zzmod zzmod
  | ZZModMult  Type zzmod zzmod
  | ZZModPow   Type zzmod a -- ZZ

  | ZZModSum   Type a -- ListOf ZZModulo
  | ZZModProd  Type a -- ListOf ZZModulo
  deriving (Eq, Show)



instance Typed (ZZModExprLeaf a mod) where
  typeOf x = case x of
    ZZModConst      typ _     -> typ
    ZZModVar        typ _     -> typ
    ZZModAtPos      typ _ _   -> typ
    ZZModAtIdx      typ _ _ _ -> typ
    ZZModMacro      typ _ _   -> typ
    ZZModIfThenElse typ _ _ _ -> typ
    ZZModCast       typ _     -> typ
    ZZModNeg        typ _     -> typ
    ZZModInv        typ _     -> typ
    ZZModAdd        typ _ _   -> typ
    ZZModSub        typ _ _   -> typ
    ZZModMult       typ _ _   -> typ
    ZZModPow        typ _ _   -> typ
    ZZModSum        typ _     -> typ
    ZZModProd       typ _     -> typ
