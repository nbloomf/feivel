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

module Feivel.Expr.ZZMod where

import Feivel.Expr.Util


type ZZModExpr a = AtLocus (ZZModExprLeaf a)

data ZZModExprLeaf a
  = ZZModConst Type ZZModulo
  | ZZModVar   Type Key
  | ZZModCast  Type a

  | ZZModMacro Type [(Type, Key, a)] a -- Expr, MacTo ZZModulo
  | ZZModAtPos Type a a   -- ListOf ZZModulo, ZZ
  | ZZModAtIdx Type a a a -- MatOf ZZModulo, ZZ, ZZ

  | ZZModIfThenElse Type a (ZZModExpr a) (ZZModExpr a) -- BB
 
  -- Arithmetic
  | ZZModNeg   Type (ZZModExpr a)
  | ZZModInv   Type (ZZModExpr a)
 
  | ZZModAdd   Type (ZZModExpr a) (ZZModExpr a)
  | ZZModSub   Type (ZZModExpr a) (ZZModExpr a)
  | ZZModMult  Type (ZZModExpr a) (ZZModExpr a)
  | ZZModPow   Type (ZZModExpr a) a -- ZZ

  | ZZModSum   Type a -- ListOf ZZModulo
  | ZZModProd  Type a -- ListOf ZZModulo
  deriving (Eq, Show)
