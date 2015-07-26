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

module Feivel.Grammar.ZZMod where

import Feivel.Grammar.Util


newtype ZZModExpr a = ZZModExpr
  { unZZModExpr :: AtLocus (ZZModExprLeaf a)
  } deriving (Eq, Show)

instance HasLocus (ZZModExpr a) where
  locusOf = locusOf . unZZModExpr


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



instance Typed (ZZModExpr a) where
  typeOf (ZZModExpr x) = case x of
    ZZModConst      typ _     :@ _ -> typ
    ZZModVar        typ _     :@ _ -> typ
    ZZModAtPos      typ _ _   :@ _ -> typ
    ZZModAtIdx      typ _ _ _ :@ _ -> typ
    ZZModMacro      typ _ _   :@ _ -> typ
    ZZModIfThenElse typ _ _ _ :@ _ -> typ
    ZZModCast       typ _     :@ _ -> typ
    ZZModNeg        typ _     :@ _ -> typ
    ZZModInv        typ _     :@ _ -> typ
    ZZModAdd        typ _ _   :@ _ -> typ
    ZZModSub        typ _ _   :@ _ -> typ
    ZZModMult       typ _ _   :@ _ -> typ
    ZZModPow        typ _ _   :@ _ -> typ
    ZZModSum        typ _     :@ _ -> typ
    ZZModProd       typ _     :@ _ -> typ
