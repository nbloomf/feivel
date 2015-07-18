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

module Feivel.Expr.Rat where

import Feivel.Expr.Util

{------------}
{- :RatExpr -}
{------------}

type RatExpr a = AtLocus (RatExprLeaf a)

data RatExprLeaf a
  = RatConst Rat
  | RatVar   Key
  | RatCast  a -- ZZ

  | RatMacro [(Type, Key, a)] a -- Expr, MacTo QQ
  | RatAtPos a a -- ListOf QQ, ZZ
  | RatAtIdx a a a -- MatOf QQ, ZZ, ZZ

  | RatIfThenElse a (RatExpr a) (RatExpr a) -- BB
 
  -- Arithmetic
  | RatNeg   (RatExpr a)
  | RatAbs   (RatExpr a)
 
  | RatAdd   (RatExpr a) (RatExpr a)
  | RatSub   (RatExpr a) (RatExpr a)
  | RatMult  (RatExpr a) (RatExpr a)
  | RatQuot  (RatExpr a) (RatExpr a)
  | RatMin   (RatExpr a) (RatExpr a)
  | RatMax   (RatExpr a) (RatExpr a)

  | RatPow   (RatExpr a) a -- ZZ

  -- List
  | RatRand  a -- ListOf QQ
  | RatSum   a -- ListOf QQ
  | RatProd  a -- ListOf QQ
  | RatMaxim a -- ListOf QQ
  | RatMinim a -- ListOf QQ

  -- Stats
  | RatMean    a -- ListOf XX
  | RatMeanDev a -- ListOf XX
  | RatStdDev  a a -- ListOf XX, ZZ
  | RatZScore  (RatExpr a) a a -- ListOf XX, ZZ

  -- Approximations
  | RatSqrt  (RatExpr a) a -- ZZ

  -- Casting
  | RatCastStr a -- SS
  deriving (Eq, Show)