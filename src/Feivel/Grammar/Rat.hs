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

module Feivel.Grammar.Rat where

import Feivel.Grammar.Util


data RatExprLeaf a int rat
  = RatConst Rat
  | RatVar   Key
  | RatCast  a -- ZZ

  | RatMacro [(Type, Key, a)] a -- Expr, MacTo QQ
  | RatAtPos a int -- ListOf QQ
  | RatAtIdx a int int -- MatOf QQ

  | RatIfThenElse a rat rat -- BB
 
  -- Arithmetic
  | RatNeg   rat
  | RatAbs   rat
 
  | RatAdd   rat rat
  | RatSub   rat rat
  | RatMult  rat rat
  | RatQuot  rat rat
  | RatMin   rat rat
  | RatMax   rat rat

  | RatPow   rat int -- ZZ

  -- List
  | RatRand  a -- ListOf QQ
  | RatSum   a -- ListOf QQ
  | RatProd  a -- ListOf QQ
  | RatMaxim a -- ListOf QQ
  | RatMinim a -- ListOf QQ

  -- Stats
  | RatMean    a -- ListOf XX
  | RatMeanDev a -- ListOf XX
  | RatStdDev  a int -- ListOf XX, ZZ
  | RatZScore  rat a int -- ListOf XX, ZZ

  -- Approximations
  | RatSqrt  rat int -- ZZ

  -- Casting
  | RatCastStr a -- SS
  deriving (Eq, Show)
