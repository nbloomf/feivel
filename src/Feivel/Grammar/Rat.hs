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


data RatExprLeaf a bool int list mat rat
  = RatConst Rat
  | RatVar   Key
  | RatCast  a -- ZZ

  | RatMacro [(Type, Key, a)] a -- Expr, MacTo QQ
  | RatAtPos list int
  | RatAtIdx mat  int int

  | RatIfThenElse bool rat rat -- BB
 
  -- Arithmetic
  | RatNeg   rat
  | RatAbs   rat
 
  | RatAdd   rat rat
  | RatSub   rat rat
  | RatMult  rat rat
  | RatQuot  rat rat
  | RatMin   rat rat
  | RatMax   rat rat

  | RatPow   rat int

  -- List
  | RatRand  list
  | RatSum   list
  | RatProd  list
  | RatMaxim list
  | RatMinim list

  -- Stats
  | RatMean    list
  | RatMeanDev list
  | RatStdDev  list int
  | RatZScore  rat list int

  -- Approximations
  | RatSqrt  rat int

  -- Casting
  | RatCastStr a -- SS
  deriving (Eq, Show)
