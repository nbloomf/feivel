{---------------------------------------------------------------------}
{- Copyright 2015, 2016 Nathan Bloomfield                            -}
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

module Feivel.Grammar.Int where

import Feivel.Grammar.Util


data IntExprLeaf
      int  str bool rat  mod
      list mat tup  poly mac
      a
  = IntConst Integer
  | IntVar   Key

  | IntMacro  [(Type, Key, a)] a -- Expr, MacTo ZZ
  | IntAtPos  list int
  | IntAtIdx  mat  int int
  | IntAtSlot tup  int

  | IntIfThenElse bool int int

  -- Arithmetic
  | IntAdd    int int
  | IntSub    int int
  | IntMult   int int
  | IntQuo    int int
  | IntMod    int int
  | IntPow    int int
  | IntGCD    int int
  | IntLCM    int int
  | IntMin    int int
  | IntMax    int int
  | IntChoose int int
 
  | IntNeg        int
  | IntAbs        int
  | IntRad        int
  | IntSqPart     int
  | IntSqFreePart int

  -- String
  | StrLength str

  -- Rational
  | RatNumer rat
  | RatDenom rat
  | RatFloor rat

  -- List
  | ListLen  list
  | IntRand  list
  | IntSum   list
  | IntProd  list
  | IntMaxim list
  | IntMinim list
  | IntGCDiv list
  | IntLCMul list

  -- Matrix
  | MatNumRows mat
  | MatNumCols mat
  | MatRank    mat

  -- Polynomial
  | IntContent poly
  | PolyDegree Type poly

  -- Stats
  | IntObserveUniform  int int
  | IntObserveBinomial int rat
  | IntObservePoisson  rat

  -- Casts
  | IntCastStr str
  deriving (Eq, Show)
