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

module Feivel.Grammar.Int where

import Feivel.Grammar.Util


data IntExprLeaf a bool int
  = IntConst Integer
  | IntVar   Key

  | IntMacro [(Type, Key, a)] a -- MacTo ZZ
  | IntAtPos a int   -- ListOf ZZ
  | IntAtIdx a int int -- MatOf ZZ
 
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
  | StrLength a -- SS

  -- Rational
  | RatNumer a -- QQ
  | RatDenom a -- QQ
  | RatFloor a -- QQ

  -- List
  | ListLen  a -- ListOf XX
  | IntRand  a -- ListOf ZZ
  | IntSum   a -- ListOf ZZ
  | IntProd  a -- ListOf ZZ
  | IntMaxim a -- ListOf ZZ
  | IntMinim a -- ListOf ZZ
  | IntGCDiv a -- ListOf ZZ
  | IntLCMul a -- ListOf ZZ

  -- Matrix
  | MatNumRows a -- MatOf XX
  | MatNumCols a -- MatOf XX
  | MatRank    a -- MatOf XX

  -- Polynomial
  | IntContent a -- PolyOver ZZ

  -- Stats
  | IntObserveUniform  int int
  | IntObserveBinomial int a    -- QQ
  | IntObservePoisson  a            -- QQ

  -- Casts
  | IntCastStr a -- SS
  deriving (Eq, Show)
