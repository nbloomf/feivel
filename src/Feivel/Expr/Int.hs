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

module Feivel.Expr.Int where

import Feivel.Expr.Util


type IntExpr a = AtLocus (IntExprLeaf a)

data IntExprLeaf a
  = IntConst Integer
  | IntVar   Key

  | IntMacro [(Type, Key, a)] a -- MacTo ZZ
  | IntAtPos a a   -- ListOf ZZ, ZZ
  | IntAtIdx a a a -- MatOf ZZ, ZZ, ZZ
 
  | IntIfThenElse a (IntExpr a) (IntExpr a) -- BB
 
  -- Arithmetic
  | IntAdd    (IntExpr a) (IntExpr a)
  | IntSub    (IntExpr a) (IntExpr a)
  | IntMult   (IntExpr a) (IntExpr a)
  | IntQuo    (IntExpr a) (IntExpr a)
  | IntMod    (IntExpr a) (IntExpr a)
  | IntPow    (IntExpr a) (IntExpr a)
  | IntGCD    (IntExpr a) (IntExpr a)
  | IntLCM    (IntExpr a) (IntExpr a)
  | IntMin    (IntExpr a) (IntExpr a)
  | IntMax    (IntExpr a) (IntExpr a)
  | IntChoose (IntExpr a) (IntExpr a)
 
  | IntNeg        (IntExpr a)
  | IntAbs        (IntExpr a)
  | IntRad        (IntExpr a)
  | IntSqPart     (IntExpr a)
  | IntSqFreePart (IntExpr a)

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
  | IntObserveUniform  (IntExpr a) (IntExpr a)
  | IntObserveBinomial (IntExpr a) a    -- QQ
  | IntObservePoisson  a            -- QQ

  -- Casts
  | IntCastStr a -- SS
  deriving (Eq, Show)
