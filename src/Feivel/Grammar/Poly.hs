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

module Feivel.Grammar.Poly where

import Feivel.Grammar.Util


data PolyExprLeaf a bool int list mat poly tup
  = PolyConst (Poly VarString a)
  | PolyVar   Key

  | PolyMacro [(Type, Key, a)] a -- MacTo (PolyOver typ)
  | PolyAtPos  list int
  | PolyAtIdx  mat  int int
  | PolyAtSlot tup  int

  | PolyRand list

  | PolyIfThenElse bool poly poly

  | PolyAdd poly poly
  | PolySub poly poly
  | PolyMul poly poly
  | PolyQuo poly poly
  | PolyRem poly poly
  | PolyPow poly int
  | PolyNeg poly

  | PolySum list

  | PolyFromRoots (Variable VarString) list
  | PolyFromCoefs (Variable VarString) list
  | PolyEvalPoly  poly [(Variable VarString, poly)]
  deriving (Eq, Show)
