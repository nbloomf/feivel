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

module Feivel.Grammar.Mat where

import Feivel.Grammar.Util


data MatExprLeaf a bool int list mat tup
  = MatConst (Matrix a)
  | MatVar   Key

  | MatMacro [(Type, Key, a)] a -- Expr, MacTo (MatOf typ)
  | MatAtPos  list int
  | MatAtIdx  a    int int -- MatOf (MatOf typ)
  | MatAtSlot tup  int

  | MatIfThenElse bool mat mat

  | MatBuilder a Key list Key list -- typ

  | MatRowFromList list
  | MatColFromList list

  -- Special Values
  | MatId     int
  | MatSwapE  int int int
  | MatScaleE int int a -- typ
  | MatAddE   int int int a -- typ

  -- Arithmetic
  | MatHCat  mat mat
  | MatVCat  mat mat
  | MatAdd   mat mat
  | MatMul   mat mat
  | MatPow   mat int
  | MatNeg   mat
  | MatTrans mat

  -- Mutation
  | MatSwapRows mat int int
  | MatSwapCols mat int int
  | MatScaleRow mat a int -- typ
  | MatScaleCol mat a int -- typ
  | MatAddRow   mat a int int -- typ
  | MatAddCol   mat a int int -- typ
  | MatDelRow   mat int
  | MatDelCol   mat int

  | MatGetRow   int mat
  | MatGetCol   int mat

  -- Randomness
  | MatShuffleRows mat
  | MatShuffleCols mat

  -- Factorizations
  | MatGJForm   mat 
  | MatGJFactor mat

  | MatRand list
  deriving (Eq, Show)
