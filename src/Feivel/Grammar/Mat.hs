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

module Feivel.Grammar.Mat where

import Feivel.Grammar.Util


data MatExprLeaf a bool int list mat
  = MatConst Type (Matrix a)
  | MatVar   Type Key

  | MatMacro Type [(Type, Key, a)] a -- Expr, MacTo (MatOf typ)
  | MatAtPos Type list int
  | MatAtIdx Type a int int -- MatOf (MatOf typ)

  | MatIfThenElse Type bool mat mat

  | MatBuilder Type a Key list Key list -- typ

  | MatRowFromList Type list
  | MatColFromList Type list

  -- Special Values
  | MatId     Type int
  | MatSwapE  Type int int int
  | MatScaleE Type int int a -- typ
  | MatAddE   Type int int int a -- ZZ, ZZ, ZZ, typ

  -- Arithmetic
  | MatHCat  Type mat mat
  | MatVCat  Type mat mat
  | MatAdd   Type mat mat
  | MatMul   Type mat mat
  | MatPow   Type mat int
  | MatNeg   Type mat
  | MatTrans Type mat

  -- Mutation
  | MatSwapRows Type mat int int
  | MatSwapCols Type mat int int
  | MatScaleRow Type mat a int -- typ
  | MatScaleCol Type mat a int -- typ
  | MatAddRow   Type mat a int int -- typ
  | MatAddCol   Type mat a int int -- typ
  | MatDelRow   Type mat int
  | MatDelCol   Type mat int

  | MatGetRow   Type int mat
  | MatGetCol   Type int mat

  -- Randomness
  | MatShuffleRows Type mat
  | MatShuffleCols Type mat

  -- Factorizations
  | MatGJForm   Type mat 
  | MatGJFactor Type mat

  | MatRand Type list
  deriving (Eq, Show)



instance Typed (MatExprLeaf a bool int list mat) where
  typeOf x = case x of
    MatVar         typ _         -> MatOf typ
    MatMacro       typ _ _       -> MatOf typ
    MatAtPos       typ _ _       -> MatOf typ
    MatAtIdx       typ _ _ _     -> MatOf typ
    MatIfThenElse  typ _ _ _     -> MatOf typ
    MatConst       typ _         -> MatOf typ
    MatId          typ _         -> MatOf typ
    MatSwapE       typ _ _ _     -> MatOf typ
    MatScaleE      typ _ _ _     -> MatOf typ
    MatAddE        typ _ _ _ _   -> MatOf typ
    MatHCat        typ _ _       -> MatOf typ
    MatVCat        typ _ _       -> MatOf typ
    MatAdd         typ _ _       -> MatOf typ
    MatMul         typ _ _       -> MatOf typ
    MatPow         typ _ _       -> MatOf typ
    MatTrans       typ _         -> MatOf typ
    MatNeg         typ _         -> MatOf typ
    MatSwapRows    typ _ _ _     -> MatOf typ
    MatSwapCols    typ _ _ _     -> MatOf typ
    MatScaleRow    typ _ _ _     -> MatOf typ
    MatScaleCol    typ _ _ _     -> MatOf typ
    MatAddRow      typ _ _ _ _   -> MatOf typ
    MatAddCol      typ _ _ _ _   -> MatOf typ
    MatDelRow      typ _ _       -> MatOf typ
    MatDelCol      typ _ _       -> MatOf typ
    MatShuffleRows typ _         -> MatOf typ
    MatShuffleCols typ _         -> MatOf typ
    MatGJForm      typ _         -> MatOf typ
    MatGJFactor    typ _         -> MatOf typ
    MatGetRow      typ _ _       -> MatOf typ
    MatGetCol      typ _ _       -> MatOf typ
    MatRowFromList typ _         -> MatOf typ
    MatColFromList typ _         -> MatOf typ
    MatRand        typ _         -> MatOf typ
    MatBuilder     typ _ _ _ _ _ -> MatOf typ


