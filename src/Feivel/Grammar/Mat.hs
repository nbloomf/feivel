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

module Feivel.Grammar.Mat where

import Feivel.Grammar.Util


newtype MatExpr a = MatExpr
  { unMatExpr :: AtLocus (MatExprLeaf a)
  } deriving (Eq, Show)

instance HasLocus (MatExpr a) where
  locusOf = locusOf . unMatExpr


data MatExprLeaf a
  = MatConst Type (Matrix a)
  | MatVar   Type Key

  | MatMacro Type [(Type, Key, a)] a -- Expr, MacTo (MatOf typ)
  | MatAtPos Type a a -- ListOf (MatOf typ), ZZ
  | MatAtIdx Type a a a -- MatOf (MatOf typ), ZZ, ZZ

  | MatIfThenElse Type a (MatExpr a) (MatExpr a) -- BB

  | MatBuilder Type a Key a Key a -- typ, ListOf XX, ListOf XX

  | MatRowFromList Type a -- ListOf typ
  | MatColFromList Type a -- ListOf typ

  -- Special Values
  | MatId     Type a -- ZZ
  | MatSwapE  Type a a a -- ZZ, ZZ, ZZ
  | MatScaleE Type a a a -- ZZ, ZZ, typ
  | MatAddE   Type a a a a -- ZZ, ZZ, ZZ, typ

  -- Arithmetic
  | MatHCat  Type (MatExpr a) (MatExpr a)
  | MatVCat  Type (MatExpr a) (MatExpr a)
  | MatAdd   Type (MatExpr a) (MatExpr a)
  | MatMul   Type (MatExpr a) (MatExpr a)
  | MatPow   Type (MatExpr a) a -- ZZ
  | MatNeg   Type (MatExpr a)
  | MatTrans Type (MatExpr a)

  -- Mutation
  | MatSwapRows Type (MatExpr a) a a -- ZZ, ZZ
  | MatSwapCols Type (MatExpr a) a a -- ZZ, ZZ
  | MatScaleRow Type (MatExpr a) a a -- typ, ZZ
  | MatScaleCol Type (MatExpr a) a a -- typ, ZZ
  | MatAddRow   Type (MatExpr a) a a a -- typ, ZZ, ZZ
  | MatAddCol   Type (MatExpr a) a a a -- typ, ZZ, ZZ
  | MatDelRow   Type (MatExpr a) a -- ZZ
  | MatDelCol   Type (MatExpr a) a -- ZZ

  | MatGetRow   Type a (MatExpr a) -- ZZ
  | MatGetCol   Type a (MatExpr a) -- ZZ

  -- Randomness
  | MatShuffleRows Type (MatExpr a)
  | MatShuffleCols Type (MatExpr a)

  -- Factorizations
  | MatGJForm   Type (MatExpr a) 
  | MatGJFactor Type (MatExpr a)

  | MatRand Type a -- ListOf (MatOf typ)
  deriving (Eq, Show)



instance Typed (MatExpr a) where
  typeOf (MatExpr (x :@ _)) = case x of
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


