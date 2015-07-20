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


type MatExpr a = AtLocus (MatExprLeaf a)

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
  typeOf (MatVar         typ _         :@ _) = MatOf typ
  typeOf (MatMacro       typ _ _       :@ _) = MatOf typ
  typeOf (MatAtPos       typ _ _       :@ _) = MatOf typ
  typeOf (MatAtIdx       typ _ _ _     :@ _) = MatOf typ
  typeOf (MatIfThenElse  typ _ _ _     :@ _) = MatOf typ
  typeOf (MatConst       typ _         :@ _) = MatOf typ
  typeOf (MatId          typ _         :@ _) = MatOf typ
  typeOf (MatSwapE       typ _ _ _     :@ _) = MatOf typ
  typeOf (MatScaleE      typ _ _ _     :@ _) = MatOf typ
  typeOf (MatAddE        typ _ _ _ _   :@ _) = MatOf typ
  typeOf (MatHCat        typ _ _       :@ _) = MatOf typ
  typeOf (MatVCat        typ _ _       :@ _) = MatOf typ
  typeOf (MatAdd         typ _ _       :@ _) = MatOf typ
  typeOf (MatMul         typ _ _       :@ _) = MatOf typ
  typeOf (MatPow         typ _ _       :@ _) = MatOf typ
  typeOf (MatTrans       typ _         :@ _) = MatOf typ
  typeOf (MatNeg         typ _         :@ _) = MatOf typ
  typeOf (MatSwapRows    typ _ _ _     :@ _) = MatOf typ
  typeOf (MatSwapCols    typ _ _ _     :@ _) = MatOf typ
  typeOf (MatScaleRow    typ _ _ _     :@ _) = MatOf typ
  typeOf (MatScaleCol    typ _ _ _     :@ _) = MatOf typ
  typeOf (MatAddRow      typ _ _ _ _   :@ _) = MatOf typ
  typeOf (MatAddCol      typ _ _ _ _   :@ _) = MatOf typ
  typeOf (MatDelRow      typ _ _       :@ _) = MatOf typ
  typeOf (MatDelCol      typ _ _       :@ _) = MatOf typ
  typeOf (MatShuffleRows typ _         :@ _) = MatOf typ
  typeOf (MatShuffleCols typ _         :@ _) = MatOf typ
  typeOf (MatGJForm      typ _         :@ _) = MatOf typ
  typeOf (MatGJFactor    typ _         :@ _) = MatOf typ
  typeOf (MatGetRow      typ _ _       :@ _) = MatOf typ
  typeOf (MatGetCol      typ _ _       :@ _) = MatOf typ
  typeOf (MatRowFromList typ _         :@ _) = MatOf typ
  typeOf (MatColFromList typ _         :@ _) = MatOf typ
  typeOf (MatRand        typ _         :@ _) = MatOf typ
  typeOf (MatBuilder     typ _ _ _ _ _ :@ _) = MatOf typ


