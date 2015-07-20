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

module Feivel.Grammar.Poly where

import Feivel.Grammar.Util

{-------------}
{- :PolyExpr -}
{-------------}

type PolyExpr a = AtLocus (PolyExprLeaf a)

data PolyExprLeaf a
  = PolyConst Type (Poly a)
  | PolyVar   Type Key

  | PolyMacro Type [(Type, Key, a)] a -- MacTo (PolyOver typ)
  | PolyAtPos Type a a -- ListOf (PolyOver typ), ZZ
  | PolyAtIdx Type a a a -- MatOf (PolyOver typ), ZZ, ZZ

  | PolyRand Type a -- ListOf (PolyOf typ)

  | PolyIfThenElse Type a (PolyExpr a) (PolyExpr a) -- BB

  | PolyAdd Type (PolyExpr a) (PolyExpr a)
  | PolySub Type (PolyExpr a) (PolyExpr a)
  | PolyMul Type (PolyExpr a) (PolyExpr a)
  | PolyPow Type (PolyExpr a) a -- ZZ
  | PolyNeg Type (PolyExpr a)

  | PolyFromRoots Type Variable a -- ListOf typ
  | PolyEvalPoly  Type (PolyExpr a) [(Variable, PolyExpr a)]
  deriving (Eq, Show)


instance Typed (PolyExpr a) where
  typeOf (PolyVar        typ _     :@ _) = PolyOver typ
  typeOf (PolyMacro      typ _ _   :@ _) = PolyOver typ
  typeOf (PolyConst      typ _     :@ _) = PolyOver typ
  typeOf (PolyAdd        typ _ _   :@ _) = PolyOver typ
  typeOf (PolySub        typ _ _   :@ _) = PolyOver typ
  typeOf (PolyMul        typ _ _   :@ _) = PolyOver typ
  typeOf (PolyNeg        typ _     :@ _) = PolyOver typ
  typeOf (PolyPow        typ _ _   :@ _) = PolyOver typ
  typeOf (PolyAtPos      typ _ _   :@ _) = PolyOver typ
  typeOf (PolyAtIdx      typ _ _ _ :@ _) = PolyOver typ
  typeOf (PolyIfThenElse typ _ _ _ :@ _) = PolyOver typ
  typeOf (PolyRand       typ _     :@ _) = PolyOver typ
  typeOf (PolyFromRoots  typ _ _   :@ _) = PolyOver typ
  typeOf (PolyEvalPoly   typ _ _   :@ _) = PolyOver typ
