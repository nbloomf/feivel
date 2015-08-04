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

module Feivel.Grammar.Poly where

import Feivel.Grammar.Util


data PolyExprLeaf a bool int list poly
  = PolyConst Type (Poly a)
  | PolyVar   Type Key

  | PolyMacro Type [(Type, Key, a)] a -- MacTo (PolyOver typ)
  | PolyAtPos Type list int
  | PolyAtIdx Type a int int -- MatOf (PolyOver typ)

  | PolyRand Type list

  | PolyIfThenElse Type bool poly poly

  | PolyAdd Type poly poly
  | PolySub Type poly poly
  | PolyMul Type poly poly
  | PolyQuo Type poly poly
  | PolyRem Type poly poly
  | PolyPow Type poly int
  | PolyNeg Type poly

  | PolySum Type list

  | PolyFromRoots Type Variable list
  | PolyEvalPoly  Type poly [(Variable, poly)]
  deriving (Eq, Show)


instance Typed (PolyExprLeaf a bool int list poly) where
  typeOf x = case x of
    PolyVar        typ _     -> PolyOver typ
    PolyMacro      typ _ _   -> PolyOver typ
    PolyConst      typ _     -> PolyOver typ
    PolyAdd        typ _ _   -> PolyOver typ
    PolySub        typ _ _   -> PolyOver typ
    PolyMul        typ _ _   -> PolyOver typ
    PolyQuo        typ _ _   -> PolyOver typ
    PolyRem        typ _ _   -> PolyOver typ
    PolyNeg        typ _     -> PolyOver typ
    PolySum        typ _     -> PolyOver typ
    PolyPow        typ _ _   -> PolyOver typ
    PolyAtPos      typ _ _   -> PolyOver typ
    PolyAtIdx      typ _ _ _ -> PolyOver typ
    PolyIfThenElse typ _ _ _ -> PolyOver typ
    PolyRand       typ _     -> PolyOver typ
    PolyFromRoots  typ _ _   -> PolyOver typ
    PolyEvalPoly   typ _ _   -> PolyOver typ
