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

module Feivel.Expr.Mac where

import Feivel.Type
import Feivel.Key
import Feivel.Locus
import Feivel.Store

{------------}
{- :MacExpr -}
{------------}

type MacExpr a = AtLocus (MacExprLeaf a)

data MacExprLeaf a
  = MacConst Type [(Type, Key, a)] a (Store a, Bool) -- XX, typ, Expr
  | MacVar   Type Key

  | MacMacro Type [(Type, Key, a)] a -- MacTo (MacTo typ)
  | MacAtPos Type a a -- ListOf (MacTo typ), ZZ
  | MacAtIdx Type a a a -- MatOf (MacTo typ), ZZ, ZZ

  | MacRand Type a -- ListOf (MacTo typ)

  | MacIfThenElse Type a (MacExpr a) (MacExpr a) -- BB
  deriving (Eq, Show)
