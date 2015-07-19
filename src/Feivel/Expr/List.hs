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

module Feivel.Expr.List where

import Feivel.Expr.Util


type ListExpr a = AtLocus (ListExprLeaf a)

data ListExprLeaf a
  = ListConst   Type [a]
  | ListVar     Type Key
  | ListBuilder Type a [ListGuard a]

  | ListMacro      Type [(Type, Key, a)] a -- MacTo (ListOf typ)
  | ListAtPos      Type a a -- ListOf (ListOf typ), ZZ
  | ListAtIdx      Type a a a -- MatOf (ListOf typ), ZZ, ZZ
  | ListRand       Type a -- ListOf (ListOf typ)
  | ListIfThenElse Type a (ListExpr a) (ListExpr a) -- BB

  -- Arithmetic
  | ListCat   Type (ListExpr a) (ListExpr a)
  | ListToss  Type (ListExpr a) (ListExpr a)
  | ListRev   Type (ListExpr a)
  | ListSort  Type (ListExpr a)
  | ListUniq  Type (ListExpr a)

  | ListFilter Type Key a (ListExpr a) -- BB

  -- Integer
  | ListRange Type a a -- ZZ, ZZ

  -- Matrices
  | ListMatRow Type a a -- ZZ, MatOf typ
  | ListMatCol Type a a -- ZZ, MatOf typ

  -- Random
  | ListShuffle  Type (ListExpr a)
  | ListChoose   Type a  (ListExpr a) -- ZZ

  | ListShuffles Type (ListExpr a)
  | ListChoices  Type a  (ListExpr a) -- ZZ

  -- Permutations
  | ListPermsOf Type (ListExpr a)

  | ListPivotColIndices Type a -- MatOf XX
  deriving (Eq, Show)

data ListGuard a
  = Bind  Key (ListExpr a)
  | Guard a -- BB
  deriving (Eq, Show)
