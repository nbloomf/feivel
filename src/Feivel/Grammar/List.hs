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

module Feivel.Grammar.List where

import Feivel.Grammar.Util


data ListExprLeaf a bool int list mat
  = ListConst   [a]
  | ListVar     Key
  | ListBuilder a [ListGuard a list]

  | ListMacro      [(Type, Key, a)] a -- MacTo (ListOf typ)
  | ListAtPos      a int -- ListOf (ListOf typ)
  | ListAtIdx      mat int int
  | ListRand       a -- ListOf (ListOf typ)
  | ListIfThenElse bool list list

  -- Arithmetic
  | ListCat   list list
  | ListToss  list list
  | ListRev   list
  | ListSort  list
  | ListUniq  list

  | ListFilter Key a list -- BB

  -- Integer
  | ListRange int int -- ZZ, ZZ

  -- Matrices
  | ListMatRow int mat
  | ListMatCol int mat

  -- Random
  | ListShuffle  list
  | ListChoose   int list

  | ListShuffles list
  | ListChoices  int list

  -- Permutations
  | ListPermsOf list

  | ListBezouts list

  | ListPivotColIndices mat
  deriving (Eq, Show)

data ListGuard a list
  = Bind  Key list
  | Guard a -- BB
  deriving (Eq, Show)
