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

module Feivel.Grammar.Bool where

import Feivel.Grammar.Util


data BoolExprLeaf a int bool list mat tup
  = BoolConst Bool
  | BoolVar   Key
  | IsDefined Key

  | BoolMacro [(Type, Key, a)] a -- Expr, MacTo BB
  | BoolAtPos  list int
  | BoolAtIdx  mat  int int
  | BoolAtSlot tup  int

  | BoolIfThenElse bool bool bool

  | BoolEq  a a
  | BoolNEq a a

  | BoolLT  Type a a
  | BoolLEq Type a a
  | BoolGT  Type a a
  | BoolGEq Type a a

  -- Arithmetic
  | Conj bool bool
  | Disj bool bool
  | Imp  bool bool
  | Neg  bool

  -- String
  | Matches a Text -- SS

  -- Integer
  | IntDiv    int int
  | IntSqFree int

  | BoolRand list

  -- List
  | ListElem    a list -- XX, ListOf XX
  | ListIsEmpty list

  -- Matrix
  | MatIsRow    mat
  | MatIsCol    mat
  | MatIsGJForm mat
  deriving (Eq, Show)
