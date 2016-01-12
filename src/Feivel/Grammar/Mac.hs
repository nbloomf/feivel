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

module Feivel.Grammar.Mac where

import Feivel.Grammar.Util


data MacExprLeaf a bool int list mat mac tup
  = MacConst [(Type, Key, a)] a (Store a, Bool) -- XX, typ, Expr
  | MacVar   Key

  | MacMacro  [(Type, Key, a)] a -- MacTo (MacTo typ)
  | MacAtPos  list int
  | MacAtIdx  mat  int int
  | MacAtSlot tup  int

  | MacRand list

  | MacIfThenElse bool mac mac
  deriving (Eq, Show)
