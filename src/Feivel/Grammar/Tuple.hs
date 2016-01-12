{---------------------------------------------------------------------}
{- Copyright 2016 Nathan Bloomfield                                  -}
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

module Feivel.Grammar.Tuple where

import Carl.Struct.Tuple
import Feivel.Grammar.Util


data TupleExprLeaf a bool int list mat tup
  = TupleConst   (Tuple a)
  | TupleVar     Key

  | TupleMacro  [(Type, Key, a)] a -- MacTo (TupleOf typ)
  | TupleAtPos  a   int -- ListOf (TupleOf typ)
  | TupleAtIdx  mat int int
  | TupleAtSlot tup int

  | TupleRand       a -- ListOf (TupleOf typ)
  | TupleIfThenElse bool tup tup
  deriving (Eq, Show)
