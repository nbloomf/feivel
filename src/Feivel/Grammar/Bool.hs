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

module Feivel.Grammar.Bool where

import Feivel.Grammar.Util


data BoolExprLeaf a bool
  = BoolConst Bool
  | BoolVar   Key
  | IsDefined Key

  | BoolMacro [(Type, Key, a)] a -- Expr, MacTo BB
  | BoolAtPos a a      -- ListOf BB, ZZ
  | BoolAtIdx a a a -- MatOf BB, ZZ, ZZ

  | BoolIfThenElse a bool bool -- BB

  | BoolEq  a a
  | BoolNEq a a

  | BoolLT  a a
  | BoolLEq a a
  | BoolGT  a a
  | BoolGEq a a

  -- Arithmetic
  | Conj bool bool
  | Disj bool bool
  | Imp  bool bool
  | Neg  bool

  -- String
  | Matches a Text -- SS

  -- Integer
  | IntDiv    a a -- ZZ, ZZ
  | IntSqFree a      -- ZZ

  | BoolRand a -- ListOf BB

  -- List
  | ListElem    a a -- XX, ListOf XX
  | ListIsEmpty a -- ListOf XX

  -- Matrix
  | MatIsRow    a -- MatOf XX
  | MatIsCol    a -- MatOf XX
  | MatIsGJForm a -- MatOf XX
  deriving (Eq, Show)
