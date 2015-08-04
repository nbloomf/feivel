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
  = ListConst   Type [a]
  | ListVar     Type Key
  | ListBuilder Type a [ListGuard a list]

  | ListMacro      Type [(Type, Key, a)] a -- MacTo (ListOf typ)
  | ListAtPos      Type a int -- ListOf (ListOf typ)
  | ListAtIdx      Type mat int int
  | ListRand       Type a -- ListOf (ListOf typ)
  | ListIfThenElse Type bool list list

  -- Arithmetic
  | ListCat   Type list list
  | ListToss  Type list list
  | ListRev   Type list
  | ListSort  Type list
  | ListUniq  Type list

  | ListFilter Type Key a list -- BB

  -- Integer
  | ListRange Type int int -- ZZ, ZZ

  -- Matrices
  | ListMatRow Type int mat
  | ListMatCol Type int mat

  -- Random
  | ListShuffle  Type list
  | ListChoose   Type int list

  | ListShuffles Type list
  | ListChoices  Type int list

  -- Permutations
  | ListPermsOf Type list

  | ListPivotColIndices Type mat
  deriving (Eq, Show)

data ListGuard a list
  = Bind  Key list
  | Guard a -- BB
  deriving (Eq, Show)


instance Typed (ListExprLeaf a bool int list mat) where
  typeOf x = case x of
    ListConst           typ _     -> ListOf typ
    ListVar             typ _     -> ListOf typ
    ListIfThenElse      typ _ _ _ -> ListOf typ
    ListRand            typ _     -> ListOf typ
    ListAtPos           typ _ _   -> ListOf typ
    ListAtIdx           typ _ _ _ -> ListOf typ
    ListMacro           typ _ _   -> ListOf typ
    ListCat             typ _ _   -> ListOf typ
    ListToss            typ _ _   -> ListOf typ
    ListRev             typ _     -> ListOf typ
    ListSort            typ _     -> ListOf typ
    ListUniq            typ _     -> ListOf typ
    ListShuffle         typ _     -> ListOf typ
    ListFilter          typ _ _ _ -> ListOf typ
    ListMatRow          typ _ _   -> ListOf typ
    ListMatCol          typ _ _   -> ListOf typ
    ListChoose          typ _ _   -> ListOf typ
    ListShuffles        typ _     -> ListOf typ
    ListChoices         typ _ _   -> ListOf typ
    ListRange           typ _ _   -> ListOf typ
    ListPermsOf         typ _     -> ListOf typ
    ListBuilder         typ _ _   -> ListOf typ
    ListPivotColIndices typ _     -> ListOf typ
