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

module Feivel.Grammar.List where

import Feivel.Grammar.Util


newtype ListExpr a = ListExpr
  { unListExpr :: AtLocus (ListExprLeaf a)
  } deriving (Eq, Show)

instance HasLocus (ListExpr a) where
  locusOf = locusOf . unListExpr


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



instance Typed (ListExpr a) where
  typeOf (ListExpr (x :@ _)) = case x of
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
