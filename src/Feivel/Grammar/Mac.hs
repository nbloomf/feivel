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

module Feivel.Grammar.Mac where

import Feivel.Grammar.Util


data MacExprLeaf a mac
  = MacConst Type [(Type, Key, a)] a (Store a, Bool) -- XX, typ, Expr
  | MacVar   Type Key

  | MacMacro Type [(Type, Key, a)] a -- MacTo (MacTo typ)
  | MacAtPos Type a a -- ListOf (MacTo typ), ZZ
  | MacAtIdx Type a a a -- MatOf (MacTo typ), ZZ, ZZ

  | MacRand Type a -- ListOf (MacTo typ)

  | MacIfThenElse Type a mac mac -- BB
  deriving (Eq, Show)


instance Typed (MacExprLeaf a mac) where
  typeOf x = case x of
    MacConst      typ _ _ _ -> MacTo typ
    MacVar        typ _     -> MacTo typ
    MacMacro      typ _ _   -> MacTo typ
    MacAtPos      typ _ _   -> MacTo typ
    MacAtIdx      typ _ _ _ -> MacTo typ
    MacRand       typ _     -> MacTo typ
    MacIfThenElse typ _ _ _ -> MacTo typ
