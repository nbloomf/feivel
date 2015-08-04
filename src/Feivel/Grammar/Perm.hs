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

module Feivel.Grammar.Perm where

import Feivel.Grammar.Util


data PermExprLeaf a bool int list mat perm
  = PermConst (Perm a)
  | PermVar   Key

  | PermMacro [(Type, Key, a)] a -- Expr, MacTo (PermOf typ)
  | PermAtPos list int
  | PermAtIdx mat  int int

  | PermRand list

  | PermIfThenElse bool perm perm

  | PermCompose perm perm
  | PermInvert  perm
  deriving (Eq, Show)
