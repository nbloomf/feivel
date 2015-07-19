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

module Feivel.Expr.Perm where

import Feivel.Expr.Util


type PermExpr a = AtLocus (PermExprLeaf a)

data PermExprLeaf a
  = PermConst Type (Perm a)
  | PermVar   Type Key

  | PermMacro Type [(Type, Key, a)] a -- Expr, MacTo (PermOf typ)
  | PermAtPos Type a a -- ListOf (PermOf typ), ZZ
  | PermAtIdx Type a a a -- MatOf (PermOf typ), ZZ, ZZ

  | PermRand Type a -- ListOf typ

  | PermIfThenElse Type a (PermExpr a) (PermExpr a) -- BB

  | PermCompose Type (PermExpr a) (PermExpr a)
  | PermInvert  Type (PermExpr a)
  deriving (Eq, Show)
