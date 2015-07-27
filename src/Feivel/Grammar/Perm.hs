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


data PermExprLeaf a perm
  = PermConst Type (Perm a)
  | PermVar   Type Key

  | PermMacro Type [(Type, Key, a)] a -- Expr, MacTo (PermOf typ)
  | PermAtPos Type a a -- ListOf (PermOf typ), ZZ
  | PermAtIdx Type a a a -- MatOf (PermOf typ), ZZ, ZZ

  | PermRand Type a -- ListOf typ

  | PermIfThenElse Type a perm perm -- BB

  | PermCompose Type perm perm
  | PermInvert  Type perm
  deriving (Eq, Show)



instance Typed (PermExprLeaf a perm) where
  typeOf x = case x of
    PermVar        typ _     -> PermOf typ
    PermMacro      typ _ _   -> PermOf typ
    PermConst      typ _     -> PermOf typ
    PermAtPos      typ _ _   -> PermOf typ
    PermAtIdx      typ _ _ _ -> PermOf typ
    PermIfThenElse typ _ _ _ -> PermOf typ
    PermRand       typ _     -> PermOf typ
    PermCompose    typ _ _   -> PermOf typ
    PermInvert     typ _     -> PermOf typ
