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

{-# LANGUAGE FlexibleInstances #-}

module Feivel.Grammar (
  module Feivel.Grammar.Expr,
  module Feivel.Grammar.Put,
  module Feivel.Grammar.Get,
  module Feivel.Grammar.Type,

  -- Errors
  ExprErr(..)
) where


import Feivel.Grammar.Expr
import Feivel.Grammar.Put
import Feivel.Grammar.Get
import Feivel.Grammar.Type


data ExprErr
 = UnevaluatedExpression
 | BailMessage String
 deriving (Eq, Show)


