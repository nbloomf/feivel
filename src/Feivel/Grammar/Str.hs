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

module Feivel.Grammar.Str where

import Feivel.Grammar.Util


data StrExprLeaf a int str
  = StrConst Text
  | StrVar   Key

  | StrMacro [(Type, Key, a)] a -- Expr, MacTo SS
  | StrAtPos a int -- ListOf SS
  | StrAtIdx a int int -- MatOf SS
 
  | StrIfThenElse a str str -- BB

  -- Combinators
  | Concat      str str
  | StrStrip    str str
 
  | Reverse     str
  | ToUpper     str
  | ToLower     str
  | Rot13       str

  -- Integer
  | StrHex      int -- ZZ
  | StrRoman    int -- ZZ
  | StrBase36   int -- ZZ

  -- Rational
  | StrDecimal a int -- QQ, ZZ

  -- List
  | StrRand a -- ListOf SS

  -- Matrix
  | StrTab a -- MatOf XX

  -- General
  | StrFormat Format a -- XX
  | StrTypeOf a -- XX

  -- Casting
  | StrIntCast a -- ZZ
  deriving (Eq, Show)
