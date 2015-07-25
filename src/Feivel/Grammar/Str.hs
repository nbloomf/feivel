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

module Feivel.Grammar.Str where

import Feivel.Grammar.Util

newtype StrExpr a = StrExpr
  { unStrExpr :: AtLocus (StrExprLeaf a)
  } deriving (Eq, Show)

instance HasLocus (StrExpr a) where
  locusOf = locusOf . unStrExpr


data StrExprLeaf a
  = StrConst Text
  | StrVar   Key

  | StrMacro [(Type, Key, a)] a -- Expr, MacTo SS
  | StrAtPos a a -- ListOf SS, ZZ
  | StrAtIdx a a a -- MatOf SS, ZZ, ZZ
 
  | StrIfThenElse a (StrExpr a) (StrExpr a) -- BB

  -- Combinators
  | Concat      (StrExpr a) (StrExpr a)
  | StrStrip    (StrExpr a) (StrExpr a)
 
  | Reverse     (StrExpr a)
  | ToUpper     (StrExpr a)
  | ToLower     (StrExpr a)
  | Rot13       (StrExpr a)

  -- Integer
  | StrHex      a -- ZZ
  | StrRoman    a -- ZZ
  | StrBase36   a -- ZZ

  -- Rational
  | StrDecimal a a -- QQ, ZZ

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

instance Typed (StrExpr a)  where typeOf _ = SS
