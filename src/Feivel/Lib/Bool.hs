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

module Feivel.Lib.Bool (
  BoolErr(),

  strEq, strNEq, strMatch,
  boolEq, boolNEq, boolImp, boolAnd, boolOr, boolXOr, boolNot
) where

import Text.Regex.Posix ((=~))

data BoolErr
 = BoolErr
 deriving (Eq, Show)


{----------}
{- String -}
{----------}

strEq :: String -> String -> Either BoolErr Bool
strEq a b = Right $ a == b

strNEq :: String -> String -> Either BoolErr Bool
strNEq a b = Right $ a /= b

strMatch :: String -> String -> Either BoolErr Bool
strMatch str pat = Right $ str =~ pat

{- Bool -}

boolEq :: Bool -> Bool -> Either BoolErr Bool
boolEq a b = Right $ a == b

boolNEq :: Bool -> Bool -> Either BoolErr Bool
boolNEq a b = Right $ a /= b

boolImp :: Bool -> Bool -> Either BoolErr Bool
boolImp x y = Right $ (not x) || y

boolAnd :: Bool -> Bool -> Either BoolErr Bool
boolAnd a b = Right $ a && b

boolOr :: Bool -> Bool -> Either BoolErr Bool
boolOr a b = Right $ a || b

boolXOr :: Bool -> Bool -> Either BoolErr Bool
boolXOr a b = Right $ (a || b) && not (a && b)

boolNot :: Bool -> Either BoolErr Bool
boolNot a = Right $ not a