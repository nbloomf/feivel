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

module Feivel.Lib.Data.Natural where

import Feivel.Lib.AlgErr


newtype Natural = Nat { unNat :: Integer }
  deriving (Eq, Ord, Show)

natAdd :: Natural -> Natural -> Natural
natAdd (Nat a) (Nat b) = Nat (a+b)

natMul :: Natural -> Natural -> Natural
natMul (Nat a) (Nat b) = Nat (a*b)

natMax :: Natural -> Natural -> Natural
natMax (Nat a) (Nat b) = Nat (max a b)

natMin :: Natural -> Natural -> Natural
natMin (Nat a) (Nat b) = Nat (min a b)

natSum :: [Natural] -> Natural
natSum = foldr natAdd (Nat 0)

natIsZero :: Natural -> Bool
natIsZero = (0==) . unNat

natSub :: Natural -> Natural -> Either AlgErr Natural
natSub (Nat a) (Nat b)
  | b > a = Left NatSubErr
  | otherwise = return $ Nat (a-b)
