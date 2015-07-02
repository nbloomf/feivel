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

module Feivel.Lib.ZZMod where

import Feivel.Lib.Ring
import Feivel.Lib.AlgErr

data ZZMod = ZZMod
  { residue :: Integer
  , modulus :: Integer
  } deriving Show

zzmod :: Integer -> Integer -> ZZMod
a `zzmod` n = ZZMod b m
  where
    m = abs n
    b = ((a `rem` m) + m) `rem` m

instance Eq ZZMod where
  (ZZMod a n) == (ZZMod b m)
    = n == m && ((a-b) `rem` n == 0)

instance Ringoid ZZMod where
  rAdd (ZZMod a n) (ZZMod b m)
    | n /= m = Left DifferentModulus
    | otherwise = Right $ (a+b) `zzmod` n

  rMul (ZZMod a 0) (ZZMod b m) = Right $ (a*b) `zzmod` m
  rMul (ZZMod a n) (ZZMod b 0) = Right $ (a*b) `zzmod` n
  rMul (ZZMod a n) (ZZMod b m)
    | n /= m = Left DifferentModulus
    | otherwise = Right $ (a*b) `zzmod` n

  rNeg (ZZMod a n) = (-a) `zzmod` n

  rZero = 0 `zzmod` 0

  rIsZero (ZZMod a n) = (a `rem` n) == 0

  rNeutOf (ZZMod a n) = Right $ ZZMod 0 n
  rLAnnOf (ZZMod a n) = Right $ ZZMod 0 n
  rRAnnOf (ZZMod a n) = Right $ ZZMod 0 n


instance CRingoid ZZMod


instance URingoid ZZMod where
  rOne = ZZMod 1 0

  rIsOne (ZZMod a n) = (a `rem` n) == 1

  rIsUnit (ZZMod a n) = Right $ (a `gcd` n) == 1

  rInjInt a = Right (ZZMod a 0)

  rInv (ZZMod a n)
    | (a `gcd` n) /= 1 = Left $ RingoidNotInvertibleErr (show a ++ " mod " ++ show n)
    | otherwise = do
        (h,_) <- rBezout a n
        return (h `zzmod` n)

  rLOneOf (ZZMod a n) = Right $ ZZMod 1 n
  rROneOf (ZZMod a n) = Right $ ZZMod 1 n