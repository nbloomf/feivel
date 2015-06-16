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

{-# OPTIONS_GHC -XTypeSynonymInstances #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module Tests.Lib.Polynomial where

{-------------------}
{- Contents        -}
{-   :Suites       -}
{-   :Generators   -}
{-   :Properties   -}
{-------------------}

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Feivel.Lib.Polynomial
import Feivel.Lib.Ring
import Feivel.Lib.Canon

import Tests.Util
import Tests.Lib.Ring


{-----------}
{- :Suites -}
{-----------}

testRingoidPoly :: (RingoidArb t, CRingoidArb t, Show t) => t -> Test
testRingoidPoly t = testRingoid (constP t)


{---------------}
{- :Generators -}
{---------------}

varchar :: [Char]
varchar =
  "abcdefghijklmnopqrstuvwxyz" ++
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
  "_[]()"

var :: Gen String
var = listOf1 $ elements varchar

arbPowerOf :: Variable -> Gen Monomial
arbPowerOf x = do
  k <- arbitrary
  return $ fromListM [(x, k)]

instance Arbitrary Natural where
  arbitrary = do
    NonNegative k <- arbitrary
    return (Nat k)

instance Arbitrary Variable where
  arbitrary = do
    cs <- var
    return (Var cs)

instance Arbitrary Monomial where
  arbitrary = do
    t  <- choose (1,5)
    xs <- vectorOf t arbitrary
    ks <- vectorOf t arbitrary
    return $ canon $ fromListM $ zip xs ks

instance (Arbitrary a) => Arbitrary (Poly a) where
  arbitrary = do
    t  <- choose (1,10)
    cs <- vectorOf t arbitrary
    xs <- vectorOf t arbitrary
    return $ fromListP $ zip cs xs

instance (RingoidArb a, CRingoidArb a) => RingoidArb (Poly a)