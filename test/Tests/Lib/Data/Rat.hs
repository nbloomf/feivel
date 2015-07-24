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

module Tests.Lib.Data.Rat where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Feivel.Lib.Data.Rat

import Tests.Util
import Tests.Lib.Ring



{----------}
{- :Suite -}
{----------}

testRat :: TestTree
testRat = testGroup "Rat"
  [ testRingoid  (0:/:1)
  , testCRingoid (0:/:1)
  , testURingoid (0:/:1)
  , testORingoid (0:/:1)
  ]



{---------------}
{- :Generators -}
{---------------}

instance Arbitrary Rat where
  arbitrary = do
    x         <- arbitrary
    NonZero y <- arbitrary
    return $ x :/: y

instance RingoidArb Rat

instance CRingoidArb Rat

instance URingoidArb Rat where
  rMulLOne      _ = arbitrary >>= \a -> return (1:/:1,a)
  rMulROne      _ = arbitrary >>= \a -> return (a,1:/:1)

instance ORingoidArb Rat
