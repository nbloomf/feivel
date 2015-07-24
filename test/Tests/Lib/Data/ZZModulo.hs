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

module Tests.Lib.Data.ZZModulo where

{-------------------}
{- Contents        -}
{-   :Suites       -}
{-   :Generators   -}
{-   :Properties   -}
{-------------------}

import Test.Tasty(TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Feivel.Lib.Data.ZZMod
import Feivel.Lib.Algebra.Ring

import Tests.Util
import Tests.Lib.Ring



{-----------}
{- :Suites -}
{-----------}

testZZModulo :: TestTree
testZZModulo = testGroup "ZZModulo"
  [ testRingoid  (0 `zzmod` 0)
  , testCRingoid (0 `zzmod` 0)
  , testURingoid (0 `zzmod` 0)
  ]



{---------------}
{- :Generators -}
{---------------}

gMAX_MODULUS :: Int
gMAX_MODULUS = 100

arbModulus :: Gen Int
arbModulus = choose (2, gMAX_MODULUS)

arbResidueMod :: Int -> Gen ZZModulo
arbResidueMod n = do
  a <- choose (0,n) :: Gen Int
  return $ (fromIntegral a) `zzmod` (fromIntegral n)

instance Arbitrary ZZModulo where
  arbitrary = arbModulus >>= arbResidueMod



{---------------}
{- :RingoidArb -}
{---------------}

instance RingoidArb ZZModulo where
  rLocalElts (ZZModulo _ n) k = do
    as <- vectorOf k arbitrary
    return $ map (`zzmod` n) as

  rAddAssoc _ = do
    n <- arbModulus
    a <- arbResidueMod n
    b <- arbResidueMod n
    c <- arbResidueMod n
    return (a, b, c)

  rAddComm _ = do
    n <- arbModulus
    a <- arbResidueMod n
    b <- arbResidueMod n
    return (a, b)

  rAddLNeut _ = do
    n <- arbModulus
    a <- arbResidueMod n
    return (0 `zzmod` (fromIntegral n), a)

  rAddRNeut _ = do
    n <- arbModulus
    a <- arbResidueMod n
    return (a, 0 `zzmod` (fromIntegral n))

  rMulAssoc _ = do
    n <- arbModulus
    a <- arbResidueMod n
    b <- arbResidueMod n
    c <- arbResidueMod n
    return (a, b, c)

  rMulDistLrAdd _ = do
    n <- arbModulus
    a <- arbResidueMod n
    b <- arbResidueMod n
    c <- arbResidueMod n
    return (a, b, c)

  rMulDistRrAdd _ = do
    n <- arbModulus
    a <- arbResidueMod n
    b <- arbResidueMod n
    c <- arbResidueMod n
    return (a, b, c)


instance CRingoidArb ZZModulo where
  rMulComm _ = do
    n <- arbModulus
    a <- arbResidueMod n
    b <- arbResidueMod n
    return (a, b)


instance URingoidArb ZZModulo where
  rMulLOne _ = arbitrary >>= \x@(ZZModulo _ n) -> return (1 `zzmod` n, x)
  rMulROne _ = arbitrary >>= \x@(ZZModulo _ n) -> return (x, 1 `zzmod` n)

