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

module Tests.Lib.Group where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, forAll, Gen, Arbitrary, arbitrary, vectorOf)

import Tests.Util

import Feivel.Lib.Algebra.Group

{-----------}
{- :Suites -}
{-----------}

testGroupoid :: (Groupoid t, GroupoidArb t, Show t, Eq t) => t -> TestTree
testGroupoid t = testGroup "Groupoid Structure"
  [ testProperty "(a.b).c == a.(b.c)  " $ prop_gOp_assoc  t
  , testProperty "1.a == a            " $ prop_gOp_lneut  t
  , testProperty "a.1 == a            " $ prop_gOp_rneut  t
  , testProperty "inv (inv a) == a    " $ prop_gInv_invol t
  ]



class (Groupoid t, Arbitrary t) => GroupoidArb t where
  gElt :: t -> Gen t
  gElt _ = arbitrary

  gLocalElts :: t -> Int -> Gen [t]
  gLocalElts _ n = vectorOf n arbitrary

  gOpAssoc     :: t -> Gen (t,t,t) -- (x+y)+z and x+(y+z) both exist
  gOpLNeut     :: t -> Gen (t,t)   -- x is left neutral for y wrt rAdd
  gOpRNeut     :: t -> Gen (t,t)   -- y is right neutral for x wrt rAdd

  gOpLNeut _ = do
    y <- arbitrary
    return (gLIdOf y, y)

  gOpRNeut _ = do
    x <- arbitrary
    return (x, gRIdOf x)

  -- Defaults: Assume ringoid is total.
  gOpAssoc     a = arb3 a




{- :Op -}

prop_gOp_assoc :: (GroupoidArb t, Show t, Eq t) => t -> Property
prop_gOp_assoc t = forAll (gOpAssoc t) (isAssociativeBy gOp gEQ)

prop_gOp_lneut :: (GroupoidArb t, Show t, Eq t) => t -> Property
prop_gOp_lneut t = forAll (gOpLNeut t) (isLIdentityUnderBy gOp gEQ)

prop_gOp_rneut :: (GroupoidArb t, Show t, Eq t) => t -> Property
prop_gOp_rneut t = forAll (gOpRNeut t) (isRIdentityUnderBy gOp gEQ)

{- :Inv -}

prop_gInv_invol :: (GroupoidArb t, Show t, Eq t) => t -> Property
prop_gInv_invol t = forAll (gElt t) (isInvolutiveUBy gInv gEQ)


