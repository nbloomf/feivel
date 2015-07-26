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

module Tests.Lib.Struct.Permutation where

{-------------------}
{- Contents        -}
{-   :Suites       -}
{-   :Generators   -}
{-   :Properties   -}
{-------------------}

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Feivel.Lib.Struct.Permutation
import Feivel.Lib.Algebra.Group

import Tests.Util
import Tests.Lib.Group

import Data.List (nub)


testGroupoidPerm :: (Arbitrary t, Show t, Eq t) => t -> TestTree
testGroupoidPerm t = testGroupoid (idOn t)

g_MAX_SIZE :: Int
g_MAX_SIZE = 10

instance (Arbitrary t, Eq t) => Arbitrary (Perm t) where
  arbitrary = do
    xs <- vectorOf g_MAX_SIZE arbitrary
    let ys = nub xs
    zs <- shuffle ys
    case fromPairs $ zip ys zs of
      Left _ -> error "arbitrary permutation"
      Right p -> return p

instance (Arbitrary t, Eq t) => GroupoidArb (Perm t)
