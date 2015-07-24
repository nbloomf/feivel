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

module Tests.Lib.Struct.Matrix where

{-------------------}
{- Contents        -}
{-   :Suites       -}
{-   :Generators   -}
{-   :Properties   -}
{-------------------}

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Feivel.Lib.Struct.Matrix
import Feivel.Lib.Algebra.Ring

import Tests.Util
import Tests.Lib.Ring



{-----------}
{- :Suites -}
{-----------}

testRingoidMat :: (Ringoid t, Arbitrary t, Show t, RingoidArb t) => t -> TestTree
testRingoidMat t = testRingoid (mCell t)

testURingoidMat = undefined

testBipRingoidMat :: (Ringoid t, Arbitrary t, Show t, RingoidArb t) => t -> TestTree
testBipRingoidMat t = testBipRingoid (mCell t)



{---------------}
{- :Generators -}
{---------------}

gMAX_MAT_DIM :: Int
gMAX_MAT_DIM = 3

{- Arbitrary dimensions (within reason) -}

arbDim2 :: Gen (Integer, Integer)
arbDim2 = do
  r <- choose (1, gMAX_MAT_DIM)
  c <- choose (1, gMAX_MAT_DIM)
  return (fromIntegral r, fromIntegral c)

arbDim3 :: Gen (Integer, Integer, Integer)
arbDim3 = do
  r <- choose (1, gMAX_MAT_DIM)
  k <- choose (1, gMAX_MAT_DIM)
  c <- choose (1, gMAX_MAT_DIM)
  return (fromIntegral r, fromIntegral k, fromIntegral c)

arbDim4 :: Gen (Integer, Integer, Integer, Integer)
arbDim4 = do
  r <- choose (1, gMAX_MAT_DIM)
  k <- choose (1, gMAX_MAT_DIM)
  m <- choose (1, gMAX_MAT_DIM)
  c <- choose (1, gMAX_MAT_DIM)
  return (fromIntegral r, fromIntegral k, fromIntegral m, fromIntegral c)


-- Arbitrary matrix with fixed dimension
arbMatDim :: (Arbitrary a) => Integer -> Integer -> Gen (Matrix a)
arbMatDim r c = do
  as <- vectorOf ((fromIntegral r)*(fromIntegral c)) arbitrary
  case mRowMajorFromList r c as of
    Left  _ -> return Null
    Right m -> return m

arbRingoidMatDim :: (RingoidArb t) => Integer -> Integer -> t -> Gen (Matrix t)
arbRingoidMatDim r c x = do
  as <- rLocalElts x ((fromIntegral r)*(fromIntegral c))
  case mRowMajorFromList r c as of
    Left  _ -> return Null
    Right m -> return m


instance (Arbitrary t) => Arbitrary (Matrix t) where
  arbitrary = do
    t <- choose (1, 20) :: Gen Int
    case t of
      1 -> return Null
      _ -> do
        (r,c) <- arbDim2
        arbMatDim r c



{---------------}
{- :RingoidArb -}
{---------------}

instance (Ringoid t, Arbitrary t, RingoidArb t) => RingoidArb (Matrix t) where
  rAddAssoc _ = do
    (r,c) <- arbDim2
    x  <- arbitrary
    m1 <- arbRingoidMatDim r c x
    m2 <- arbRingoidMatDim r c x
    m3 <- arbRingoidMatDim r c x
    return (m1, m2, m3)

  rAddComm _ = do
    (r,c) <- arbDim2
    x  <- arbitrary
    m1 <- arbRingoidMatDim r c x
    m2 <- arbRingoidMatDim r c x
    return (m1, m2)

  rMulAssoc _ = do
    (r,k,m,c) <- arbDim4
    x  <- arbitrary
    m1 <- arbRingoidMatDim r k x
    m2 <- arbRingoidMatDim k m x
    m3 <- arbRingoidMatDim m c x
    return (m1, m2, m3)

  rMulDistLrAdd _ = do
    (r,k,c) <- arbDim3
    x  <- arbitrary
    m1 <- arbRingoidMatDim r k x
    m2 <- arbRingoidMatDim k c x
    m3 <- arbRingoidMatDim k c x
    return (m1, m2, m3)

  rMulDistRrAdd _ = do
    (r,k,c) <- arbDim3
    x  <- arbitrary
    m1 <- arbRingoidMatDim r k x
    m2 <- arbRingoidMatDim r k x
    m3 <- arbRingoidMatDim k c x
    return (m1, m2, m3)

  rMulLZero _ = do
    (r,c) <- arbDim2
    x <- arbitrary
    m <- arbRingoidMatDim r c x
    case rLAnnOf m of
      Left _ -> error "rMulLZero in Tests.Lib.Matrix"
      Right z -> return (z,m)

  rMulRZero _ = do
    (r,c) <- arbDim2
    x <- arbitrary
    m <- arbRingoidMatDim r c x
    case rRAnnOf m of
      Left _ -> error "rMulRZero in Tests.Lib.Matrix"
      Right z -> return (m,z)


{------------------}
{- :BipRingoidArb -}
{------------------}

instance (Ringoid t, Arbitrary t, RingoidArb t) => BipRingoidArb (Matrix t) where
  rBipInAssoc _ = do
    (r1,r2,r3,c) <- arbDim4
    m1 <- arbMatDim r1 c
    m2 <- arbMatDim r2 c
    m3 <- arbMatDim r3 c
    return (m1, m2, m3)

  rMulDistRrBipIn _ = do
    (r1,r2,k,c) <- arbDim4
    x  <- arbitrary
    m1 <- arbRingoidMatDim r1 k x
    m2 <- arbRingoidMatDim r2 k x
    m3 <- arbRingoidMatDim k  c x
    return (m1, m2, m3)

  rBipOutAssoc _ = do
    (r,c1,c2,c3) <- arbDim4
    m1 <- arbMatDim r c1
    m2 <- arbMatDim r c2
    m3 <- arbMatDim r c3
    return (m1, m2, m3)

  rMulDistLrBipOut _ = do
    (r,k,c1,c2) <- arbDim4
    x  <- arbitrary
    m1 <- arbRingoidMatDim r k  x
    m2 <- arbRingoidMatDim k c1 x
    m3 <- arbRingoidMatDim k c2 x
    return (m1, m2, m3)



{-

{- Properties that hold over any type -}

testMatrixStructure :: (Arbitrary a, Eq a, Show a) => a -> Test
testMatrixStructure a = testGroup "Matrix Structure"
  [ testGroup "hCat (|)"
      , testProperty "A|Ω === Ω|A === A        " $ prop_hCat_null a
      , testProperty "split                    " $ prop_hCat_split a
      , testProperty "κ₁A|...|κₘA === A        " $ prop_hCat_cols a
      ]
  , testGroup "vCat ()"
      , testProperty "A÷Ω === Ω÷A === A        " $ prop_vCat_null a
      , testProperty "split                    " $ prop_vCat_split a
      , testProperty "ρ₁A÷...÷ρₙA === A        " $ prop_vCat_rows a
      ]
  , testGroup "transpose"
      [ testProperty "t(t(a)) == a             " $ prop_transpose_involution a
      , testProperty "t(A|B) == t(A)÷t(B)      " $ prop_transpose_hCat a
      , testProperty "t(A÷B) == t(A)|t(B)      " $ prop_transpose_vCat a
      , testProperty "t(row) = col             " $ prop_transpose_row a
      ]
  , testGroup "rowOf (ρₜ)"
      [ testProperty "list                     " $ prop_rowOf_list a
      , testProperty "is row                   " $ prop_rowOf_isrow a
      ]
  , testGroup "colOf (κₜ)"
      [ testProperty "list                     " $ prop_colOf_list a
      , testProperty "is col                   " $ prop_colOf_iscol a
      ]
  , testGroup "swapRows"
      [ testProperty "involution               " $ prop_swapRows_involution a
      ]
  , testGroup "swapCols"
      [ testProperty "involution               " $ prop_swapCols_involution a
      ]
  ]


{- Properties that hold over any ring -}

testMatrixArithmetic :: (Arbitrary a, Eq a, Show a, Ring a) => a -> Test
testMatrixArithmetic a = testGroup "Matrix Arithmetic"
  [ testGroup "matAdd (+)"
      , testProperty "A+Ω == Ω+A == A          " $ prop_matAdd_null a
      ]
  ]

{- Peroperties that hold over any unital ring -}

testMatrixUnital :: (Arbitrary a, Eq a, Show a, URing a) => a -> Test
testMatrixUnital a = testGroup "Unital Matrix Properties"
  [ testGroup "Elementary Matrices"
      [ testProperty "Elementary Row Swap      " $ prop_swapRows_elementary a
      , testProperty "Elementary Col Swap      " $ prop_swapCols_elementary a
      ]
  ]

{- Peroperties that hold over any commutative ring -}

testMatrixCU :: (Arbitrary a, Eq a, Show a, CRing a, URing a) => a -> Test
testMatrixCU a = testGroup "Unital Matrix Properties"
  [ testGroup "Elementary Matrices"
      [ testProperty "Elementary Row Scale     " $ prop_scaleRow_elementary a
      , testProperty "Elementary Col Scale     " $ prop_scaleCol_elementary a
      , testProperty "Elementary Row Add       " $ prop_addRow_elementary a
      , testProperty "Elementary Col Add       " $ prop_addCol_elementary a
      ]
  ]

testMatrixField :: (Arbitrary a, Eq a, Show a, Field a) => a -> Test
testMatrixField a = testGroup "Field Matrix Properties"
  [ testGroup "Factorizations"
      [ testProperty "Gauss-Jordan: M = PR     " $ prop_gaussjordan_factor a
      , testProperty "GJForm is in GJForm      " $ prop_gaussjordan_gjform a
      ]
  ]

testMatrix :: Test
testMatrix = testGroup "Matrix"
  [ 
   testGroup "misc"
      [ testProperty "rows&cols                " prop_dim
      --, testProperty "isRowIndexOf             " prop_isRowIndexOf
     -- , testProperty "isColIndexOf             " prop_isColIndexOf
      , testProperty "singleton                " prop_singleton
    --  , testProperty "bCat: joinsplit          " prop_bCat_joinsplit a
      ]
  ]


-}