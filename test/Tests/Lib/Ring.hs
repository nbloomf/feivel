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

module Tests.Lib.Ring where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, forAll, Gen, Arbitrary, arbitrary, vectorOf)

import Tests.Util

import Feivel.Lib.Algebra.Ring

{--------------------}
{- Contents         -}
{-   :Suites        -}
{-   :Generators    -}
{-     :RingoidArb  -}
{-     :CRingoidArb -}
{-     :URingoidArb -}
{-   :Properties    -}
{-     :Ringoid     -}
{-     :CRingoid    -}
{-     :URingoid    -}
{-     :ORingoid    -}
{-     :GCDoid      -}
{-     :BipRingoid  -}
{--------------------}

{-----------}
{- :Suites -}
{-----------}

testRingoid :: (Ringoid t, RingoidArb t, Show t) => t -> TestTree
testRingoid t = testGroup "Ringoid Structure"
  [ testProperty "(a+b)+c == a+(b+c)  " $ prop_rAdd_assoc        t
  , testProperty "a+b == b+a          " $ prop_rAdd_comm         t
  , testProperty "0+a == a            " $ prop_rAdd_lneut        t
  , testProperty "a+0 == a            " $ prop_rAdd_rneut        t
  , testProperty "-(-a) == a          " $ prop_rNeg_invol        t
  , testProperty "(a·b)·c == a·(b·c)  " $ prop_rMul_assoc        t
  , testProperty "a·(b+c) == a·b + a·c" $ prop_rMul_ldist_rAdd   t
  , testProperty "(a+b)·c == a·c + b·c" $ prop_rMul_rdist_rAdd   t
  , testProperty "0·a == 0            " $ prop_rMul_lzero        t
  , testProperty "a·0 == 0            " $ prop_rMul_rzero        t
  ]

testCRingoid :: (RingoidArb t, CRingoidArb t, Show t) => t -> TestTree
testCRingoid t = testGroup "CRingoid Structure"
  [ testProperty "a·b == b·a          " $ prop_rMul_comm         t
  ]

testURingoid :: (RingoidArb t, URingoidArb t, Show t) => t -> TestTree
testURingoid t = testGroup "URingoid Structure"
  [ testProperty "1·a == a            " $ prop_rMul_lone         t
  , testProperty "a·1 == a            " $ prop_rMul_rone         t
  ]

testORingoid :: (RingoidArb t, ORingoidArb t, Show t) => t -> TestTree
testORingoid t = testGroup "ORingoid Structure"
  [ testProperty "aµa == a            " $ prop_rMin_idemp        t
  , testProperty "(aµb)µc == aµ(bµc)  " $ prop_rMin_assoc        t
  , testProperty "aµb == bµa          " $ prop_rMin_comm         t
  , testProperty "aµ(bMc) == aµb M aµc" $ prop_rMin_ldist_rMax   t
  , testProperty "(aMb)µc == aµc M bµc" $ prop_rMin_rdist_rMax   t
  , testProperty "a+(bµc) == a+b µ a+c" $ prop_rAdd_ldist_rMin   t
  , testProperty "(aµb)+c == a+c µ b+c" $ prop_rAdd_rdist_rMin   t

  , testProperty "aMa == a            " $ prop_rMax_idemp        t
  , testProperty "(aMb)Mc == aM(bMc)  " $ prop_rMax_assoc        t
  , testProperty "aMb == bMa          " $ prop_rMax_comm         t
  , testProperty "aM(bµc) == aMb µ aMc" $ prop_rMax_ldist_rMin   t
  , testProperty "(aµb)Mc == aMc µ bMc" $ prop_rMax_rdist_rMin   t
  , testProperty "a+(bMc) == a+b M a+c" $ prop_rAdd_ldist_rMax   t
  , testProperty "(aMb)+c == a+c M b+c" $ prop_rAdd_rdist_rMax   t

  , testProperty "||a|| == |a|        " $ prop_rAbs_idemp        t
  , testProperty "|a·b| == |a| · |b|  " $ prop_rAbs_udist_rMul   t
  , testProperty "|a+b| <= |a| + |b|  " $ prop_rAbs_triangle     t
  ]

testGCDoid :: (RingoidArb t, GCDoidArb t, Show t) => t -> TestTree
testGCDoid t = testGroup "GCDoid Structure"
  [ testProperty "(a;b);c == a;(b;c)  " $ prop_rGCD_assoc        t
  , testProperty "a;b == b;a          " $ prop_rGCD_comm         t
  , testProperty "a;a ~~ a            " $ prop_rGCD_idemp        t
  , testProperty "a·(b;c) ~~ a·b ; a·c" $ prop_rMul_ldist_rGCD   t
  , testProperty "(a;b)·c ~~ a·c ; b·c" $ prop_rMul_rdist_rGCD   t
  , testProperty "0;a ~~ a            " $ prop_rGCD_lneut        t
  , testProperty "a;0 ~~ a            " $ prop_rGCD_rneut        t
  ]

testEDoid :: (RingoidArb t, EDoidArb t, Show t) => t -> TestTree
testEDoid t = testGroup "EDoid Structure"
  [ testProperty "Division Algorithm" $ prop_rQuotRem t
  ]

testBipRingoid :: (RingoidArb t, BipRingoidArb t, Show t) => t -> TestTree
testBipRingoid t = testGroup "BipRingoid Structure"
  [ testProperty "(a÷b)÷c == a÷(b÷c)  " $ prop_rBipIn_assoc       t
  , testProperty "(a÷b)·c == a·c ÷ b·c" $ prop_rMul_rdist_rBipIn  t

  , testProperty "(a|b)|c == a|(b|c)  " $ prop_rBipOut_assoc      t
  , testProperty "a·(b|c) == a·b | a·c" $ prop_rMul_ldist_rBipOut t
  ]



{---------------}
{- :Generators -}
{---------------}

{---------------}
{- :RingoidArb -}
{---------------}

class (Ringoid t, Arbitrary t) => RingoidArb t where
  rElt :: t -> Gen t
  rElt _ = arbitrary

  rLocalElts :: t -> Int -> Gen [t]
  rLocalElts _ n = vectorOf n arbitrary

  rAddAssoc     :: t -> Gen (t,t,t) -- (x+y)+z and x+(y+z) both exist
  rAddComm      :: t -> Gen (t,t)   -- x+y and y+x both exist
  rAddLNeut     :: t -> Gen (t,t)   -- x is left neutral for y wrt rAdd
  rAddRNeut     :: t -> Gen (t,t)   -- y is right neutral for x wrt rAdd

  rMulAssoc     :: t -> Gen (t,t,t) -- (x*y)*z and x*(y*z) both exist
  rMulDistLrAdd :: t -> Gen (t,t,t) -- x*y + x*z and x*(y+z) both exist
  rMulDistRrAdd :: t -> Gen (t,t,t) -- x*z + y*z and (x+y)*z both exist
  rMulLZero     :: t -> Gen (t,t)   -- x is left zero of y
  rMulRZero     :: t -> Gen (t,t)   -- y is right zero of x

  rAddLNeut     a = do
    y <- arbitrary
    case rNeutOf y of
      Left _ -> error "rAddLNeut default"
      Right x -> return (x,y)

  rAddRNeut     a = do
    x <- arbitrary
    case rNeutOf x of
      Left _ -> error "rAddRNeut default"
      Right y -> return (x,y)

  rMulLZero     a = do
    y <- arbitrary
    case rLAnnOf y of
      Left _ -> error "rMulLZero default"
      Right x -> return (x,y)

  rMulRZero     a = do
    x <- arbitrary
    case rRAnnOf x of
      Left _ -> error "rMulRZero default"
      Right y -> return (x,y)

  -- Defaults: Assume ringoid is total.
  rAddAssoc     a = arb3 a
  rAddComm      a = arb2 a
  rMulAssoc     a = arb3 a
  rMulDistLrAdd a = arb3 a
  rMulDistRrAdd a = arb3 a


{----------------}
{- :CRingoidArb -}
{----------------}

class (CRingoid t, Arbitrary t) => CRingoidArb t where
  rMulComm      :: t -> Gen (t,t)   -- x*y and y*x both exist

  -- Default: Assume ringoid is total
  rMulComm a = arb2 a


{----------------}
{- :URingoidArb -}
{----------------}

class (URingoid t, Arbitrary t) => URingoidArb t where
  rMulLOne      :: t -> Gen (t,t)   -- x is left one of y
  rMulROne      :: t -> Gen (t,t)   -- y is right one of x


{----------------}
{- :ORingoidArb -}
{----------------}

class (ORingoid t, Arbitrary t) => ORingoidArb t where
  rMinIdemp     :: t -> Gen t       -- xµx exists
  rMinAssoc     :: t -> Gen (t,t,t) -- (xµy)µz and xµ(yµz) both exist
  rMinComm      :: t -> Gen (t,t)   -- xµy and yµx both exist
  rMinDistLrMax :: t -> Gen (t,t,t) -- xµ(yMz) and xµy M xµz both exist
  rMinDistRrMax :: t -> Gen (t,t,t) -- (xMy)µz and xµz M yµz both exist
  rAddDistLrMin :: t -> Gen (t,t,t) -- x+(yµz) and x+y µ x+z both exist
  rAddDistRrMin :: t -> Gen (t,t,t) -- (xµy)+z and x+z µ y+z both exist

  rMaxIdemp     :: t -> Gen t       -- xMx exists
  rMaxAssoc     :: t -> Gen (t,t,t) -- (xMy)Mz and xM(yMz) both exist
  rMaxComm      :: t -> Gen (t,t)   -- xMy and yMx both exist
  rMaxDistLrMin :: t -> Gen (t,t,t) -- xM(yµz) and xMy µ xMz both exist
  rMaxDistRrMin :: t -> Gen (t,t,t) -- (xµy)Mz and xMz µ yMz both exist
  rAddDistLrMax :: t -> Gen (t,t,t) -- x+(yMz) and x+y M x+z both exist
  rAddDistRrMax :: t -> Gen (t,t,t) -- (xMy)+z and x+z M y+z both exist

  rAbsDistUrMul :: t -> Gen (t,t)   -- |ab| and |a||b| both exist
  rAbsTriangle  :: t -> Gen (t,t)   -- |a+b| and |a|+|b| both exist
  rAbsIdemp     :: t -> Gen t       -- |a| and ||a|| both exist

  -- Defaults: Assume Ringoid is total.
  rMinIdemp     _ = arbitrary
  rMinAssoc     a = arb3 a
  rMinComm      a = arb2 a
  rMinDistLrMax a = arb3 a
  rMinDistRrMax a = arb3 a
  rAddDistLrMin a = arb3 a
  rAddDistRrMin a = arb3 a

  rMaxIdemp     _ = arbitrary
  rMaxAssoc     a = arb3 a
  rMaxComm      a = arb2 a
  rMaxDistLrMin a = arb3 a
  rMaxDistRrMin a = arb3 a
  rAddDistLrMax a = arb3 a
  rAddDistRrMax a = arb3 a

  rAbsDistUrMul a = arb2 a
  rAbsTriangle  a = arb2 a
  rAbsIdemp     _ = arbitrary


{--------------}
{- :GCDoidArb -}
{--------------}

class (URingoidAssoc t, GCDoid t, Arbitrary t) => GCDoidArb t where
  rGCDAssoc     :: t -> Gen (t,t,t) -- (a;b);c and a;(b;c) both exist
  rGCDComm      :: t -> Gen (t,t)   -- a;b and b;a both exist
  rGCDIdemp     :: t -> Gen t       -- a;a exists
  rGCDLNeut     :: t -> Gen (t,t)   -- x is left neutral for y wrt GCD
  rGCDRNeut     :: t -> Gen (t,t)   -- y is right neutral for x wrt GCD
  rMulDistLrGCD :: t -> Gen (t,t,t) -- a(b;c) and ab;ac both exist
  rMulDistRrGCD :: t -> Gen (t,t,t) -- (a;b)c and ac;bc both exist

  -- Defaults: Assume Ringoid is total.
  rGCDAssoc     a = arb3 a
  rGCDComm      a = arb2 a
  rGCDIdemp     _ = arbitrary
  rMulDistLrGCD a = arb3 a
  rMulDistRrGCD a = arb3 a



{-------------}
{- :EDoidArb -}
{-------------}


class (Ringoid t, EDoid t, Arbitrary t) => EDoidArb t where
  rQuotRem :: t -> Gen (t,t) -- a and b are compatible for division algorithm and b /= 0



{------------------}
{- :BipRingoidArb -}
{------------------}

class (Ringoid t, BipRingoid t, Arbitrary t) => BipRingoidArb t where
  rBipInAssoc      :: t -> Gen (t,t,t)
  rMulDistRrBipIn  :: t -> Gen (t,t,t)

  rBipOutAssoc     :: t -> Gen (t,t,t)
  rMulDistLrBipOut :: t -> Gen (t,t,t)



{---------------}
{- :Properties -}
{---------------}

{------------}
{- :Ringoid -}
{------------}

{- :Add -}

prop_rAdd_assoc :: (RingoidArb t, Show t) => t -> Property
prop_rAdd_assoc t = forAll (rAddAssoc t) (isAssociativeBy rAdd rEQ)

prop_rAdd_comm :: (RingoidArb t, Show t) => t -> Property
prop_rAdd_comm t = forAll (rAddComm t) (isCommutativeBy rAdd rEQ)

prop_rAdd_lneut :: (RingoidArb t, Show t) => t -> Property
prop_rAdd_lneut t = forAll (rAddLNeut t) (isLIdentityUnderBy rAdd rEQ)

prop_rAdd_rneut :: (RingoidArb t, Show t) => t -> Property
prop_rAdd_rneut t = forAll (rAddRNeut t) (isRIdentityUnderBy rAdd rEQ)

{- :Neg -}

prop_rNeg_invol :: (RingoidArb t, Show t) => t -> Property
prop_rNeg_invol t = forAll (rElt t) (isInvolutiveUBy rNeg rEQ)

{- :Mul -}

prop_rMul_assoc :: (RingoidArb t, Show t) => t -> Property
prop_rMul_assoc t = forAll (rMulAssoc t) (isAssociativeBy rMul rEQ)

prop_rMul_ldist_rAdd :: (RingoidArb t, Show t) => t -> Property
prop_rMul_ldist_rAdd t = forAll (rMulDistLrAdd t) (rMul `isLDistributiveOverBy` rAdd $ rEQ)

prop_rMul_rdist_rAdd :: (RingoidArb t, Show t) => t -> Property
prop_rMul_rdist_rAdd t = forAll (rMulDistRrAdd t) (rMul `isRDistributiveOverBy` rAdd $ rEQ)

prop_rMul_lzero :: (RingoidArb t, Show t) => t -> Property
prop_rMul_lzero t = forAll (rMulLZero t) (rIsZero `isSatisfiedAfter` rMul)

prop_rMul_rzero :: (RingoidArb t, Show t) => t -> Property
prop_rMul_rzero t = forAll (rMulRZero t) (rIsZero `isSatisfiedAfter` rMul)



{-------------}
{- :CRingoid -}
{-------------}

prop_rMul_comm :: (RingoidArb t, CRingoidArb t, Show t) => t -> Property
prop_rMul_comm t = forAll (rMulComm t) (rMul `isCommutativeBy` rEQ)



{-------------}
{- :URingoid -}
{-------------}

prop_rMul_lone :: (RingoidArb t, URingoidArb t, Show t) => t -> Property
prop_rMul_lone t = forAll (rMulLOne t) (isLIdentityUnderBy rMul rEQ)

prop_rMul_rone :: (RingoidArb t, URingoidArb t, Show t) => t -> Property
prop_rMul_rone t = forAll (rMulROne t) (isRIdentityUnderBy rMul rEQ)



{-------------}
{- :ORingoid -}
{-------------}

{- :Min -}

prop_rMin_idemp :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rMin_idemp t = forAll (rMinIdemp t) (rMin `isIdempotentBy` rEQ)

prop_rMin_assoc :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rMin_assoc t = forAll (rMinAssoc t) (rMin `isAssociativeBy` rEQ)

prop_rMin_comm :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rMin_comm t = forAll (rMinComm t) (rMin `isCommutativeBy` rEQ)

prop_rMin_ldist_rMax :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rMin_ldist_rMax t = forAll (rMinDistLrMax t) (isLDistributiveOverBy rMin rMax rEQ)

prop_rMin_rdist_rMax :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rMin_rdist_rMax t = forAll (rMinDistRrMax t) (isRDistributiveOverBy rMin rMax rEQ)

prop_rAdd_ldist_rMin :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rAdd_ldist_rMin t = forAll (rAddDistLrMin t) (isLDistributiveOverBy rAdd rMin rEQ)

prop_rAdd_rdist_rMin :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rAdd_rdist_rMin t = forAll (rAddDistRrMin t) (isRDistributiveOverBy rAdd rMin rEQ)

{- :Max -}

prop_rMax_idemp :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rMax_idemp t = forAll (rMaxIdemp t) (rMax `isIdempotentBy` rEQ)

prop_rMax_assoc :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rMax_assoc t = forAll (rMaxAssoc t) (rMax `isAssociativeBy` rEQ)

prop_rMax_comm :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rMax_comm t = forAll (rMaxComm t) (rMax `isCommutativeBy` rEQ)

prop_rMax_ldist_rMin :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rMax_ldist_rMin t = forAll (rMaxDistLrMin t) (isLDistributiveOverBy rMax rMin rEQ)

prop_rMax_rdist_rMin :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rMax_rdist_rMin t = forAll (rMaxDistRrMin t) (isRDistributiveOverBy rMax rMin rEQ)

prop_rAdd_ldist_rMax :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rAdd_ldist_rMax t = forAll (rAddDistLrMax t) (isLDistributiveOverBy rAdd rMax rEQ)

prop_rAdd_rdist_rMax :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rAdd_rdist_rMax t = forAll (rAddDistRrMax t) (isRDistributiveOverBy rAdd rMax rEQ)

{- :Abs -}

prop_rAbs_idemp :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rAbs_idemp t = forAll (rAbsIdemp t) (rAbs `isIdempotentUBy` rEQ)

prop_rAbs_udist_rMul :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rAbs_udist_rMul t = forAll (rAbsDistUrMul t) (rAbs `isDistributiveUOverBy` rMul $ rEQ)

prop_rAbs_triangle :: (RingoidArb t, ORingoidArb t, Show t) => t -> Property
prop_rAbs_triangle t = forAll (rAbsTriangle t) (rAbs `isDistributiveUOverBy` rAdd $ rLEQ)



{-----------}
{- :GCDoid -}
{-----------}

prop_rGCD_assoc :: (RingoidArb t, GCDoidArb t, Show t) => t -> Property
prop_rGCD_assoc t = forAll (rGCDAssoc t) (rGCD `isAssociativeBy` rEQ)

prop_rGCD_comm :: (RingoidArb t, GCDoidArb t, Show t) => t -> Property
prop_rGCD_comm t = forAll (rGCDComm t) (rGCD `isCommutativeBy` rEQ)

prop_rGCD_idemp :: (URingoidAssoc t, GCDoidArb t, Show t) => t -> Property
prop_rGCD_idemp t = forAll (rGCDIdemp t) (rGCD `isIdempotentBy` rAssoc)

prop_rGCD_lneut :: (URingoidAssoc t, GCDoidArb t, Show t) => t -> Property
prop_rGCD_lneut t = forAll (rGCDLNeut t) (isLIdentityUnderBy rGCD rAssoc)

prop_rGCD_rneut :: (URingoidAssoc t, GCDoidArb t, Show t) => t -> Property
prop_rGCD_rneut t = forAll (rGCDRNeut t) (isRIdentityUnderBy rGCD rAssoc)

prop_rMul_ldist_rGCD :: (URingoidAssoc t, RingoidArb t, GCDoidArb t, Show t) => t -> Property
prop_rMul_ldist_rGCD t = forAll (rMulDistLrGCD t) (rMul `isLDistributiveOverBy` rGCD $ rAssoc)

prop_rMul_rdist_rGCD :: (URingoidAssoc t, RingoidArb t, GCDoidArb t, Show t) => t -> Property
prop_rMul_rdist_rGCD t = forAll (rMulDistRrGCD t) (rMul `isRDistributiveOverBy` rGCD $ rAssoc)



{----------}
{- :EDoid -}
{----------}

prop_rQuotRem :: (Ringoid t, EDoid t, EDoidArb t, Show t) => t -> Property
prop_rQuotRem t = forAll (rQuotRem t) (testDivAlgBy rDivAlg rMul rAdd rNorm rEQ rIsZero)



{---------------}
{- :BipRingoid -}
{---------------}

prop_rBipIn_assoc :: (RingoidArb t, BipRingoidArb t, Show t) => t -> Property
prop_rBipIn_assoc t = forAll (rBipInAssoc t) (rBipIn `isAssociativeBy` rEQ)

prop_rMul_rdist_rBipIn :: (RingoidArb t, BipRingoidArb t, Show t) => t -> Property
prop_rMul_rdist_rBipIn t = forAll (rMulDistRrBipIn t) (rMul `isRDistributiveOverBy` rBipIn $ rEQ)

prop_rBipOut_assoc :: (RingoidArb t, BipRingoidArb t, Show t) => t -> Property
prop_rBipOut_assoc t = forAll (rBipOutAssoc t) (rBipOut `isAssociativeBy` rEQ)

prop_rMul_ldist_rBipOut :: (RingoidArb t, BipRingoidArb t, Show t) => t -> Property
prop_rMul_ldist_rBipOut t = forAll (rMulDistLrBipOut t) (rMul `isLDistributiveOverBy` rBipOut $ rEQ)