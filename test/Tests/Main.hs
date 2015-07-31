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

module Tests.Main where

import Test.Tasty (defaultMain, testGroup)

import Feivel.Lib (Rat((:/:)), zzmod)

import Tests.Lib.Data.Integer
import Tests.Lib.Data.Rat
import Tests.Lib.Data.ZZModulo

import Tests.Lib.Struct.Matrix
import Tests.Lib.Struct.Polynomial
import Tests.Lib.Struct.Permutation

main = defaultMain $ testGroup "Property Tests"
  [ testInteger
  , testRat
  , testZZModulo

  , testGroup "Matrices"
      [ testGroup "Integer Matrix"
          [ testRingoidMat    (0::Integer)
          , testBipRingoidMat (0::Integer)
          ]

      , testGroup "Rat Matrix"
          [ testRingoidMat    (0:/:1)
          , testBipRingoidMat (0:/:1)
          ]

      , testGroup "ZZMod Matrix"
          [ testRingoidMat    (0`zzmod`0)
          , testBipRingoidMat (0`zzmod`0)
          ]
      ]

  , testGroup "Polynomials"
      [ testGroup "Integer Polynomial"
          [ testRingoidPoly  (0::Integer)
          , testCRingoidPoly (0::Integer)
          ]

      , testGroup "Rat Polynomial"
          [ testRingoidPoly  (0:/:1)
          , testCRingoidPoly (0:/:1)
          --, testEDoidPoly    (0:/:1)
          ]

      , testGroup "ZZMod Polynomial"
          [ testRingoidPoly  (0`zzmod`0)
          , testCRingoidPoly (0`zzmod`0)
          ]
      ]

  , testGroup "Permutations"
      [ testGroup "Integer Permutation"
          [ testGroupoidPerm (0::Integer)
          ]
      ]
  ]
