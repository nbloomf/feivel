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

import Test.Framework (defaultMain, testGroup)

import Feivel.Lib.Rat (Rat((:/:)))

import Tests.Lib.Integer
import Tests.Lib.Rat
import Tests.Lib.ZZModulo

import Tests.Lib.Matrix
import Tests.Lib.Polynomial

main = defaultMain
  [ testInteger
  , testRat
  , testZZModulo

  , testGroup "Integer Matrix"
      [ testRingoidMat    (0::Integer)
      , testBipRingoidMat (0::Integer)
      ]

  , testGroup "Rat Matrix"
      [ testRingoidMat     (0:/:1)
      , testBipRingoidMat  (0:/:1)
      ]

  , testGroup "Integer Polynomial"
      [ testRingoidPoly (0::Integer)
      ]

  , testGroup "Rat Polynomial"
      [ testRingoidPoly (0:/:1)
      ]
  ]
