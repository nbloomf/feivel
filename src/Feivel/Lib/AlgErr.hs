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

module Feivel.Lib.AlgErr where

data AlgErr
  = RingoidOK
  | RingoidAddErr String
  | RingoidMulErr String
  | RingoidNegErr String

  | RingoidDivideByZeroErr String
  | RingoidNotInvertibleErr String
  | RingoidNegativeExponentErr Integer
  | RingoidZeroExponentErr

  | RingoidEmptyListErr String
  | RingoidSingletonListErr String

  | EvenRootOfNegative String

  | MatCoefErr String

  | PolyDivErr String
  | PolyNotConstant String

  -- Structural Errors
  | NullMatrix String
  | MalformedMatrix
  | InvalidIndex String -- Index Dim
  | InvalidRowIndex String -- Integer Dim
  | InvalidColIndex String -- Integer Dim
  | DimMismatch String -- Dim Dim
  | EmptyMatrixList

  -- Arithmetic Errors
  | NegativeExponent
  | NonSquareMatrix
  | ZeroMatrix
  | EmptyMatrixProduct
  | NonInvertibleMatrix

  | NotACycle
  | NotDisjoint

  | DifferentModulus
  deriving (Eq, Show)