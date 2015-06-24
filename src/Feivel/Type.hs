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

module Feivel.Type (
  Type(..), TypeErr(..), unify, unifyAll
) where

import Control.Monad (foldM)
import Control.Monad.Instances ()

{--------------}
{- Contents   -}
{-   :Type    -}
{-   :TypeErr -}
{--------------}

{---------}
{- :Type -}
{---------}

data Type
  = XX -- Type Variable
  | DD -- Doc

  | ZZ -- Integer
  | SS -- String
  | BB -- Boolean
  | QQ -- Rational

  | ListOf   Type -- List
  | MatOf    Type -- Matrix
  | PolyOver Type -- Polynomial
  | PermOf   Type -- Permutation

  | MacTo  Type -- Macro
  deriving Eq


unify :: Type -> Type -> Either TypeErr Type
unify XX t  = Right t
unify t  XX = Right t

unify DD DD = Right DD
unify ZZ ZZ = Right ZZ
unify SS SS = Right SS
unify BB BB = Right BB
unify QQ QQ = Right QQ

unify (ListOf a) (ListOf b) = do
  t <- unify a b
  return (ListOf t)

unify (MatOf a) (MatOf b) = do
  t <- unify a b
  return (MatOf t)

unify (PolyOver a) (PolyOver b) = do
  t <- unify a b
  return (PolyOver t)

unify (PermOf a) (PermOf b) = do
  t <- unify a b
  return (PermOf t)

unify (MacTo a) (MacTo b) = do
  t <- unify a b
  return (MacTo t)

unify a b = Left $ TypeUnificationError a b


unifyAll :: [Type] -> Either TypeErr Type
unifyAll = foldM unify XX
  

instance Show Type where
  show XX = "x"

  show DD = "doc"

  show ZZ = "int"
  show SS = "str"
  show BB = "bool"
  show QQ = "rat"

  show (ListOf   t) = "{" ++ show t ++ "}"
  show (MatOf    t) = "[" ++ show t ++ "]"
  show (PolyOver t) = "^" ++ show t
  show (PermOf   t) = "$" ++ show t

  show (MacTo    t) = ">" ++ show t



{------------}
{- :TypeErr -}
{------------}

data TypeErr
  = TypeMismatch          Type Type -- expected, received

  | ListExpected          Type
  | NumericListExpected   Type
  | SortableListExpected  Type
  | MatrixListExpected    Type
  | MacroListExpected     Type
  | ListListExpected      Type

  | MatrixExpected        Type
  | NumericMatrixExpected Type
  | MacroMatrixExpected   Type
  | ListMatrixExpected    Type
  | FieldMatrixExpected   Type
  | MatrixMatrixExpected  Type

  | MacroExpected         Type

  | SortableExpected      Type

  | PolynomialExpected    Type
  | NumericPolynomialExpected Type
  | PolynomialListExpected Type
  | PolynomialMatrixExpected Type

  | PermutationExpected Type
  | PermutationListExpected Type
  | PermutationMatrixExpected Type

  | TypeUnificationError  Type Type
  deriving Eq



instance Show TypeErr where
  show (TypeMismatch ex re) =
    "An expression of type " ++ show ex ++ " was expected, but this expression has type "
     ++ show re ++ "."


  show (ListExpected t) =
    "A list was expected, but this expression has type " ++ show t ++ "."

  show (NumericListExpected t) =
    "A numeric list was expected, but this expression has type " ++ show t ++ "."

  show (SortableListExpected t) =
    "A sortable list was expected, but this expression has type " ++ show t ++ "."

  show (MatrixListExpected t) =
    "A list of matrices was expected, but this expression has type " ++ show t ++ "."

  show (MacroListExpected t) =
    "A list of macros was expected, but this expression has type " ++ show t ++ "."

  show (ListListExpected t) =
    "A list of lists was expected, but this expression has type " ++ show t ++ "."


  show (MatrixExpected t) =
    "A matrix was expected, but this expression has type " ++ show t ++ "."

  show (NumericMatrixExpected t) =
    "A numeric matrix was expected, but this expression has type " ++ show t ++ "."

  show (MacroMatrixExpected t) =
    "A matrix of macros was expected, but this expression has type " ++ show t ++ "."

  show (ListMatrixExpected t) =
    "A matrix of lists was expected, but this expression has type " ++ show t ++ "."

  show (FieldMatrixExpected t) =
    "A matrix over a field was expected, but this expression has type " ++ show t ++ "."

  show (MatrixMatrixExpected t) =
    "A matrix of matrices was expected, but this expression has type " ++ show t ++ "."


  show (MacroExpected t) = 
    "A macro was expected, but thus expression has type " ++ show t ++ "."

  show (SortableExpected t) =
    "A sortable type was expected, but this expression has type " ++ show t ++ "."


  show (PolynomialExpected t) =
    "A polynomial was expected, but this expression has type " ++ show t ++ "."

  show (NumericPolynomialExpected t) =
    "A numeric polynomial was expected, but this expression has type " ++ show t ++ "."

  show (PolynomialListExpected t) =
    "A list of polynomials was expected, but this expression has type " ++ show t ++ "."

  show (PolynomialMatrixExpected t) =
    "A matrix of polynomials was expected, but this expression has type " ++ show t ++ "."


  show (PermutationExpected t) =
    "A permutation was expected, but this expression has type " ++ show t ++ "."

  show (PermutationListExpected t) =
    "A list of permutations was expected, but this expression has type " ++ show t ++ "."

  show (PermutationMatrixExpected t) =
    "A matrix of permutations was expected, but this expression has type " ++ show t ++ "."


  show (TypeUnificationError a b) =
    "Cannot unify types: " ++ show a ++ " and " ++ show b ++ "."