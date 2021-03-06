{---------------------------------------------------------------------}
{- Copyright 2015, 2016 Nathan Bloomfield                            -}
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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Feivel.Grammar.Type (
  Type(..), TypeErr(..), unify, unifyAll, Typed, typeOf, OfType(..)
) where

import Carl.String
import Carl.Data.ZZMod
import Carl.Data.Rat
import Carl.Struct.Polynomial
import Carl.Struct.Matrix

import Data.List (intersperse)
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

data OfType a = a :# Type deriving (Eq, Show)

class Typed t where
  typeOf :: t -> Type

data Type
  = XX -- Type Variable

  -- Atomic types
  | DD -- Doc
  | ZZ -- Integers
  | SS -- Strings
  | BB -- Booleans
  | QQ -- Rationals

  -- Parameterized types
  | ZZMod  Integer -- Modular Integers
  | PermOf Type    -- Permutations

  -- Constructed types
  | ListOf   Type   -- Lists
  | MatOf    Type   -- Matrices
  | PolyOver Type   -- Polynomials
  | TupleOf  [Type] -- Tuples
  | MacTo    Type   -- Macros
  deriving Eq


unify :: Type -> Type -> Either TypeErr Type
unify XX t = Right t
unify t XX = Right t
unify t1 t2 = if t1==t2
  then Right t1
  else Left $ TypeUnificationError t1 t2


unifyAll :: [Type] -> Either TypeErr Type
unifyAll = foldM unify XX
  

instance Show Type where
  show XX = "x"

  show DD = "doc"

  show ZZ = "int"
  show SS = "str"
  show BB = "bool"
  show QQ = "rat"

  show (ZZMod n) = "mod" ++ show (abs n)

  show (ListOf   t) = "{" ++ show t ++ "}"
  show (MatOf    t) = "[" ++ show t ++ "]"
  show (PolyOver t) = "^" ++ show t
  show (PermOf   t) = "$" ++ show t

  show (TupleOf ts) = "(" ++ (concat $ intersperse "," $ map show ts) ++ ")"

  show (MacTo    t) = ">" ++ show t



{------------}
{- :TypeErr -}
{------------}

data TypeErr
  = TypeMismatch          Type Type -- expected, received

  | NumericTypeExpected   Type
  | FieldTypeExpected     Type

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

  | ModularIntegerExpected Type
  | ModularIntegerListExpected Type
  | ModularIntegerMatrixExpected Type

  | TypeUnificationError  Type Type
  deriving Eq



instance Show TypeErr where
  show (TypeMismatch ex re) =
    "An expression of type " ++ show ex ++ " was expected, but this expression has type "
     ++ show re ++ "."

  show (NumericTypeExpected u) =
    "A numeric type was expected, but this expression has type " ++ show u ++ "."

  show (FieldTypeExpected u) =
    "A field type was expected, but this expression has type " ++ show u ++ "."


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

  show (ModularIntegerExpected t) =
    "A modular integer was expected, but this expression has type " ++ show t ++ "."

  show (ModularIntegerListExpected t) =
    "A list of modular integers was expected, but this expression has type " ++ show t ++ "."

  show (ModularIntegerMatrixExpected t) =
    "A matrix of modular integers was expected, but this expression has type " ++ show t ++ "."


  show (TypeUnificationError a b) =
    "Cannot unify types: " ++ show a ++ " and " ++ show b ++ "."

instance Typed Integer  where typeOf _              = ZZ
instance Typed Text     where typeOf _              = SS
instance Typed Bool     where typeOf _              = BB
instance Typed Rat      where typeOf _              = QQ
instance Typed ZZModulo where typeOf (ZZModulo _ n) = ZZMod n

instance (Typed a) => Typed (Poly VarString a) where
  typeOf x = case getCoefficients x of
    (c:_) -> PolyOver (typeOf c)
    []    -> PolyOver XX

instance (Typed a) => Typed (Matrix a) where
  typeOf x = case toListM x of
    (a:_) -> MatOf (typeOf a)
    []    -> MatOf XX

instance (Typed a) => Typed [a] where
  typeOf (x:_) = typeOf x
  typeOf []    = XX

