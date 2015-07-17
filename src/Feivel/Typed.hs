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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Feivel.Typed (
  Typed, typeOf,

  unifyTypesOf,

  expectMatrix
) where

import Feivel.Type
import Feivel.Expr
import Feivel.Locus
import Feivel.Error
import Feivel.EvalM
import Feivel.Lib

{---------------}
{- :Instances  -}
{-   :ListExpr -}
{-   :MacExpr  -}
{-   :MatExpr  -}
{-   :PolyExpr -}
{-   :PermExpr -}
{---------------}


{----------}
{- :Typed -}
{----------}

class Typed t where
  typeOf :: t -> Type



unifyTypesOf :: (Typed a, Typed b) => Locus -> a -> b -> EvalM Type
unifyTypesOf loc a b = do
  let ta = typeOf a
  let tb = typeOf b
  case unify ta tb of
    Left err -> reportErr loc err
    Right t -> return t

expectType :: (Typed a) => Locus -> Type -> a -> EvalM Type
expectType loc t x = do
  let u = typeOf x
  if t == u
    then return u
    else reportErr loc $ TypeMismatch t u

expectMatrix :: (Typed a) => Locus -> a -> EvalM Type
expectMatrix loc x = do
  let u = typeOf x
  case u of
    MatOf t -> return t
    _ -> reportErr loc $ MatrixExpected u

sameType :: (Typed a, Typed b) => Locus -> a -> b -> EvalM Type
sameType loc a b = do
  let ta = typeOf a
  let tb = typeOf b
  if ta == tb
    then return ta
    else reportErr loc $ TypeMismatch ta tb


{--------------}
{- :Instances -}
{--------------}

instance Typed Integer  where typeOf _              = ZZ
instance Typed Text     where typeOf _              = SS
instance Typed Bool     where typeOf _              = BB
instance Typed Rat      where typeOf _              = QQ
instance Typed ZZModulo where typeOf (ZZModulo _ n) = ZZMod n

instance Typed (Poly Integer) where typeOf _ = PolyOver ZZ
instance Typed (Poly Rat)     where typeOf _ = PolyOver QQ
instance Typed (Poly Bool)    where typeOf _ = PolyOver BB


instance Typed IntExpr  where typeOf _ = ZZ
instance Typed StrExpr  where typeOf _ = SS
instance Typed BoolExpr where typeOf _ = BB
instance Typed RatExpr  where typeOf _ = QQ
instance Typed Doc      where typeOf _ = DD

instance Typed Expr where
  typeOf (StrE   x) = typeOf x
  typeOf (IntE   x) = typeOf x
  typeOf (RatE   x) = typeOf x
  typeOf (BoolE  x) = typeOf x
  typeOf (ListE  x) = typeOf x
  typeOf (MacE   x) = typeOf x
  typeOf (DocE   x) = typeOf x
  typeOf (MatE   x) = typeOf x
  typeOf (PolyE  x) = typeOf x
  typeOf (PermE  x) = typeOf x
  typeOf (ZZModE x) = typeOf x


{-------------}
{- :ListExpr -}
{-------------}

instance Typed ListExpr where
  typeOf (ListConst           typ _     :@ _) = ListOf typ
  typeOf (ListVar             typ _     :@ _) = ListOf typ
  typeOf (ListIfThenElse      typ _ _ _ :@ _) = ListOf typ
  typeOf (ListRand            typ _     :@ _) = ListOf typ
  typeOf (ListAtPos           typ _ _   :@ _) = ListOf typ
  typeOf (ListAtIdx           typ _ _ _ :@ _) = ListOf typ
  typeOf (ListMacro           typ _ _   :@ _) = ListOf typ
  typeOf (ListCat             typ _ _   :@ _) = ListOf typ
  typeOf (ListToss            typ _ _   :@ _) = ListOf typ
  typeOf (ListRev             typ _     :@ _) = ListOf typ
  typeOf (ListSort            typ _     :@ _) = ListOf typ
  typeOf (ListUniq            typ _     :@ _) = ListOf typ
  typeOf (ListShuffle         typ _     :@ _) = ListOf typ
  typeOf (ListFilter          typ _ _ _ :@ _) = ListOf typ
  typeOf (ListMatRow          typ _ _   :@ _) = ListOf typ
  typeOf (ListMatCol          typ _ _   :@ _) = ListOf typ
  typeOf (ListChoose          typ _ _   :@ _) = ListOf typ
  typeOf (ListShuffles        typ _     :@ _) = ListOf typ
  typeOf (ListChoices         typ _ _   :@ _) = ListOf typ
  typeOf (ListRange           typ _ _   :@ _) = ListOf typ
  typeOf (ListPermsOf         typ _     :@ _) = ListOf typ
  typeOf (ListBuilder         typ _ _   :@ _) = ListOf typ
  typeOf (ListPivotColIndices typ _     :@ _) = ListOf typ



{------------}
{- :MacExpr -}
{------------}

instance Typed MacExpr where
  typeOf (MacConst      typ _ _ _ :@ _) = MacTo typ
  typeOf (MacVar        typ _     :@ _) = MacTo typ
  typeOf (MacMacro      typ _ _   :@ _) = MacTo typ
  typeOf (MacAtPos      typ _ _   :@ _) = MacTo typ
  typeOf (MacAtIdx      typ _ _ _ :@ _) = MacTo typ
  typeOf (MacRand       typ _     :@ _) = MacTo typ
  typeOf (MacIfThenElse typ _ _ _ :@ _) = MacTo typ



{------------}
{- :MatExpr -}
{------------}

instance Typed MatExpr where
  typeOf (MatVar         typ _         :@ _) = MatOf typ
  typeOf (MatMacro       typ _ _       :@ _) = MatOf typ
  typeOf (MatAtPos       typ _ _       :@ _) = MatOf typ
  typeOf (MatAtIdx       typ _ _ _     :@ _) = MatOf typ
  typeOf (MatIfThenElse  typ _ _ _     :@ _) = MatOf typ
  typeOf (MatConst       typ _         :@ _) = MatOf typ
  typeOf (MatId          typ _         :@ _) = MatOf typ
  typeOf (MatSwapE       typ _ _ _     :@ _) = MatOf typ
  typeOf (MatScaleE      typ _ _ _     :@ _) = MatOf typ
  typeOf (MatAddE        typ _ _ _ _   :@ _) = MatOf typ
  typeOf (MatHCat        typ _ _       :@ _) = MatOf typ
  typeOf (MatVCat        typ _ _       :@ _) = MatOf typ
  typeOf (MatAdd         typ _ _       :@ _) = MatOf typ
  typeOf (MatMul         typ _ _       :@ _) = MatOf typ
  typeOf (MatPow         typ _ _       :@ _) = MatOf typ
  typeOf (MatTrans       typ _         :@ _) = MatOf typ
  typeOf (MatNeg         typ _         :@ _) = MatOf typ
  typeOf (MatSwapRows    typ _ _ _     :@ _) = MatOf typ
  typeOf (MatSwapCols    typ _ _ _     :@ _) = MatOf typ
  typeOf (MatScaleRow    typ _ _ _     :@ _) = MatOf typ
  typeOf (MatScaleCol    typ _ _ _     :@ _) = MatOf typ
  typeOf (MatAddRow      typ _ _ _ _   :@ _) = MatOf typ
  typeOf (MatAddCol      typ _ _ _ _   :@ _) = MatOf typ
  typeOf (MatDelRow      typ _ _       :@ _) = MatOf typ
  typeOf (MatDelCol      typ _ _       :@ _) = MatOf typ
  typeOf (MatShuffleRows typ _         :@ _) = MatOf typ
  typeOf (MatShuffleCols typ _         :@ _) = MatOf typ
  typeOf (MatGJForm      typ _         :@ _) = MatOf typ
  typeOf (MatGJFactor    typ _         :@ _) = MatOf typ
  typeOf (MatGetRow      typ _ _       :@ _) = MatOf typ
  typeOf (MatGetCol      typ _ _       :@ _) = MatOf typ
  typeOf (MatRowFromList typ _         :@ _) = MatOf typ
  typeOf (MatColFromList typ _         :@ _) = MatOf typ
  typeOf (MatRand        typ _         :@ _) = MatOf typ
  typeOf (MatBuilder     typ _ _ _ _ _ :@ _) = MatOf typ



{-------------}
{- :PolyExpr -}
{-------------}

instance Typed PolyExpr where
  typeOf (PolyVar        typ _     :@ _) = PolyOver typ
  typeOf (PolyMacro      typ _ _   :@ _) = PolyOver typ
  typeOf (PolyConst      typ _     :@ _) = PolyOver typ
  typeOf (PolyAdd        typ _ _   :@ _) = PolyOver typ
  typeOf (PolySub        typ _ _   :@ _) = PolyOver typ
  typeOf (PolyMul        typ _ _   :@ _) = PolyOver typ
  typeOf (PolyNeg        typ _     :@ _) = PolyOver typ
  typeOf (PolyPow        typ _ _   :@ _) = PolyOver typ
  typeOf (PolyAtPos      typ _ _   :@ _) = PolyOver typ
  typeOf (PolyAtIdx      typ _ _ _ :@ _) = PolyOver typ
  typeOf (PolyIfThenElse typ _ _ _ :@ _) = PolyOver typ
  typeOf (PolyRand       typ _     :@ _) = PolyOver typ
  typeOf (PolyFromRoots  typ _ _   :@ _) = PolyOver typ
  typeOf (PolyEvalPoly   typ _ _   :@ _) = PolyOver typ



{-------------}
{- :PermExpr -}
{-------------}

instance Typed PermExpr where
  typeOf (PermVar        typ _     :@ _) = PermOf typ
  typeOf (PermMacro      typ _ _   :@ _) = PermOf typ
  typeOf (PermConst      typ _     :@ _) = PermOf typ
  typeOf (PermAtPos      typ _ _   :@ _) = PermOf typ
  typeOf (PermAtIdx      typ _ _ _ :@ _) = PermOf typ
  typeOf (PermIfThenElse typ _ _ _ :@ _) = PermOf typ
  typeOf (PermRand       typ _     :@ _) = PermOf typ
  typeOf (PermCompose    typ _ _   :@ _) = PermOf typ
  typeOf (PermInvert     typ _     :@ _) = PermOf typ



{--------------}
{- :ZZModExpr -}
{--------------}

instance Typed ZZModExpr where
  typeOf (ZZModConst      typ _     :@ _) = typ
  typeOf (ZZModVar        typ _     :@ _) = typ
  typeOf (ZZModAtPos      typ _ _   :@ _) = typ
  typeOf (ZZModAtIdx      typ _ _ _ :@ _) = typ
  typeOf (ZZModMacro      typ _ _   :@ _) = typ
  typeOf (ZZModIfThenElse typ _ _ _ :@ _) = typ
  typeOf (ZZModCast       typ _     :@ _) = typ
  typeOf (ZZModNeg        typ _     :@ _) = typ
  typeOf (ZZModInv        typ _     :@ _) = typ
  typeOf (ZZModAdd        typ _ _   :@ _) = typ
  typeOf (ZZModSub        typ _ _   :@ _) = typ
  typeOf (ZZModMult       typ _ _   :@ _) = typ
  typeOf (ZZModPow        typ _ _   :@ _) = typ
  typeOf (ZZModSum        typ _     :@ _) = typ
  typeOf (ZZModProd       typ _     :@ _) = typ
