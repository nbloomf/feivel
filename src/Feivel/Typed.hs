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
  typeOf :: t -> EvalM Type



unifyTypesOf :: (Typed a, Typed b) => Locus -> a -> b -> EvalM Type
unifyTypesOf loc a b = do
  ta <- typeOf a
  tb <- typeOf b
  case unify ta tb of
    Left err -> reportErr loc err
    Right t -> return t

expectType :: (Typed a) => Locus -> Type -> a -> EvalM Type
expectType loc t x = do
  u <- typeOf x
  if t == u
    then return u
    else reportErr loc $ TypeMismatch t u

expectMatrix :: (Typed a) => Locus -> a -> EvalM Type
expectMatrix loc x = do
  u <- typeOf x
  case u of
    MatOf t -> return t
    _ -> reportErr loc $ MatrixExpected u

sameType :: (Typed a, Typed b) => Locus -> a -> b -> EvalM Type
sameType loc a b = do
  ta <- typeOf a
  tb <- typeOf b
  if ta == tb
    then return ta
    else reportErr loc $ TypeMismatch ta tb


{--------------}
{- :Instances -}
{--------------}

instance Typed IntExpr  where typeOf _ = return ZZ
instance Typed StrExpr  where typeOf _ = return SS
instance Typed BoolExpr where typeOf _ = return BB
instance Typed RatExpr  where typeOf _ = return QQ
instance Typed Doc      where typeOf _ = return DD

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
  typeOf (ListConst           typ _     :@ _) = return (ListOf typ)
  typeOf (ListVar             typ _     :@ _) = return (ListOf typ)
  typeOf (ListIfThenElse      typ _ _ _ :@ _) = return (ListOf typ)
  typeOf (ListRand            typ _     :@ _) = return (ListOf typ)
  typeOf (ListAtPos           typ _ _   :@ _) = return (ListOf typ)
  typeOf (ListAtIdx           typ _ _ _ :@ _) = return (ListOf typ)
  typeOf (ListMacro           typ _ _   :@ _) = return (ListOf typ)
  typeOf (ListCat             typ _ _   :@ _) = return (ListOf typ)
  typeOf (ListToss            typ _ _   :@ _) = return (ListOf typ)
  typeOf (ListRev             typ _     :@ _) = return (ListOf typ)
  typeOf (ListSort            typ _     :@ _) = return (ListOf typ)
  typeOf (ListUniq            typ _     :@ _) = return (ListOf typ)
  typeOf (ListShuffle         typ _     :@ _) = return (ListOf typ)
  typeOf (ListFilter          typ _ _ _ :@ _) = return (ListOf typ)
  typeOf (ListMatRow          typ _ _   :@ _) = return (ListOf typ)
  typeOf (ListMatCol          typ _ _   :@ _) = return (ListOf typ)
  typeOf (ListChoose          typ _ _   :@ _) = return (ListOf typ)
  typeOf (ListShuffles        typ _     :@ _) = return (ListOf typ)
  typeOf (ListChoices         typ _ _   :@ _) = return (ListOf typ)
  typeOf (ListRange           typ _ _   :@ _) = return (ListOf typ)
  typeOf (ListPermsOf         typ _     :@ _) = return (ListOf typ)
  typeOf (ListBuilder         typ _ _   :@ _) = return (ListOf typ)
  typeOf (ListPivotColIndices typ _     :@ _) = return (ListOf typ)



{------------}
{- :MacExpr -}
{------------}

instance Typed MacExpr where
  typeOf (MacConst typ _ _ _ :@ _) = return $ MacTo typ

  typeOf (MacVar key :@ loc) = do
    expr <- lookupKey loc key
    t <- typeOf expr
    case t of
      MacTo x -> return $ MacTo x
      u -> reportErr loc $ MacroExpected u

  typeOf (MacMacro _ expr :@ _) = do
    t <- typeOf expr
    return $ MacTo t

  typeOf (MacAtPos ms _ :@ loc) = do
    t <- typeOf ms
    case t of
      ListOf (MacTo u) -> return $ MacTo u
      _ -> reportErr loc $ MacroListExpected t

  typeOf (MacAtIdx ms _ _ :@ loc) = do
    t <- typeOf ms
    case t of
      MatOf (MacTo u) -> return $ MacTo u
      _ -> reportErr loc $ MacroMatrixExpected t

  typeOf (MacRand ms :@ loc) = do
    t <- typeOf ms
    case t of
      ListOf (MacTo u) -> return $ MacTo u
      _ -> reportErr loc $ MacroListExpected t

  typeOf (MacIfThenElse _ t f :@ loc) = do
    a <- typeOf t
    b <- typeOf f
    case unify a b of
      Right u -> return u
      Left err -> reportErr loc err



{------------}
{- :MatExpr -}
{------------}

instance Typed MatExpr where
  typeOf (MatVar key :@ loc) = do
    expr <- lookupKey loc key
    t <- typeOf expr
    case t of
      MatOf x -> return $ MatOf x
      u -> reportErr loc $ MatrixExpected u

  typeOf (MatMacro _ x :@ _) = typeOf x

  typeOf (MatAtPos m _ :@ loc) = do
    t <- typeOf m
    case t of
      MatOf (MatOf u) -> return $ MatOf u
      _ -> reportErr loc $ MatrixListExpected t

  typeOf (MatAtIdx m _ _ :@ loc) = do
    t <- typeOf m
    case t of
      ListOf (MatOf u) -> return $ MatOf u
      _ -> reportErr loc $ MatrixMatrixExpected t

  typeOf (MatIfThenElse _ t f :@ loc) = do
    a <- typeOf t
    b <- typeOf f
    case unify a b of
      Right u -> return u
      Left err -> reportErr loc err

  typeOf (MatConst t _ :@ _) = return $ MatOf t

  typeOf (MatId t _ :@ _) = return $ MatOf t
  typeOf (MatSwapE t _ _ _ :@ _) = return $ MatOf t
  typeOf (MatScaleE t _ _ _ :@ _) = return $ MatOf t
  typeOf (MatAddE t _ _ _ _ :@ _) = return $ MatOf t

  typeOf (MatHCat a b :@ loc) = sameType loc a b
  typeOf (MatVCat a b :@ loc) = sameType loc a b
  typeOf (MatAdd  a b :@ loc) = sameType loc a b
  typeOf (MatMul  a b :@ loc) = sameType loc a b
  typeOf (MatPow m _ :@ _) = typeOf m
  typeOf (MatTrans m :@ _) = typeOf m
  typeOf (MatNeg m :@ _) = typeOf m

  typeOf (MatSwapRows m _ _ :@ _) = typeOf m
  typeOf (MatSwapCols m _ _ :@ _) = typeOf m
  typeOf (MatScaleRow m _ _ :@ _) = typeOf m
  typeOf (MatScaleCol m _ _ :@ _) = typeOf m
  typeOf (MatAddRow m _ _ _ :@ _) = typeOf m
  typeOf (MatAddCol m _ _ _ :@ _) = typeOf m
  typeOf (MatDelRow m _ :@ _) = typeOf m
  typeOf (MatDelCol m _ :@ _) = typeOf m

  typeOf (MatShuffleRows m :@ _) = typeOf m
  typeOf (MatShuffleCols m :@ _) = typeOf m

  typeOf (MatGJForm   m :@ _) = typeOf m
  typeOf (MatGJFactor m :@ _) = typeOf m

  typeOf (MatGetRow _ m :@ _) = typeOf m
  typeOf (MatGetCol _ m :@ _) = typeOf m

  typeOf (MatRowFromList t _ :@ _) = return (MatOf t)
  typeOf (MatColFromList t _ :@ _) = return (MatOf t)

  typeOf (MatRand ms :@ loc) = do
    t <- typeOf ms
    case t of
      ListOf (MatOf u) -> return $ MatOf u
      _ -> reportErr loc $ MatrixListExpected t

  typeOf (MatBuilder typ _ _ _ _ _ :@ _) = return (MatOf typ)



{-------------}
{- :PolyExpr -}
{-------------}

instance Typed PolyExpr where
  typeOf (PolyVar        typ _     :@ _) = return (PolyOver typ)
  typeOf (PolyMacro      typ _ _   :@ _) = return (PolyOver typ)
  typeOf (PolyConst      typ _     :@ _) = return (PolyOver typ)
  typeOf (PolyAdd        typ _ _   :@ _) = return (PolyOver typ)
  typeOf (PolySub        typ _ _   :@ _) = return (PolyOver typ)
  typeOf (PolyMul        typ _ _   :@ _) = return (PolyOver typ)
  typeOf (PolyNeg        typ _     :@ _) = return (PolyOver typ)
  typeOf (PolyPow        typ _ _   :@ _) = return (PolyOver typ)
  typeOf (PolyAtPos      typ _ _   :@ _) = return (PolyOver typ)
  typeOf (PolyAtIdx      typ _ _ _ :@ _) = return (PolyOver typ)
  typeOf (PolyIfThenElse typ _ _ _ :@ _) = return (PolyOver typ)
  typeOf (PolyRand       typ _     :@ _) = return (PolyOver typ)
  typeOf (PolyFromRoots  typ _ _   :@ _) = return (PolyOver typ)
  typeOf (PolyEvalPoly   typ _ _   :@ _) = return (PolyOver typ)



{-------------}
{- :PermExpr -}
{-------------}

instance Typed PermExpr where
  typeOf (PermVar        typ _     :@ _) = return (PermOf typ)
  typeOf (PermMacro      typ _ _   :@ _) = return (PermOf typ)
  typeOf (PermConst      typ _     :@ _) = return (PermOf typ)
  typeOf (PermAtPos      typ _ _   :@ _) = return (PermOf typ)
  typeOf (PermAtIdx      typ _ _ _ :@ _) = return (PermOf typ)
  typeOf (PermIfThenElse typ _ _ _ :@ _) = return (PermOf typ)
  typeOf (PermRand       typ _     :@ _) = return (PermOf typ)
  typeOf (PermCompose    typ _ _   :@ _) = return (PermOf typ)
  typeOf (PermInvert     typ _     :@ _) = return (PermOf typ)



{--------------}
{- :ZZModExpr -}
{--------------}

instance Typed ZZModExpr where
  typeOf (ZZModConst (ZZModulo _ n) :@ _) = return (ZZMod n)

  typeOf (ZZModVar key :@ loc) = do
    expr <- lookupKey loc key
    t <- typeOf expr
    case t of
      ZZMod n -> return $ ZZMod n
      u -> reportErr loc $ ModularIntegerExpected u

  typeOf (ZZModAtPos m _ :@ loc) = do
    t <- typeOf m
    case t of
      MatOf (ZZMod n) -> return $ ZZMod n
      _ -> reportErr loc $ ModularIntegerListExpected t

  typeOf (ZZModAtIdx m _ _ :@ loc) = do
    t <- typeOf m
    case t of
      ListOf (ZZMod n) -> return $ ZZMod n
      _ -> reportErr loc $ ModularIntegerMatrixExpected t

  typeOf (ZZModMacro _ x :@ _) = typeOf x

  typeOf (ZZModIfThenElse _ t f :@ loc) = do
    a <- typeOf t
    b <- typeOf f
    case unify a b of
      Right u -> return u
      Left err -> reportErr loc err

  typeOf (ZZModCast n _ :@ _) = return (ZZMod n)

  typeOf (ZZModNeg a :@ _) = typeOf a
  typeOf (ZZModInv a :@ _) = typeOf a

  typeOf (ZZModAdd  a b :@ loc) = sameType loc a b
  typeOf (ZZModSub  a b :@ loc) = sameType loc a b
  typeOf (ZZModMult a b :@ loc) = sameType loc a b
  typeOf (ZZModPow  a _ :@ _)   = typeOf a

  typeOf (ZZModSum xs :@ loc) = do
    t <- typeOf xs
    case t of
      ListOf u -> return u
      u -> reportErr loc $ ListExpected u

  typeOf (ZZModProd xs :@ loc) = do
    t <- typeOf xs
    case t of
      ListOf u -> return u
      u -> reportErr loc $ ListExpected u
