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
{-# LANGUAGE FlexibleInstances    #-}

module Feivel.Grammar.Get (
  Get, get, toStateT, GetErr()
) where

import Feivel.Grammar.Expr
import Feivel.Grammar.Util
import Feivel.Lib (mSeq, polySeq, mapPerm, seqPerm)


class Get a where
  get :: Expr -> Either GetErr a

data GetErr
  = GetTypeMismatch { typeExpected :: Type, typeReceived :: Type }
  | GetUnevaluatedExpression
  deriving (Eq, Show)


{----------------}
{- :Expressions -}
{----------------}

instance Get Expr where
  get expr = return expr

instance Get IntExpr where
  get (IntE y) = return y
  get v = Left $ GetTypeMismatch
    { typeExpected = ZZ, typeReceived = typeOf v }

instance Get BoolExpr where
  get (BoolE y) = return y
  get v = Left $ GetTypeMismatch
    { typeExpected = BB, typeReceived = typeOf v }

instance Get StrExpr where
  get (StrE y) = return y
  get (DocE (Doc (Empty :@ loc))) = return $ StrExpr $ StrConst (Text "") :@ loc
  get (DocE (Doc (DocText str :@ loc))) = return $ StrExpr $ StrConst str :@ loc
  get (DocE (Doc (Escaped c :@ loc))) = return $ StrExpr $ StrConst (Text [c]) :@ loc
  get v = Left $ GetTypeMismatch
    { typeExpected = SS, typeReceived = typeOf v }

instance Get RatExpr where
  get (RatE y) = return y
  get v = Left $ GetTypeMismatch
    { typeExpected = QQ, typeReceived = typeOf v }

instance Get ZZModExpr where
  get (ZZModE y) = return y
  get v = Left $ GetTypeMismatch
    { typeExpected = ZZMod 0, typeReceived = typeOf v }

instance Get ListExpr where
  get (ListE y) = return y
  get v = Left $ GetTypeMismatch
    { typeExpected = ListOf XX, typeReceived = typeOf v }

instance Get MatExpr where
  get (MatE m) = return m
  get v = Left $ GetTypeMismatch
    { typeExpected = MatOf XX, typeReceived = typeOf v }

instance Get PolyExpr where
  get (PolyE m) = return m
  get v = Left $ GetTypeMismatch
    { typeExpected = PolyOver XX, typeReceived = typeOf v }

instance Get PermExpr where
  get (PermE m) = return m
  get v = Left $ GetTypeMismatch
    { typeExpected = PermOf XX, typeReceived = typeOf v }

instance Get MacExpr where
  get (MacE y) = return y
  get v = Left $ GetTypeMismatch
    { typeExpected = MacTo XX, typeReceived = typeOf v }

instance Get (Doc Expr) where
  get (DocE y) = return y
  get v = Left $ GetTypeMismatch
    { typeExpected = DD, typeReceived = typeOf v }



{--------------}
{- :Constants -}
{--------------}

instance Get Integer where
  get expr = do
    x <- get expr :: Either GetErr IntExpr
    case x of
      IntExpr (IntConst k :@ _) -> return k
      _ -> Left GetUnevaluatedExpression

instance Get Bool where
  get expr = do
    x <- get expr :: Either GetErr BoolExpr
    case x of
      BoolExpr (BoolConst b :@ _) -> return b
      v -> Left GetUnevaluatedExpression

instance Get Text where
  get expr = do
    x <- get expr :: Either GetErr StrExpr
    case x of
      StrExpr (StrConst s :@ _) -> return s
      v -> Left GetUnevaluatedExpression

instance Get Rat where
  get expr = do
    x <- get expr :: Either GetErr RatExpr
    case x of
      RatExpr (RatConst r :@ _) -> return r
      v -> Left GetUnevaluatedExpression

instance Get ZZModulo where
  get expr = do
    x <- get expr :: Either GetErr ZZModExpr
    case x of
      ZZModExpr (ZZModConst _ k :@ _) -> return k
      _ -> Left GetUnevaluatedExpression

instance (Get a) => Get [a] where
  get expr = do
    x <- getList expr
    sequence $ fmap get x
    where
      getList :: Expr -> Either GetErr [Expr]
      getList w = do
        case w of
          ListE (ListExpr (ListConst _ xs :@ _)) -> return xs
          ListE v -> Left GetUnevaluatedExpression
          v -> Left $ GetTypeMismatch
                 { typeExpected = ListOf XX, typeReceived = typeOf v }

instance (Get a) => Get (Matrix a) where
  get expr = do
    x <- getMatrix expr
    mSeq $ fmap get x
    where
      getMatrix :: Expr -> Either GetErr (Matrix Expr)
      getMatrix w = do
        case w of
          MatE (MatExpr (MatConst _ m :@ _)) -> return m
          MatE v -> Left GetUnevaluatedExpression
          v -> Left $ GetTypeMismatch
                 { typeExpected = MatOf XX, typeReceived = typeOf v }

instance (Get a) => Get (Poly a) where
  get expr = do
    x <- getPoly expr
    polySeq $ fmap get x
    where
      getPoly :: Expr -> Either GetErr (Poly Expr)
      getPoly w = do
        case w of
          PolyE (PolyExpr (PolyConst _ m :@ _)) -> return m
          PolyE v -> Left GetUnevaluatedExpression
          v -> Left $ GetTypeMismatch
                 { typeExpected = PolyOver XX, typeReceived = typeOf v }

instance (Get a) => Get (Perm a) where
  get expr = do
    x <- getPerm expr
    seqPerm $ mapPerm get x
    where
      getPerm :: Expr -> Either GetErr (Perm Expr)
      getPerm w = do
        case w of
          PermE (PermExpr (PermConst _ m :@ _)) -> return m
          PermE v -> Left GetUnevaluatedExpression
          v -> Left $ GetTypeMismatch
                 { typeExpected = PermOf XX, typeReceived = typeOf v }

instance Get (Store Expr, Expr) where
  get expr = do
    case expr of
      MacE (MacExpr (MacConst _ vals y (amb,_) :@ _)) -> do
        st <- toStateT vals
        return (mergeState st amb, y)
      MacE v -> Left GetUnevaluatedExpression
      v -> Left $ GetTypeMismatch
             { typeExpected = MacTo XX, typeReceived = typeOf v }



{--------------}
{- :Utilities -}
{--------------}

toStateT :: [(Type, Key, Expr)] -> Either GetErr (Store Expr)
toStateT vs = do
  ws <- sequence $ map checkType vs
  case fromKeyValList ws of
    Left _   -> error "toStateT"
    Right st -> return st
    where
      checkType :: (Type, Key, Expr) -> Either GetErr (Key, Expr)
      checkType (t,k,v) = do
        if t == typeOf v
          then return (k,v)
          else Left $ GetTypeMismatch
                 { typeExpected = t, typeReceived = typeOf v }
