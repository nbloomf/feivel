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

module Feivel.Get (Get, get, getVal, toStateT) where

import Feivel.Glyph
import Feivel.Type
import Feivel.Error
import Feivel.Typed
import Feivel.Locus
import Feivel.EvalM
import Feivel.Expr
import Feivel.Lib
import Feivel.Store
import Feivel.Key


toStateT :: Locus -> [(Type, Key, Expr)] -> EvalM (Store Expr)
toStateT loc vs = do
  ws <- sequence $ map (checkType loc) vs
  toState loc ws
    where
      checkType :: Locus -> (Type, Key, Expr) -> EvalM (Key, Expr)
      checkType loc' (t,k,v) = do
        tv <- typeOf v
        if tv == t
          then return (k,v)
          else reportErr loc' $ TypeMismatch t tv


class Get a where
  get :: Expr -> EvalM a

getVal :: (ToExpr a, Get b) => a -> EvalM b
getVal x = get (toExpr x)

instance Get Expr where
  get expr = return expr


{----------------}
{- :Get:IntExpr -}
{----------------}

instance Get IntExpr where
  get (IntE y) = return y
  get v = do
    t <- typeOf v
    reportErr (locusOf v) $ TypeMismatch ZZ t

instance Get Integer where
  get expr = do
    x <- get expr :: EvalM IntExpr
    case x of
      IntConst k :@ _ -> return k
      v -> reportErr (locusOf v) UnevaluatedExpression


{----------------}
{- :Get:StrExpr -}
{----------------}

instance Get StrExpr where
  get (StrE y) = return y
  get (DocE y) = do  -- this shouldn't be necessary. why is it?
    str <- toGlyph y
    return $ StrConst str :@ (locusOf y)
  get v = do
    t <- typeOf v
    reportErr (locusOf v) $ TypeMismatch SS t

instance Get String where
  get expr = do
    x <- get expr
    case x of
      StrConst s :@ _ -> return s
      v -> reportErr (locusOf v) UnevaluatedExpression


{-----------------}
{- :Get:BoolExpr -}
{-----------------}

instance Get BoolExpr where
  get (BoolE y) = return y
  get v = do
    t <- typeOf v
    reportErr (locusOf v) $ TypeMismatch BB t

instance Get Bool where
  get expr = do
    x <- get expr
    case x of
      BoolConst b :@ _ -> return b
      v -> reportErr (locusOf v) UnevaluatedExpression


{----------------}
{- :Get:RatExpr -}
{----------------}

instance Get RatExpr where
  get (RatE y) = return y
  get v = do
    t <- typeOf v
    reportErr (locusOf v) $ TypeMismatch QQ t

instance Get Rat where
  get expr = do
    x <- get expr
    case x of
      RatConst r :@ _ -> return r
      v -> reportErr (locusOf v) UnevaluatedExpression


{-----------------}
{- :Get:ListExpr -}
{-----------------}

instance Get ListExpr where
  get (ListE y) = return y
  get v = do
    t <- typeOf v
    reportErr (locusOf v) $ ListExpected t

instance (Get a) => Get [a] where
  get expr = do
    x <- getList expr
    sequence $ fmap get x
    where
      getList :: Expr -> EvalM [Expr]
      getList w = do
        case w of
          ListE (ListConst _ xs :@ _) -> return xs
          ListE v -> reportErr (locusOf v) UnevaluatedExpression
          v -> do
            t <- typeOf v
            reportErr (locusOf v) $ ListExpected t


{----------------}
{- :Get:MacExpr -}
{----------------}

instance Get MacExpr where
  get (MacE y) = return y
  get v = do
    t <- typeOf v
    reportErr (locusOf v) $ MacroExpected t

instance Get (Store Expr, Expr) where
  get expr = do
    case expr of
      MacE (MacConst _ vals y (amb,_) :@ loc) -> do
        st <- toStateT loc vals
        return (mergeState st amb, y)
      MacE v -> reportErr (locusOf v) UnevaluatedExpression
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ MacroExpected t


{----------------}
{- :Get:MatExpr -}
{----------------}

instance Get MatExpr where
  get (MatE m) = return m
  get v = do
    t <- typeOf v
    reportErr (locusOf v) $ MatrixExpected t

instance (Get a) => Get (Matrix a) where
  get expr = do
    x <- getMatrix expr
    mSeq $ fmap get x
    where
      getMatrix :: Expr -> EvalM (Matrix Expr)
      getMatrix w = do
        case w of
          MatE (MatConst _ m :@ _) -> return m
          MatE v -> reportErr (locusOf v) UnevaluatedExpression
          v -> do
            t <- typeOf v
            reportErr (locusOf v) $ MatrixExpected t


{------------}
{- :Get:Doc -}
{------------}

instance Get Doc where
  get (DocE y) = return y
  get v = do
    t <- typeOf v
    reportErr (locusOf v) $ TypeMismatch DD t


{-----------------}
{- :Get:PolyExpr -}
{-----------------}

instance Get PolyExpr where
  get (PolyE m) = return m
  get v = do
    t <- typeOf v
    reportErr (locusOf v) $ PolynomialExpected t

instance (Get a) => Get (Poly a) where
  get expr = do
    x <- getPoly expr
    polySeq $ fmap get x
    where
      getPoly :: Expr -> EvalM (Poly Expr)
      getPoly w = do
        case w of
          PolyE (PolyConst _ m :@ _) -> return m
          PolyE v -> reportErr (locusOf v) UnevaluatedExpression
          v -> do
            t <- typeOf v
            reportErr (locusOf v) $ PolynomialExpected t


{-----------------}
{- :Get:PermExpr -}
{-----------------}

instance Get PermExpr where
  get (PermE m) = return m
  get v = do
    t <- typeOf v
    reportErr (locusOf v) $ PermutationExpected t

instance Get (Perm Expr) where
  get expr = do
    case expr of
      PermE (PermConst _ m :@ _) -> return m
      PermE v -> reportErr (locusOf v) UnevaluatedExpression
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ PermutationExpected t

instance Get (Perm Integer) where
  get expr = do
    x <- get expr :: EvalM (Perm Expr)
    seqPerm $ mapPerm get x


{------------------}
{- :Get:ZZModExpr -}
{------------------}

instance Get ZZModExpr where
  get (ZZModE y) = return y
  get v = do
        t <- typeOf v
        reportErr (locusOf v) $ ModularIntegerExpected t

instance Get ZZModulo where
  get expr = do
    x <- get expr :: EvalM ZZModExpr
    case x of
      ZZModConst k :@ _ -> return k
      v -> reportErr (locusOf v) UnevaluatedExpression

