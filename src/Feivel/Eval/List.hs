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

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Feivel.Eval.List () where

import Feivel.Eval.Util

import Data.List (intersperse, (\\), sort, nub, permutations)
import Control.Monad (filterM)


instance (Glyph Expr) => Glyph ListExpr where
  toGlyph (ListExpr (ListConst xs :# _ :@ _)) = do
    ys <- sequence $ map toGlyph xs
    return $ "{" ++ concat (intersperse ";" ys) ++ "}"
  toGlyph x = error $ "toGlyph: ListExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr, Eval MatExpr) => Eval ListExpr where
  eval (ListExpr (zappa :# typ :@ loc)) = case zappa of
    ListConst xs -> do
      ys <- sequence $ map eval xs
      putTypeVal typ loc ys >>= getVal

    {- :Common -}
    ListVar key -> eKey key loc
    ListAtIdx m h k -> eAtIdx m h k loc
    ListMacro vals mac -> eMacro vals mac loc
    ListIfThenElse b t f -> eIfThenElse b t f

    ListAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [ListExpr] -> Integer -> Either ListErr ListExpr

    ListRange a b -> do
      x <- eval a >>= getVal :: EvalM Integer
      y <- eval b >>= getVal :: EvalM Integer
      putTypeVal ZZ loc [put loc k | k <- [x..y]] >>= getVal

    ListCat a b -> do
      xs <- eval a >>= getVal :: EvalM [Expr]
      ys <- eval b >>= getVal :: EvalM [Expr]
      putTypeVal typ loc (xs ++ ys) >>= getVal

    ListToss a b -> do
      xs <- eval a >>= getVal :: EvalM [Expr]
      ys <- eval b >>= getVal :: EvalM [Expr]
      putTypeVal typ loc (xs \\ ys) >>= getVal

    ListRev a -> do
      xs <- eval a >>= getVal :: EvalM [Expr]
      putTypeVal typ loc (reverse xs) >>= getVal

    ListSort a -> case typ of
      SS -> do
        xs <- eval a >>= getVal :: EvalM [Text]
        putTypeVal SS loc (map (put loc) (sort xs)) >>= getVal
      ZZ -> do
        xs <- eval a >>= getVal :: EvalM [Integer]
        putTypeVal ZZ loc (map (put loc) (sort xs)) >>= getVal
      QQ -> do
        xs <- eval a >>= getVal :: EvalM [Rat]
        putTypeVal QQ loc (map (put loc) (sort xs)) >>= getVal
      BB -> do
        xs <- eval a >>= getVal :: EvalM [Bool]
        putTypeVal BB loc (map (put loc) (sort xs)) >>= getVal
      _ -> reportErr loc $ SortableListExpected typ

    ListRand ls -> do
      xs <- eval ls >>= getVal :: EvalM [Expr]
      randomElementEvalM xs >>= getVal

    ListUniq a -> do
      xs <- eval a >>= getVal :: EvalM [Expr]
      putTypeVal typ loc (nub xs) >>= getVal

    ListShuffle ls -> do
      xs <- eval ls >>= getVal :: EvalM [Expr]
      ys <- shuffleEvalM xs
      putTypeVal typ loc ys >>= getVal

    ListShuffles ls -> case typ of
      ListOf u -> do
        xs <- eval ls >>= getVal :: EvalM [Expr]
        let us = [putType (ListOf u) loc ys | ys <- permutations xs]
        putTypeVal (ListOf u) loc us >>= getVal
      _ -> reportErr loc $ ListExpected typ

    ListChoose n ls -> do
      k  <- eval n >>= getVal :: EvalM Integer
      xs <- eval ls >>= getVal :: EvalM [Expr]
      ys <- sampleEvalM (fromIntegral k) xs
      putTypeVal typ loc ys >>= getVal

    ListChoices n ls -> case typ of
      ListOf u -> do
        k  <- eval n  >>= getVal :: EvalM Integer
        xs <- eval ls >>= getVal :: EvalM [Expr]
        let foos = [putType u loc x | x <- combinations (fromIntegral k) xs]
        putTypeVal (ListOf u) loc foos >>= getVal
      _ -> reportErr loc $ ListExpected typ

    ListBuilder e gs -> do
      st <- getState
      xs <- bar st gs
      ys <- sequence [evalWith e x >>= getVal | x <- xs]
      t <- case ys of
             [] -> return XX
             (z:_) -> return (typeOf z)
      eval $ ListExpr $ ListConst ys :# t :@ loc
        where
          bar :: Store Expr -> [ListGuard Expr ListExpr] -> EvalM [Store Expr]
          bar st []     = return [st]
          bar st (h:hs) = do
            xs <- foo st h
            fmap concat $ sequence $ [bar x hs | x <- xs]
        
          foo :: Store Expr -> (ListGuard Expr ListExpr) -> EvalM [Store Expr]
          foo st (Bind key ls) = do
            xs <- eval ls >>= getVal :: EvalM [Expr]
            sequence [addKeyToStore key x (locusOf x) st | x <- xs]
        
          foo st (Guard p) = do
            x <- evalWith p st >>= getVal :: EvalM Bool
            if x == True
              then return [st]
              else return []

    ListFilter k g xs -> do
      ys <- eval xs >>= getVal :: EvalM [Expr]
      let foo e = do
            defineKey k e loc
            x <- eval g >>= getVal :: EvalM Bool
            undefineKey k
            return x
      zs <- filterM foo ys
      putTypeVal typ loc zs >>= getVal

    ListMatRow k m -> do
      i  <- eval k >>= getVal :: EvalM Integer
      n  <- eval m >>= getVal :: EvalM (Matrix Expr)
      as <- tryEvalM loc $ mListRowOf i n
      putTypeVal typ loc as >>= getVal

    ListMatCol k m -> do
      i  <- eval k >>= getVal :: EvalM Integer
      n  <- eval m >>= getVal :: EvalM (Matrix Expr)
      as <- tryEvalM loc $ mListColOf i n
      putTypeVal typ loc as >>= getVal

    ListPermsOf xs -> do
      case typ of
        PermOf ZZ -> do
          as <- eval xs >>= getVal :: EvalM [Integer]
          qs <- tryEvalM loc $ permsOf as
          let us = map (put loc) qs :: [Expr]
          putTypeVal typ loc us >>= getVal
        PermOf SS -> do
          as <- eval xs >>= getVal :: EvalM [Text]
          qs <- tryEvalM loc $ permsOf as
          let us = map (put loc) qs :: [Expr]
          putTypeVal typ loc us >>= getVal
        PermOf QQ -> do
          as <- eval xs >>= getVal :: EvalM [Rat]
          qs <- tryEvalM loc $ permsOf as
          let us = map (put loc) qs :: [Expr]
          putTypeVal typ loc us >>= getVal
        u -> reportErr loc $ PermutationExpected u

    ListBezouts as -> do
      case typ of
        ZZ -> do
          xs <- eval as >>= getVal :: EvalM [Integer]
          ys <- tryEvalM loc $ rBezouts xs
          putTypeVal typ loc ys >>= getVal
        _ -> error "ListBezouts"

    ListPivotColIndices m -> do
      let pivotIndices x = do
            n  <- eval m >>= getVal
            suchThat $ n `hasSameTypeAs` x
            is <- tryEvalM loc $ mPivotCols n
            putTypeVal ZZ loc (map (put loc) is) >>= getVal
      case typeOf m of
        MatOf QQ -> pivotIndices (mCell zeroQQ)
        MatOf BB -> pivotIndices (mCell zeroBB)
        MatOf u  -> reportErr loc $ FieldMatrixExpected u
        t        -> reportErr loc $ MatrixExpected t
