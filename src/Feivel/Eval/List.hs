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

module Feivel.Eval.List () where

import Feivel.Eval.Util

import Data.List (intersperse, (\\), sort, nub, permutations)
import Control.Monad (filterM)


instance (Glyph Expr) => Glyph ListExpr where
  toGlyph (ListExpr (ListConst _ xs :@ _)) = do
    ys <- sequence $ map toGlyph xs
    return $ "{" ++ concat (intersperse ";" ys) ++ "}"
  toGlyph x = error $ "toGlyph: ListExpr: " ++ show x


instance (Eval Expr, Eval IntExpr) => Eval ListExpr where
  eval (ListExpr (ListConst t xs :@ loc)) = do
    ys <- sequence $ map eval xs
    putTypeVal t loc ys >>= getVal

  {- :Common -}
  eval (ListExpr (ListVar _ key :@ loc))        = eKey key loc
  eval (ListExpr (ListAtIdx _ m h k :@ loc))    = eAtIdx m h k loc
  eval (ListExpr (ListMacro _ vals mac :@ loc)) = eMacro vals mac loc
  eval (ListExpr (ListIfThenElse _ b t f :@ _)) = eIfThenElse b t f

  eval (ListExpr (ListAtPos _ a t :@ loc)) = lift2 loc a t (foo)
    where foo = listAtPos :: [ListExpr] -> Integer -> Either ListErr ListExpr

  eval (ListExpr (ListRange _ a b :@ loc)) = do
    x <- eval a >>= getVal :: EvalM Integer
    y <- eval b >>= getVal :: EvalM Integer
    putTypeVal ZZ loc [put loc k | k <- [x..y]] >>= getVal

  eval (ListExpr (ListCat u a b :@ loc)) = do
    xs <- eval a >>= getVal :: EvalM [Expr]
    ys <- eval b >>= getVal :: EvalM [Expr]
    putTypeVal u loc (xs ++ ys) >>= getVal

  eval (ListExpr (ListToss u a b :@ loc)) = do
    xs <- eval a >>= getVal :: EvalM [Expr]
    ys <- eval b >>= getVal :: EvalM [Expr]
    putTypeVal u loc (xs \\ ys) >>= getVal

  eval (ListExpr (ListRev u a :@ loc)) = do
    xs <- eval a >>= getVal :: EvalM [Expr]
    putTypeVal u loc (reverse xs) >>= getVal

  eval (ListExpr (ListSort SS a :@ loc)) = do
    xs <- eval a >>= getVal :: EvalM [Text]
    putTypeVal SS loc (map (put loc) (sort xs)) >>= getVal
  eval (ListExpr (ListSort ZZ a :@ loc)) = do
    xs <- eval a >>= getVal :: EvalM [Integer]
    putTypeVal ZZ loc (map (put loc) (sort xs)) >>= getVal
  eval (ListExpr (ListSort QQ a :@ loc)) = do
    xs <- eval a >>= getVal :: EvalM [Rat]
    putTypeVal QQ loc (map (put loc) (sort xs)) >>= getVal
  eval (ListExpr (ListSort BB a :@ loc)) = do
    xs <- eval a >>= getVal :: EvalM [Bool]
    putTypeVal BB loc (map (put loc) (sort xs)) >>= getVal
  eval (ListExpr (ListSort typ _ :@ loc)) = reportErr loc $ SortableListExpected typ

  eval (ListExpr (ListRand _ ls :@ loc)) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    randomElementEvalM xs >>= getVal

  eval (ListExpr (ListUniq u a :@ loc)) = do
    xs <- eval a >>= getVal :: EvalM [Expr]
    putTypeVal u loc (nub xs) >>= getVal

  eval (ListExpr (ListShuffle u ls :@ loc)) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    ys <- shuffleEvalM xs
    putTypeVal u loc ys >>= getVal

  eval (ListExpr (ListShuffles (ListOf u) ls :@ loc)) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    let us = [putType (ListOf u) loc ys | ys <- permutations xs]
    putTypeVal (ListOf u) loc us >>= getVal
  eval (ListExpr (ListShuffles u _ :@ loc)) = reportErr loc $ ListExpected u

  eval (ListExpr (ListChoose u n ls :@ loc)) = do
    k  <- eval n >>= getVal :: EvalM Integer
    xs <- eval ls >>= getVal :: EvalM [Expr]
    ys <- sampleEvalM (fromIntegral k) xs
    putTypeVal u loc ys >>= getVal

  eval (ListExpr (ListChoices (ListOf u) n ls :@ loc)) = do
    k  <- eval n  >>= getVal :: EvalM Integer
    xs <- eval ls >>= getVal :: EvalM [Expr]
    let foos = [putType u loc x | x <- combinations (fromIntegral k) xs]
    putTypeVal (ListOf u) loc foos >>= getVal
  eval (ListExpr (ListChoices u _ _ :@ loc)) = reportErr loc $ ListExpected u

  eval (ListExpr (ListBuilder _ e gs :@ loc)) = do
    st <- getState
    xs <- bar st gs
    ys <- sequence [evalWith e x >>= getVal | x <- xs]
    t <- case ys of
           [] -> return XX
           (z:_) -> return (typeOf z)
    eval $ ListExpr $ ListConst t ys :@ loc
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

  eval (ListExpr (ListFilter u k g xs :@ loc)) = do
    ys <- eval xs >>= getVal :: EvalM [Expr]
    let foo e = do
          defineKey k e loc
          x <- eval g >>= getVal :: EvalM Bool
          undefineKey k
          return x
    zs <- filterM foo ys
    putTypeVal u loc zs >>= getVal

  eval (ListExpr (ListMatRow u k m :@ loc)) = do
    i  <- eval k >>= getVal :: EvalM Integer
    n  <- eval m >>= getVal :: EvalM (Matrix Expr)
    as <- tryEvalM loc $ mListRowOf i n
    putTypeVal u loc as >>= getVal

  eval (ListExpr (ListMatCol u k m :@ loc)) = do
    i  <- eval k >>= getVal :: EvalM Integer
    n  <- eval m >>= getVal :: EvalM (Matrix Expr)
    as <- tryEvalM loc $ mListColOf i n
    putTypeVal u loc as >>= getVal

  eval (ListExpr (ListPermsOf typ xs :@ loc)) = do
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

  eval (ListExpr (ListPivotColIndices _ m :@ loc)) = do
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
