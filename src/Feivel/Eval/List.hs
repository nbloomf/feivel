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

{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}

module Feivel.Eval.List () where

import Feivel.Eval.Util

import Data.List (intersperse, (\\), sort, nub, permutations)
import Control.Monad (filterM)


instance (Glyph Expr) => Glyph (ListExpr Expr) where
  toGlyph (ListConst _ xs :@ _) = do
    ys <- sequence $ map toGlyph xs
    return $ "{" ++ concat (intersperse ";" ys) ++ "}"
  toGlyph x = error $ "toGlyph: ListExpr: " ++ show x


instance (Eval Expr) => Eval (ListExpr Expr) where
  eval (ListConst t xs :@ loc) = do
    ys <- sequence $ map eval xs
    return $ ListConst t ys :@ loc

  {- :Common -}
  eval (ListVar _ key :@ loc)        = eKey key loc
  eval (ListAtIdx _ m h k :@ loc)    = eAtIdx m h k loc
  eval (ListMacro _ vals mac :@ loc) = eMacro vals mac loc
  eval (ListIfThenElse _ b t f :@ _) = eIfThenElse b t f

  eval (ListAtPos _ a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [ListExpr Expr] -> Integer -> Either ListErr (ListExpr Expr)

  eval (ListRange _ a b :@ loc) = do
    x <- eval a >>= getVal :: EvalM Integer
    y <- eval b >>= getVal :: EvalM Integer
    return (ListConst ZZ [IntE $ IntExpr $ IntConst k :@ loc | k <- [x..y]] :@ loc)

  eval (ListCat u a b :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Expr]
    ys <- eval b >>= getVal :: EvalM [Expr]
    return (ListConst u (xs ++ ys) :@ loc)

  eval (ListToss u a b :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Expr]
    ys <- eval b >>= getVal :: EvalM [Expr]
    return $ ListConst u (xs \\ ys) :@ loc

  eval (ListRev u a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Expr]
    return $ ListConst u (reverse xs) :@ loc

  eval (ListSort SS a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Text]
    return $ ListConst SS (map (\k -> StrE $ StrConst k :@ loc) (sort xs)) :@ loc
  eval (ListSort ZZ a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Integer]
    return $ ListConst ZZ (map (\k -> IntE $ IntExpr $ IntConst k :@ loc) (sort xs)) :@ loc
  eval (ListSort QQ a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Rat]
    return $ ListConst QQ (map (\k -> RatE $ RatConst k :@ loc) (sort xs)) :@ loc
  eval (ListSort BB a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Bool]
    return $ ListConst BB (map (\k -> BoolE $ BoolConst k :@ loc) (sort xs)) :@ loc
  eval (ListSort typ _ :@ loc) = reportErr loc $ SortableListExpected typ

  eval (ListRand _ ls :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    randomElementEvalM xs >>= getVal :: EvalM (ListExpr Expr)

  eval (ListUniq u a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Expr]
    return $ ListConst u (nub xs) :@ loc

  eval (ListShuffle u ls :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    ys <- shuffleEvalM xs
    return $ ListConst u ys :@ loc

  eval (ListShuffles (ListOf u) ls :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    let us = [ListE $ ListConst (ListOf u) ys :@ loc | ys <- permutations xs]
    return (ListConst (ListOf u) us :@ loc)
  eval (ListShuffles u _ :@ loc) = reportErr loc $ ListExpected u

  eval (ListChoose u n ls :@ loc) = do
    k  <- eval n >>= getVal :: EvalM Integer
    xs <- eval ls >>= getVal :: EvalM [Expr]
    ys <- sampleEvalM (fromIntegral k) xs
    return (ListConst u ys :@ loc)

  eval (ListChoices (ListOf u) n ls :@ loc) = do
    k  <- eval n  >>= getVal :: EvalM Integer
    xs <- eval ls >>= getVal :: EvalM [Expr]
    let foos = [ListE $ ListConst u x :@ loc | x <- combinations (fromIntegral k) xs]
    return $ ListConst (ListOf u) foos :@ loc
  eval (ListChoices u _ _ :@ loc) = reportErr loc $ ListExpected u

  eval (ListBuilder _ e gs :@ loc) = do
    st <- getState
    xs <- bar st gs
    ys <- sequence [evalWith e x >>= getVal | x <- xs]
    t <- case ys of
           [] -> return XX
           (z:_) -> return (typeOf z)
    eval $ ListConst t ys :@ loc
      where
        bar :: Store Expr -> [ListGuard Expr] -> EvalM [Store Expr]
        bar st []     = return [st]
        bar st (h:hs) = do
          xs <- foo st h
          fmap concat $ sequence $ [bar x hs | x <- xs]
        
        foo :: Store Expr -> (ListGuard Expr) -> EvalM [Store Expr]
        foo st (Bind key ls) = do
          xs <- eval ls >>= getVal :: EvalM [Expr]
          sequence [addKeyToStore key x (locusOf x) st | x <- xs]
        
        foo st (Guard p) = do
          x <- evalWith p st >>= getVal :: EvalM Bool
          if x == True
            then return [st]
            else return []

  eval (ListFilter u k g xs :@ loc) = do
    ys <- eval xs >>= getVal :: EvalM [Expr]
    let foo e = do
          defineKey k e loc
          x <- eval g >>= getVal :: EvalM Bool
          undefineKey k
          return x
    zs <- filterM foo ys
    return (ListConst u zs :@ loc)

  eval (ListMatRow u k m :@ loc) = do
    i  <- eval k >>= getVal :: EvalM Integer
    n  <- eval m >>= getVal :: EvalM (Matrix Expr)
    as <- tryEvalM loc $ mListRowOf i n
    return (ListConst u as :@ loc)

  eval (ListMatCol u k m :@ loc) = do
    i  <- eval k >>= getVal :: EvalM Integer
    n  <- eval m >>= getVal :: EvalM (Matrix Expr)
    as <- tryEvalM loc $ mListColOf i n
    return (ListConst u as :@ loc)

  eval (ListPermsOf typ xs :@ loc) = do
    case typ of
      PermOf ZZ -> do
        as <- eval xs >>= getVal :: EvalM [Integer]
        qs <- tryEvalM loc $ permsOf as
        let us = map (put loc) qs :: [Expr]
        return (ListConst (PermOf ZZ) us :@ loc)
      PermOf SS -> do
        as <- eval xs >>= getVal :: EvalM [Text]
        qs <- tryEvalM loc $ permsOf as
        let us = map (put loc) qs :: [Expr]
        return (ListConst (PermOf SS) us :@ loc)
      PermOf QQ -> do
        as <- eval xs >>= getVal :: EvalM [Rat]
        qs <- tryEvalM loc $ permsOf as
        let us = map (put loc) qs :: [Expr]
        return (ListConst (PermOf SS) us :@ loc)
      u -> reportErr loc $ PermutationExpected u

  eval (ListPivotColIndices _ m :@ loc) = do
    let pivotIndices x = do
          n  <- eval m >>= getVal
          suchThat $ n `hasSameTypeAs` x
          is <- tryEvalM loc $ mPivotCols n
          return (ListConst ZZ (map (put loc) is) :@ loc)
    case typeOf m of
      MatOf QQ -> pivotIndices (mCell zeroQQ)
      MatOf BB -> pivotIndices (mCell zeroBB)
      MatOf u  -> reportErr loc $ FieldMatrixExpected u
      t        -> reportErr loc $ MatrixExpected t