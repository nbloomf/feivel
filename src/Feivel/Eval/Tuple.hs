{---------------------------------------------------------------------}
{- Copyright 2016 Nathan Bloomfield                                  -}
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

module Feivel.Eval.Tuple () where

import Feivel.Eval.Util
import Carl.List
import Carl.String

import Data.List (intersperse, (\\), sort, nub, permutations)
import Control.Monad (filterM)


instance (Glyph Expr) => Glyph TupleExpr where
  toGlyph (TupleExpr (TupleConst (Tuple xs) :# _ :@ _)) = do
    ys <- sequence $ map toGlyph xs
    return $ "(" ++ concat (intersperse "," ys) ++ ")"
  toGlyph x = error $ "toGlyph: TupleExpr: " ++ show x


instance (Eval Expr, Eval BoolExpr, Eval IntExpr, Eval MatExpr) => Eval TupleExpr where
  eval (TupleExpr (zappa :# typ :@ loc)) = case zappa of
    TupleConst (Tuple xs) -> do
      ys <- sequence $ map eval xs
      putTypeVal typ loc (Tuple ys) >>= getVal

    {- :Common -}
    TupleVar key -> eKey key loc
    TupleAtIdx m h k -> eAtIdx m h k loc
    TupleMacro vals mac -> eMacro vals mac loc
    TupleIfThenElse b t f -> eIfThenElse b t f

    TupleAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [ListExpr] -> Integer -> Either ListErr ListExpr
