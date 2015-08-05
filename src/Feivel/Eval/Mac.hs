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

module Feivel.Eval.Mac () where

import Feivel.Eval.Util


instance (Glyph Expr, Eval Expr) => Glyph MacExpr where
  toGlyph (MacExpr (MacConst st ex (amb,_) :# _ :@ loc)) = do
    old <- getState
    ctx <- tryEvalM loc $ toStateT st
    f   <- evalWith ex (mergeStores [ctx, old, amb])
    eval f >>= toGlyph
  toGlyph _ = error "toGlyph: MacExpr"


instance (Eval Expr, Eval BoolExpr, Eval IntExpr, Eval ListExpr, Eval MatExpr) => Eval MacExpr where
  eval (MacExpr (zappa :# typ :@ loc)) = case zappa of
    MacConst vals expr (amb,p) -> do
      if p == True
        then return $ MacExpr $ MacConst vals expr (amb,True) :# typ :@ loc
        else do
          st <- getState
          return $ MacExpr $ MacConst vals expr (st,True) :# typ :@ loc

    {- :Common -}
    MacVar key -> eKey key loc
    MacIfThenElse b t f -> eIfThenElse b t f
    MacAtIdx m h k -> eAtIdx m h k loc
    MacMacro vals mac -> eMacro vals mac loc

    MacAtPos a t -> lift2 loc a t (foo)
      where foo = listAtPos :: [MacExpr] -> Integer -> Either ListErr MacExpr

    MacRand ls -> do
      xs <- eval ls >>= getVal
      randomElementEvalM xs
