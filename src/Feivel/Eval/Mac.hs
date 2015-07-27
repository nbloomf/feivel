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

module Feivel.Eval.Mac () where

import Feivel.Eval.Util


instance (Glyph Expr, Eval Expr) => Glyph MacExpr where
  toGlyph (MacExpr (MacConst _ st ex (amb,_) :@ loc)) = do
    old <- getState
    ctx <- tryEvalM loc $ toStateT st
    f   <- evalWith ex (mergeStores [ctx, old, amb])
    eval f >>= toGlyph
  toGlyph _ = error "toGlyph: MacExpr"


instance (Eval Expr) => Eval MacExpr where
  eval (MacExpr (MacConst typ vals expr (amb,p) :@ loc)) = do
    if p == True
      then return $ MacExpr $ MacConst typ vals expr (amb,True) :@ loc
      else do
        st <- getState
        return $ MacExpr $ MacConst typ vals expr (st,True) :@ loc

  {- :Common -}
  eval (MacExpr (MacVar _ key :@ loc))        = eKey key loc
  eval (MacExpr (MacIfThenElse _ b t f :@ _)) = eIfThenElse b t f
  eval (MacExpr (MacAtIdx _ m h k :@ loc))    = eAtIdx m h k loc
  eval (MacExpr (MacMacro _ vals mac :@ loc)) = eMacro vals mac loc

  eval (MacExpr (MacAtPos _ a t :@ loc)) = lift2 loc a t (foo)
    where foo = listAtPos :: [MacExpr] -> Integer -> Either ListErr MacExpr

  eval (MacExpr (MacRand _ ls :@ _)) = do
    xs <- eval ls >>= getVal
    randomElementEvalM xs
