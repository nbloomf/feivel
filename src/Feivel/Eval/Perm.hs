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

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Feivel.Eval.Perm () where

import Feivel.Eval.Util


instance (Glyph Expr) => Glyph (PermExpr Expr) where
  toGlyph (PermConst _ px :@ _) = do
    qx <- seqPerm $ mapPerm toGlyph px
    return $ showPerm qx
  toGlyph x = error $ "toGlyph: PermExpr: " ++ show x


instance (Eval Expr) => Eval (PermExpr Expr) where
  eval (PermConst t p :@ loc) = do
    q <- seqPerm $ mapPerm eval p
    return $ PermConst t q :@ loc

  eval (PermAtPos _ a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [PermExpr Expr] -> Integer -> Either ListErr (PermExpr Expr)

  {- Common -}
  eval (PermVar _ key :@ loc)        = eKey key loc
  eval (PermAtIdx _ m h k :@ loc)    = eAtIdx m h k loc
  eval (PermMacro _ vals mac :@ loc) = eMacro vals mac loc
  eval (PermIfThenElse _ b t f :@ _) = eIfThenElse b t f

  eval (PermRand _ ls :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    eval r >>= getVal

  eval (PermCompose t p q :@ loc) = do
    case t of
      PermOf ZZ -> do
        a <- eval p >>= getVal :: EvalM (Perm Integer)
        b <- eval q >>= getVal :: EvalM (Perm Integer)
        let c = compose a b
        let s = mapPerm (put loc) c
        return (PermConst ZZ s :@ loc)
      _ -> reportErr loc $ PolynomialExpected t

  eval (PermInvert _ p :@ loc) = do
    let t = typeOf p
    case t of
      PermOf ZZ -> do
        a <- eval p >>= getVal :: EvalM (Perm Integer)
        let c = inverse a
        let s = mapPerm (put loc) c
        return (PermConst ZZ s :@ loc)
      _ -> reportErr loc $ PolynomialExpected t