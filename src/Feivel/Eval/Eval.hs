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

module Feivel.Eval.Eval where

import Feivel.EvalM
import Feivel.Expr
import Feivel.Get
import Feivel.Put
import Feivel.Locus
import Feivel.Error
import Feivel.Lib

class Eval t where
  eval :: t -> EvalM t

instance Eval Integer where eval = return
instance Eval String  where eval = return
instance Eval Text    where eval = return
instance Eval Rat     where eval = return
instance Eval Bool    where eval = return


lift1
  :: ( Eval x, ToExpr x, Get a
     , Put b, Get y
     , PromoteError err
  ) => Locus -> x -> (a -> Either err b) -> EvalM y
lift1 loc x f = do
  a <- eval x >>= getVal
  b <- tryEvalM loc $ f a
  getVal (put loc b)


lift2
  :: ( Eval x, ToExpr x, Get a
     , Eval y, ToExpr y, Get b
     , Put c, Get z
     , PromoteError err
  ) => Locus -> x -> y -> (a -> b -> Either err c) -> EvalM z
lift2 loc x y f = do
  a <- eval x >>= getVal
  b <- eval y >>= getVal
  c <- tryEvalM loc $ f a b
  getVal (put loc c)


lift3
  :: ( Eval x, ToExpr x, Get a
     , Eval y, ToExpr y, Get b
     , Eval z, ToExpr z, Get c
     , Put d, Get w
     , PromoteError err
  ) => Locus -> x -> y -> z -> (a -> b -> c -> Either err d) -> EvalM w
lift3 loc x y z f = do
  a <- eval x >>= getVal
  b <- eval y >>= getVal
  c <- eval z >>= getVal
  d <- tryEvalM loc $ f a b c
  getVal (put loc d)


lift4
  :: ( Eval x, ToExpr x, Get a
     , Eval y, ToExpr y, Get b
     , Eval z, ToExpr z, Get c
     , Eval w, ToExpr w, Get d
     , Put e, Get u
     , PromoteError err
  ) => Locus -> x -> y -> z -> w -> (a -> b -> c -> d -> Either err e) -> EvalM u
lift4 loc x y z w f = do
  a <- eval x >>= getVal
  b <- eval y >>= getVal
  c <- eval z >>= getVal
  d <- eval w >>= getVal
  e <- tryEvalM loc $ f a b c d
  getVal (put loc e)
