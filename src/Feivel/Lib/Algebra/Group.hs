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

module Feivel.Lib.Algebra.Group (
  Groupoid,
    gOp,  gInv, gId, gIsId, gLIdOf, gRIdOf, gProd, gPow, gEQ,
    gOpT, gInvT,

  CGroupoid
) where

import Feivel.Lib.AlgErr

import Control.Monad (foldM)

class Groupoid t where
  gOp  :: t -> t -> Either AlgErr t
  gInv :: t -> t

  gId   :: t
  gIsId :: t -> Bool

  gLIdOf :: t -> t
  gRIdOf :: t -> t

gEQ :: (Groupoid t) => t -> t -> Either AlgErr Bool
gEQ x y = do
  z <- gOp x (gInv y)
  return (gIsId z) 

gProd :: (Groupoid t) => [t] -> Either AlgErr t
gProd = foldM gOp gId

gPow :: (Groupoid t) => t -> Integer -> Either AlgErr t
gPow x n
  | n >= 0    = gProd [x | _ <- [1..n]]
  | otherwise = gPow x (-n)

{- -}

gOpT :: (Groupoid t) => t -> t -> t -> Either AlgErr t
gOpT _ = gOp

gInvT :: (Groupoid t) => t -> t -> Either AlgErr t
gInvT _ = return . gInv

class CGroupoid t
