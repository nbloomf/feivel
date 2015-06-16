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

module Feivel.Lib.Pair (
  swap, mapFst, mapSnd, seqFst, seqSnd
) where

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (a,c) = (f a, c)

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (c,a) = (c, f a)

seqFst :: (Monad m) => (m a, b) -> m (a,b)
seqFst (x,b) = do
  a <- x
  return (a,b)

seqSnd :: (Monad m) => (a, m b) -> m (a,b)
seqSnd (a,y) = do
  b <- y
  return (a,b)
