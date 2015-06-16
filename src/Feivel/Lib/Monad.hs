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

module Feivel.Lib.Monad where

opM1 :: (Monad m) => (a -> m b) -> m a -> m b
opM1 f x = do
  a <- x
  f a

opM2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
opM2 f x y = do
  a <- x
  b <- y
  f a b

sndSeq :: (Monad m) => (a, m b) -> m (a, b)
sndSeq (a,x) = do
  b <- x
  return (a,b)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False
