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

module Feivel.Lib.Permutation where

import Feivel.Lib.AlgErr

import Control.Monad (foldM)
import qualified Data.Map as Map

type Perm a = Map.Map a a

toPairs :: (Eq a, Ord a) => Perm a -> [(a,a)]
toPairs = Map.toList

movedBy :: (Eq a, Ord a) => Perm a -> [a]
movedBy = Map.keys

idPerm :: (Eq a, Ord a) => Perm a
idPerm = Map.fromList []

fromCycle :: (Eq a, Ord a) => [a] -> Either AlgErr (Perm a)
fromCycle [] = Right $ Map.fromList []
fromCycle as
  | uniq as   = Right $ Map.fromList $ zip as (tail as ++ [head as])
  | otherwise = Left NotACycle

disjoint :: (Eq a) => [a] -> [a] -> Bool
disjoint [] _ = True
disjoint (x:xs) ys = (not $ elem x ys) && (disjoint xs ys)

unionDisjoint :: (Eq a, Ord a) => Perm a -> Perm a -> Either AlgErr (Perm a)
unionDisjoint p q
  | disjoint (movedBy p) (movedBy q) = Right $ Map.union p q
  | otherwise = Left NotDisjoint

fromCycles :: (Eq a, Ord a) => [[a]] -> Either AlgErr (Perm a)
fromCycles xss = do
  os <- sequence $ map fromCycle xss
  foldM unionDisjoint idPerm os

uniq :: (Eq a) => [a] -> Bool
uniq [] = True
uniq (a:as) = if a`elem`as then False else uniq as