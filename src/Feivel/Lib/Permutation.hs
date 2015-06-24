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

module Feivel.Lib.Permutation where

import Feivel.Lib.AlgErr
import Feivel.Lib.Canon

import Control.Monad (foldM)
import Data.List (minimumBy, inits, tails, group, sortBy, sort, union)
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

image :: (Eq a, Ord a) => Perm a -> a -> a
image p a = case Map.lookup a p of
  Just b  -> b
  Nothing -> a

orbit :: (Eq a, Ord a) => Perm a -> a -> [a]
orbit p a = rotate $ a : cycle' (image p a)
  where
    cycle' x = if x==a then [] else x : cycle' (image p x)
    rotate xs = minimumBy compareHead $ zipWith (++) (tails xs) (inits xs)
    compareHead (x:_) (y:_) = compare x y

cycles :: (Eq a, Ord a) => Perm a -> [[a]]
cycles p = munch $ sortBy compareHead $ map (orbit p) (movedBy p)
  where
    compareHead (x:_) (y:_) = compare x y
    munch = (map head) . group

shape :: (Eq a, Ord a) => Perm a -> [Integer]
shape p = reverse $ sort $ map len $ cycles p
  where len = sum . map (const 1)

order :: (Eq a, Ord a) => Perm a -> Integer
order p = foldr lcm 1 (shape p)

instance (Eq a, Ord a) => Canon (Perm a) where
  canon = Map.fromList . filter foo . Map.toList
    where
      foo (a,b) = a /= b

inverse :: (Eq a, Ord a) => Perm a -> Perm a
inverse = Map.fromList . map (\(a,b) -> (b,a)) . Map.toList

compose :: (Eq a, Ord a) => Perm a -> Perm a -> Perm a
compose p q = canon $ Map.fromList
  [(i, image p (image q i)) | i <- union (movedBy p) (movedBy q)]