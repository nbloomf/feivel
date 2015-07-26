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

module Feivel.Lib.Struct.Permutation where

import Feivel.Lib.AlgErr
import Feivel.Lib.Canon
import Feivel.Lib.Algebra.Group

import Control.Monad (foldM)
import Data.List (minimumBy, inits, tails, group, sortBy, sort, union, intersperse, permutations)


{------------}
{- :Helpers -}
{------------}

-- Returns true iff input has no duplicates
uniq :: (Eq a) => [a] -> Bool
uniq [] = True
uniq (a:as) = if a`elem`as then False else uniq as

dedupe :: (Eq a) => [a] -> [a]
dedupe [] = []
dedupe (a:as) = a : (filter (/= a) as)

disjoint :: (Eq a) => [a] -> [a] -> Bool
disjoint [] _ = True
disjoint (x:xs) ys = (not $ elem x ys) && (disjoint xs ys)

removeFirstFrom :: (Eq a) => a -> [a] -> [a]
removeFirstFrom _ [] = []
removeFirstFrom a (b:bs) = if b==a then bs else b:(removeFirstFrom a bs)

removeFirstsFrom :: (Eq a) => [a] -> [a] -> [a]
removeFirstsFrom as bs = foldr removeFirstFrom bs as

contains :: (Eq a) => [a] -> [a] -> Bool
contains as bs = null $ removeFirstsFrom as bs

isPermutationOf :: (Eq a) => [a] -> [a] -> Bool
isPermutationOf as bs = (as `contains` bs) && (bs `contains` as)



{---------}
{- :Perm -}
{---------}

data Perm a = Perm {unPerm :: [(a,a)]} deriving (Eq, Show)

isValid :: (Eq a) => Perm a -> Bool
isValid (Perm []) = True
isValid (Perm ps) = uniq as && (as `isPermutationOf` bs)
  where
    as = map fst ps
    bs = map snd ps

instance (Eq a) => Canon (Perm a) where
  canon (Perm ps) = Perm $ filter (\(a,b) -> a /= b) ps

movedBy :: (Eq a) => Perm a -> [a]
movedBy = map fst . unPerm . canon



{--------------}
{- :Construct -}
{--------------}

idPerm :: (Eq a) => Perm a
idPerm = Perm []

idOn :: a -> Perm a
idOn _ = Perm []

fromCycle :: (Eq a) => [a] -> Either AlgErr (Perm a)
fromCycle [] = Right (Perm [])
fromCycle as
  | uniq as   = Right $ Perm $ zip as (tail as ++ [head as])
  | otherwise = Left NotACycle

unionDisjoint :: (Eq a) => Perm a -> Perm a -> Either AlgErr (Perm a)
unionDisjoint p q
  | disjoint (movedBy p) (movedBy q) = Right $ Perm (unPerm p ++ unPerm q)
  | otherwise = Left NotDisjoint

fromCycles :: (Eq a) => [[a]] -> Either AlgErr (Perm a)
fromCycles xss = do
  os <- sequence $ map fromCycle xss
  foldM unionDisjoint idPerm os

fromPairs :: (Eq a) => [(a,a)] -> Either AlgErr (Perm a)
fromPairs ps = case isValid q of
  True  -> return q
  False -> Left NotAPermutation
  where q = Perm ps



{----------}
{- :Query -}
{----------}

image :: (Eq a) => Perm a -> a -> a
image (Perm ps) a = case lookup a ps of
  Just b  -> b
  Nothing -> a

orbit :: (Eq a, Ord a) => Perm a -> a -> [a]
orbit p a = rotate $ a : cycle' (image p a)
  where
    cycle' x = if x==a then [] else x : cycle' (image p x)
    rotate xs = minimumBy compareHead $ zipWith (++) (tails xs) (inits xs)
    compareHead (x:_) (y:_) = compare x y
    compareHead _     _     = error "compareHead in orbit"

cycles :: (Eq a, Ord a) => Perm a -> [[a]]
cycles p = munch $ sortBy compareHead $ map (orbit p) (movedBy p)
  where
    compareHead (x:_) (y:_) = compare x y
    compareHead _     _     = error "compareHead in cycles"
    munch = (map head) . group

shape :: (Eq a, Ord a) => Perm a -> [Integer]
shape p = reverse $ sort $ map len $ cycles p
  where len = sum . map (const 1)

order :: (Eq a, Ord a) => Perm a -> Integer
order p = foldr lcm 1 (shape p)



{- :View -}

showPerm :: Perm String -> String
showPerm p = bar $ concatMap foo $ cycles p
  where
    foo as = "(" ++ (concat $ intersperse " " as) ++ ")"

    bar [] = "1"
    bar s  = s



{---------------}
{- :Operations -}
{---------------}

inverse :: (Eq a) => Perm a -> Perm a
inverse = Perm . map (\(a,b) -> (b,a)) . unPerm

compose :: (Eq a) => Perm a -> Perm a -> Perm a
compose p q = canon $ Perm
  [(i, image p (image q i)) | i <- union (movedBy p) (movedBy q)]



{- :Monadish -}

mapPerm :: (a -> b) -> Perm a -> Perm b
mapPerm f (Perm ps) = Perm [(f a, f b) | (a, b) <- ps]

seqPerm :: (Monad m, Functor m) => Perm (m a) -> m (Perm a)
seqPerm (Perm ps) = fmap Perm $ sequence $ map foo ps
  where
    foo (x,y) = do
      a <- x
      b <- y
      return (a,b)

permsOf :: (Eq a) => [a] -> Either AlgErr [Perm a]
permsOf xs = sequence $ map (fromPairs . zip xs) (permutations xs)

instance (Eq a) => Groupoid (Perm a) where
  gOp p q = return $ compose p q
  gInv  = inverse
  gId   = idPerm
  gIsId = (idPerm ==)

  gLIdOf _ = idPerm
  gRIdOf _ = idPerm
