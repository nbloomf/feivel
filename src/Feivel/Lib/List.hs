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

module Feivel.Lib.List (
  ListErr(),

  listAtPos, combinations
) where

{-----------}
{- ListErr -}
{-----------}

data ListErr
 = EmptyList
 | InvalidListIndex { index :: Integer }
 deriving (Eq, Show)



listAtPos :: [a] -> Integer -> Either ListErr a
listAtPos [] _ = Left EmptyList
listAtPos xs k
 | k <= 0    = Left $ InvalidListIndex { index = k }
 | otherwise = let (a, m) = last $ zip xs [1..k] in
                if m==k
                 then Right a
                 else Left $ InvalidListIndex { index = k }

combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations 1 xs     = map (:[]) xs
combinations _ []     = []
combinations n (x:xs)
  | n < 0     = []
  | otherwise = map (x:) (combinations (n-1) xs) ++ combinations n xs
