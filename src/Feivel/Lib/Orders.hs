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

module Feivel.Lib.Orders where

lexicographic2 :: (Ord a, Ord b) => [(a,b)] -> [(a,b)] -> Ordering
lexicographic2 [] [] = EQ
lexicographic2 [] _  = LT
lexicographic2 _  [] = GT
lexicographic2 ((a1,b1):ps1) ((a2,b2):ps2)
  | a1 <  a2 = LT
  | a1 >  a2 = GT
  | a1 == a2 && b1 < b2 = LT
  | a1 == a2 && b1 > b2 = GT
  | otherwise = lexicographic2 ps1 ps2
