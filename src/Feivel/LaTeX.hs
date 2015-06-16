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

{-# OPTIONS_GHC -XTypeSynonymInstances #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module Feivel.LaTeX where

import Feivel.Lib

import Data.List (intersperse)

class LaTeX t where
  latex :: t -> String

instance LaTeX Integer where
  latex = show

instance LaTeX String where
  latex = id

instance LaTeX Rat where
  latex p = foo $ canon p
    where
      foo (a:/:b)
       | a == 0    = "0"
       | b == 1    = show a
       | otherwise = "\\frac{" ++ show a ++ "}{" ++ show b ++ "}"

instance LaTeX (Poly Integer) where
  latex = showByOUP "" revlexM latex

instance LaTeX (Poly Rat) where
  latex = showByOUP "" revlexM latex

instance LaTeX (Matrix Integer) where
  latex m = "\\begin{bmatrix} " ++ foo ++ " \\end{bmatrix}"
    where
      foo = concat $ intersperse " \\\\ " $ map (concat . intersperse " & " . map latex) $ mToRowList m

instance LaTeX (Matrix Rat) where
  latex m = "\\begin{bmatrix} " ++ foo ++ " \\end{bmatrix}"
    where
      foo = concat $ intersperse " \\\\ " $ map (concat . intersperse " & " . map latex) $ mToRowList m