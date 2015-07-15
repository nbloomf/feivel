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

module Feivel.Locus (
  Locus(NullLocus), locus, shortReport, longReport,
  AtLocus((:@)), HasLocus, locusOf
) where

import Text.ParserCombinators.Parsec (SourcePos, sourceName, sourceColumn, sourceLine)

{----------}
{- :Locus -}
{----------}

data Locus = Locus
 { path      :: String
 , startLine :: Int
 , startCol  :: Int
 , endLine   :: Int
 , endCol    :: Int
 } | NullLocus
 deriving (Eq)

instance Show Locus where
  show NullLocus = "Nowhere"
  show loc = show (startLine loc) ++ ":" ++ show (startCol loc)


locus :: SourcePos -> SourcePos -> Locus
locus pos1 pos2 = Locus
 { path      = sourceName   pos1
 , startLine = sourceLine   pos1
 , startCol  = sourceColumn pos1
 , endLine   = sourceLine   pos2
 , endCol    = sourceColumn pos2
 }


shortReport :: Locus -> String
shortReport NullLocus = "Nowhere!"
shortReport loc = path loc ++ ":" ++ show (startLine loc)


longReport :: Locus -> String
longReport NullLocus = "Nowhere!"
longReport loc = "in " ++ path loc ++
  " from line " ++ show (startLine loc) ++ " column " ++ show (startCol loc) ++
  " to line " ++ show (endLine loc) ++ " column " ++ show (endCol loc)



class HasLocus t where
  locusOf :: t -> Locus

data AtLocus a = a :@ Locus
  deriving (Show)

instance HasLocus (AtLocus t) where
  locusOf (_ :@ loc) = loc

instance (Eq a) => Eq (AtLocus a) where
  (x :@ _) == (y :@ _) = x == y