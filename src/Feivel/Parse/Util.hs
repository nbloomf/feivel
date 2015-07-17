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

module Feivel.Parse.Util (
  keyword, whitespace, spaced,

  pTuple2, pTuple4,

  pFun1, pFun2, pFun3, pFun4, pFun1T, pFun2T, pFun3T, pFun4T
) where

import Feivel.Type
import Feivel.Parse.Type
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)
import Feivel.Parse.ParseM

{- :Primitives -}

whitespace :: ParseM ()
whitespace = spaces

keyword :: String -> ParseM ()
keyword s = whitespace >> string s >> whitespace

spaced :: ParseM a -> ParseM a
spaced p = do
  whitespace
  x <- p
  whitespace
  return x


{- Tuples -}

pTuple1 :: ParseM a -> ParseM a
pTuple1 pA = do
  try $ keyword "("
  a <- pA
  keyword ")"
  return a

pTuple2 :: ParseM a -> ParseM b -> ParseM (a,b)
pTuple2 pA pB = do
  try $ keyword "("
  a <- pA
  keyword ";"
  b <- pB
  keyword ")"
  return (a,b)

pTuple3 :: ParseM a -> ParseM b -> ParseM c -> ParseM (a,b,c)
pTuple3 pA pB pC = do
  try $ keyword "("
  a <- pA
  keyword ";"
  b <- pB
  keyword ";"
  c <- pC
  keyword ")"
  return (a,b,c)

pTuple4 :: ParseM a -> ParseM b -> ParseM c -> ParseM d -> ParseM (a,b,c,d)
pTuple4 pA pB pC pD = do
  try $ keyword "("
  a <- pA
  keyword ";"
  b <- pB
  keyword ";"
  c <- pC
  keyword ";"
  d <- pD
  keyword ")"
  return (a,b,c,d)

pTuple2C :: (Type -> ParseM (a, Type)) -> ParseM (a, Type)
pTuple2C pA = do
  try $ keyword "("
  t <- pType
  keyword ";"
  (a,u) <- pA t
  keyword ")"
  return (a,u)

pTuple2T :: ParseM a -> ParseM b -> ParseM (a,b)
pTuple2T pA pB = do
  try $ keyword "("
  a <- pA
  keyword ";"
  b <- pB
  keyword ")"
  return (a,b)

pTuple3T :: ParseM a -> ParseM b -> ParseM c -> ParseM (a,b,c)
pTuple3T pA pB pC = do
  try $ keyword "("
  a <- pA
  keyword ";"
  b <- pB
  keyword ";"
  c <- pC
  keyword ")"
  return (a,b,c)

pTuple4T :: ParseM a -> ParseM b -> ParseM c -> ParseM d -> ParseM (a,b,c,d)
pTuple4T pA pB pC pD = do
  try $ keyword "("
  a <- pA
  keyword ";"
  b <- pB
  keyword ";"
  c <- pC
  keyword ";"
  d <- pD
  keyword ")"
  return (a,b,c,d)


{- Functions -}

pFun1 :: String -> ParseM a -> (a -> b) -> t -> ParseM b
pFun1 fun pA f t = do
  try $ keyword fun
  keyword "("
  a <- pA
  keyword ")"
  return (f a)

pFun2 :: String -> ParseM a -> ParseM b -> (a -> b -> c) -> t -> ParseM c
pFun2 fun pA pB f t = do
  try $ keyword fun
  (a,b) <- pTuple2 pA pB
  return (f a b)

pFun3 :: String -> ParseM a -> ParseM b -> ParseM c -> (a -> b -> c -> d) -> t -> ParseM d
pFun3 fun pA pB pC f t = do
  try $ keyword fun
  (a,b,c) <- pTuple3 pA pB pC
  return (f a b c)

pFun4 :: String -> ParseM a -> ParseM b -> ParseM c -> ParseM d -> (a -> b -> c -> d -> e) -> t -> ParseM e
pFun4 fun pA pB pC pD f t = do
  try $ keyword fun
  (a,b,c,d) <- pTuple4 pA pB pC pD
  return (f a b c d)

pFun1T :: String -> ParseM a -> (a -> b) -> ParseM b
pFun1T fun pA f = do
  try $ keyword fun
  keyword "("
  a <- pA
  keyword ")"
  return (f a)

pFun2T :: String -> ParseM a -> ParseM b -> (a -> b -> c) -> ParseM c
pFun2T fun pA pB f = do
  try $ keyword fun
  (a,b) <- pTuple2T pA pB
  return (f a b)

pFun3T :: String -> ParseM a -> ParseM b -> ParseM c -> (a -> b -> c -> d) -> ParseM d
pFun3T fun pA pB pC f = do
  try $ keyword fun
  (a,b,c) <- pTuple3T pA pB pC
  return (f a b c)

pFun4T :: String -> ParseM a -> ParseM b -> ParseM c -> ParseM d -> (a -> b -> c -> d -> e) -> ParseM e
pFun4T fun pA pB pC pD f = do
  try $ keyword fun
  (a,b,c,d) <- pTuple4T pA pB pC pD
  return (f a b c d)
