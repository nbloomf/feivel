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
  module Feivel.Parse.Type,

  keyword, whitespace, spaced, pKey, pToken,

  pTuple2, pTuple4,

  pFun1,  pFun2, pFun3, pFun4,
  pFun1T, pFun2T,

  opParser1, opParser2, pVarExpr, pTypeKeyExpr, pTerm, pIfThenElseExprT, pMacroExprT, pConst, pTerm', opParser2', opParser1', pIfThenElseExprT'
) where


import Feivel.Store
import Feivel.Grammar

import Feivel.Parse.Type

import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)
import Feivel.Parse.ParseM

{- :Primitives -}

pToken :: ParseM String
pToken = many1 $ oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.")


pKey :: ParseM Key
pKey = do
  x <- try (char '@') >> pToken
  return (Key x)
  <?> "key (@foo)"

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


{--------------}
{- :Functions -}
{--------------}

pFun1 :: String -> ParseM a -> (a -> b) -> ParseM b
pFun1 fun pA f = do
  try $ keyword fun
  keyword "("
  a <- pA
  keyword ")"
  return (f a)

pFun2 :: String -> ParseM a -> ParseM b -> (a -> b -> c) -> ParseM c
pFun2 fun pA pB f = do
  try $ keyword fun
  (a,b) <- pTuple2 pA pB
  return (f a b)

pFun3 :: String -> ParseM a -> ParseM b -> ParseM c -> (a -> b -> c -> d) -> ParseM d
pFun3 fun pA pB pC f = do
  try $ keyword fun
  (a,b,c) <- pTuple3 pA pB pC
  return (f a b c)

pFun4 :: String -> ParseM a -> ParseM b -> ParseM c -> ParseM d -> (a -> b -> c -> d -> e) -> ParseM e
pFun4 fun pA pB pC pD f = do
  try $ keyword fun
  (a,b,c,d) <- pTuple4 pA pB pC pD
  return (f a b c d)

{- :TypedFunctions -}

pFun1T :: String -> (Type -> ParseM a) -> (a -> b) -> ParseM b
pFun1T fun pA f = do
  try $ keyword fun
  keyword "("
  t <- pType
  keyword ";"
  a <- pA t
  keyword ")"
  return (f a)

pFun2T :: String -> (Type -> ParseM a) -> (Type -> ParseM b) -> (a -> b -> c) -> ParseM c
pFun2T fun pA pB f = do
  try $ keyword fun
  keyword "("
  t <- pType
  keyword ";"
  a <- pA t
  keyword ";"
  b <- pB t
  keyword ")"
  return (f a b)

-- Unary Operators (for expression parser)
opParser1 :: (AtLocus a -> a) -> String -> ParseM ((AtLocus a) -> (AtLocus a))
opParser1 f fun = do
  start <- getPosition
  try $ keyword fun
  end <- getPosition
  return $ \x -> (f x :@ (locus start end))

-- Unary Operators (for expression parser)
opParser1' ::
  (b -> a) -> (AtLocus a -> b) -> String -> ParseM (b -> b)
opParser1' f cons fun = do
  start <- getPosition
  try $ keyword fun
  end <- getPosition
  return $ \x -> (cons $ f x :@ (locus start end))

-- Binary Operators (for expression parser)
opParser2 :: (AtLocus a -> AtLocus a -> a) -> String -> ParseM ((AtLocus a) -> (AtLocus a) -> (AtLocus a))
opParser2 f fun = do
  start <- getPosition
  try $ keyword fun
  end <- getPosition
  return $ \x y -> (f x y :@ (locus start end))

-- Binary Operators (for expression parser)
opParser2' ::
  (b -> b -> a)-> (AtLocus a -> b) -> String -> ParseM (b -> b -> b)
opParser2' f cons fun = do
  start <- getPosition
  try $ keyword fun
  end <- getPosition
  return $ \x y -> (cons $ f x y :@ (locus start end))

pVarExpr :: (Key -> a) -> Type -> ParseM a
pVarExpr h t = do
  k <- pKey
  return (h k)

pTypeKeyExpr :: (Type -> ParseM Expr) -> ParseM (Type, Key, Expr)
pTypeKeyExpr pE = do
  t <- pType
  whitespace
  k <- pKey
  keyword ":="
  v <- pE t
  return (t, k, v)

-- Terms in Expression Grammars
pTerm :: ParseM a -> ParseM (AtLocus a) -> String -> [ParseM a] -> ParseM (AtLocus a)
pTerm cst expr err atoms = 
  choice [try $ pAtLocus atom | atom <- cst:atoms] <|> (pParens expr) <?> err

-- Terms in Expression Grammars
pTerm' :: ParseM a -> (AtLocus a -> b) -> ParseM b -> String -> [ParseM a] -> ParseM b
pTerm' cst cons expr err atoms = 
  choice [try $ fmap cons $ pAtLocus atom | atom <- cst:atoms] <|> (pParens expr) <?> err

pIfThenElseExprT :: (Type -> ParseM Expr) -> ParseM a -> (Expr -> a -> a -> b) -> t -> ParseM b
pIfThenElseExprT pE p h t = do
  keyword "if"
  b  <- pE BB
  keyword "then"
  tr <- p
  keyword "else"
  fa <- p
  return (h b tr fa)

pIfThenElseExprT' :: ParseM BoolExpr -> ParseM a -> (BoolExpr -> a -> a -> b) -> t -> ParseM b
pIfThenElseExprT' pBOOL p h t = do
  keyword "if"
  b  <- pBOOL
  keyword "then"
  tr <- p
  keyword "else"
  fa <- p
  return (h b tr fa)

pMacroExprT :: (Type -> ParseM Expr) -> ([(Type, Key, Expr)] -> Expr -> a) -> ParseM a
pMacroExprT pE f = do
  try $ keyword "Eval"
  keyword "("
  t <- pType
  keyword ";"
  e <- pE (MacTo t)
  vals <- option [] $ many1 (keyword ";" >> (pTypeKeyExpr pE))
  keyword ")"
  return (f vals e)

pConst :: ParseM a -> (a -> b) -> ParseM b
pConst p h = do
  x <- p
  return (h x)
