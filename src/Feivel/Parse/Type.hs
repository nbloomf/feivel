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

module Feivel.Parse.Type where

import Feivel.Grammar
import Feivel.Parse.ParseM

import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)

pType :: ParseM Type
pType = choice
  [ try (string "doc")  >> return DD
  , try (string "int")  >> return ZZ
  , try (string "str")  >> return SS
  , try (string "rat")  >> return QQ
  , try (string "bool") >> return BB
  , pTypeL
  , pTypeMac
  , pTypeMat
  , pTypePoly
  , pTypePerm
  , pTypeTuple
  , pTypeZZMod
  ] <?> "type signature"

pTypeL :: ParseM Type
pTypeL = do
  _ <- try $ char '{'
  t <- pType
  _ <- char '}'
  return $ ListOf t

pTypeMac :: ParseM Type
pTypeMac = do
  _ <- try $ char '>'
  t <- pType
  return $ MacTo t

pTypePoly :: ParseM Type
pTypePoly = do
  _ <- try $ char '^'
  t <- pType
  return $ PolyOver t

pTypePerm :: ParseM Type
pTypePerm = do
  _ <- try $ char '$'
  t <- pType
  return $ PermOf t

pTypeMat :: ParseM Type
pTypeMat = do
  _ <- try $ char '['
  t <- pType
  _ <- char ']'
  return $ MatOf t

pTypeTuple :: ParseM Type
pTypeTuple = do
  _  <- try $ char '('
  ts <- sepBy1 pType (char ',')
  _  <- char ')'
  return $ TupleOf ts

pTypeZZMod :: ParseM Type
pTypeZZMod = do
  _ <- try $ string "mod"
  n <- pNatural
  return $ ZZMod n
