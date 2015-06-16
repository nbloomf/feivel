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

module Feivel.Parse.Key (
  pKey, pTypedKey, pUntypedKey, pToken
) where

import Feivel.Key
import Feivel.Type
import Feivel.Parse.ParseM
import Feivel.Parse.Type
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pToken :: ParseM String
pToken = many1 $ oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.")


pKey :: ParseM (Key, Type)
pKey = pUntypedKey

pUntypedKey :: ParseM (Key, Type)
pUntypedKey = do
  x <- try (char '@') >> pToken
  return (Key x, XX)
  <?> "key (@foo)"

pTypedKey :: ParseM (Key, Type)
pTypedKey = do
  x <- try (char '@') >> pToken
  t <- char '#' >> pType
  return (Key x, t)
  <?> "typed key (@foo::type)"