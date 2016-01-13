{---------------------------------------------------------------------}
{- Copyright 2016 Nathan Bloomfield                                  -}
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

module Feivel.Parse.ParserDict where

import Feivel.Grammar
import Feivel.Parse.ParseM

data ParserDict = ParserDict
  { parseINT   :: ParseM IntExpr
  , parseRAT   :: ParseM RatExpr
  , parseBOOL  :: ParseM BoolExpr
  , parseSTR   :: ParseM StrExpr
  , parseZZMOD :: Integer -> ParseM ZZModExpr
  , parseLIST  :: Type -> ParseM ListExpr
  , parseMAT   :: Type -> ParseM MatExpr
  , parseMAC   :: Type -> ParseM MacExpr
  , parsePOLY  :: Type -> ParseM PolyExpr
  , parsePERM  :: Type -> ParseM PermExpr
  , parseTUPLE :: [Type] -> ParseM TupleExpr
  }
