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

module Feivel.Parse.Mac (
  pMacConst, pTypedMacExpr
) where

import Feivel.Store (locus, emptyStore)
import Feivel.Grammar (Type(..), Expr(..), MacExpr, MacExprLeaf(..))
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)

{------------}
{- :MacExpr -}
{------------}

pMacConst
  :: Type -> (Type -> ParseM Expr) -> ((Type -> ParseM Expr) -> ParseM Expr)
      -> ParseM (MacExpr Expr)
pMacConst typ pE pBD = pAtLocus $ pMacConst' typ pE pBD

pMacConst'
  :: Type -> (Type -> ParseM Expr) -> ((Type -> ParseM Expr) -> ParseM Expr)
     -> ParseM (MacExprLeaf Expr)
pMacConst' typ pE pBD = do
  start <- getPosition
  try $ keyword "Macro"
  keyword "("
  t <- pType
  keyword ";"
  body <- if t == DD then (pBD pE) else pE t
  vals <- option [] $ many (keyword ";" >> (pTypeKeyExpr pE))
  keyword ")"
  end <- getPosition
  if typ /= t
    then error "pMacConst'" -- reportParseErr (locus start end) $ TypeUnificationError typ t
    else return (MacConst t vals body (emptyStore, False))


pMacExpr :: (Type -> ParseM Expr) -> ((Type -> ParseM Expr) -> ParseM Expr) -> ParseM (MacExpr Expr)
pMacExpr pE pBD = pTypedMacExpr XX pE pBD


pTypedMacExpr :: Type -> (Type -> ParseM Expr) -> ((Type -> ParseM Expr) -> ParseM Expr) -> ParseM (MacExpr Expr)
pTypedMacExpr typ pE pBD = spaced $ buildExpressionParser macOpTable pMacTerm
  where
    pMacTerm = pTerm (pMacConst' typ pE pBD) (pMacExpr pE pBD) "macro expression"
      [ pVarExpr (MacVar typ) (MacTo typ)

      , pFun2 "AtPos" (pE $ ListOf (MacTo typ)) (pE ZZ) (MacAtPos typ)
      , pFun3 "AtIdx" (pE $ MatOf (MacTo typ)) (pE ZZ) (pE ZZ) (MacAtIdx typ)

      , pMacroExprT pE (MacMacro typ)

      , pFun1 "Rand" (pE $ ListOf (MacTo typ)) (MacRand typ)

      , pIfThenElseExprT pE (pTypedMacExpr typ pE pBD) (MacIfThenElse typ) (MacTo typ)
      ]

    macOpTable = []
