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

import Feivel.Store (emptyStore)
import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Expr (buildExpressionParser)
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)

{------------}
{- :MacExpr -}
{------------}

pMacConst
  :: Type -> (Type -> ParseM Expr) -> ParseM Expr -> ParseM MacExpr
pMacConst typ pE pBD = fmap MacExpr $ pAtLocus $ pMacConst' typ pE pBD

pMacConst'
  :: Type -> (Type -> ParseM Expr) -> ParseM Expr -> ParseM (MacExprLeaf Expr BoolExpr IntExpr ListExpr MatExpr MacExpr)
pMacConst' typ pE pBD = do
  try $ keyword "Macro"
  keyword "("
  t <- pType
  keyword ";"
  body <- if t == DD then pBD else pE t
  vals <- option [] $ many (keyword ";" >> (pTypeKeyExpr pE))
  keyword ")"
  if typ /= t
    then error "pMacConst'" -- reportParseErr (locus start end) $ TypeUnificationError typ t
    else return (MacConst t vals body (emptyStore, False))


pTypedMacExpr :: Type -> (Type -> ParseM Expr) -> ParseM Expr -> ParseM BoolExpr -> ParseM IntExpr -> (Type -> ParseM ListExpr) -> (Type -> ParseM MatExpr) -> (Type -> ParseM MacExpr) -> ParseM MacExpr
pTypedMacExpr typ pE pBD pBOOL pINT pLIST pMAT pMAC = spaced $ buildExpressionParser macOpTable pMacTerm
  where
    pMacTerm = pTerm (pMacConst' typ pE pBD) MacExpr (pMAC typ) "macro expression"
      [ pVarExpr (MacVar typ) (MacTo typ)

      , pFun2 "AtPos" (pLIST (MacTo typ)) pINT (MacAtPos typ)
      , pFun3 "AtIdx" (pMAT (MacTo typ)) pINT pINT (MacAtIdx typ)

      , pMacroExprT pE (MacMacro typ)

      , pFun1 "Rand" (pLIST (MacTo typ)) (MacRand typ)

      , pIfThenElseExprT pBOOL (pMAC typ) (MacIfThenElse typ) (MacTo typ)
      ]

    macOpTable = []
