{---------------------------------------------------------------------}
{- Copyright 2015, 2016 Nathan Bloomfield                            -}
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
  :: Type -> (Type -> ParseM Expr) -> ParseM Expr -> ParseM (OfType MacExprLeafS)
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
    else return (MacConst vals body (emptyStore, False) :# t)


pTypedMacExpr :: Type -> (Type -> ParseM Expr) -> ParseM Expr -> ParseM BoolExpr
  -> ParseM IntExpr -> (Type -> ParseM ListExpr) -> (Type -> ParseM MatExpr)
  -> (Type -> ParseM MacExpr) -> ([Type] -> ParseM TupleExpr) -> ParseM MacExpr
pTypedMacExpr typ pE pBD pBOOL pINT pLIST pMAT pMAC pTUPLE = spaced $ buildExpressionParser macOpTable pMacTerm
  where
    pMacTerm = pTerm (pMacConst' typ pE pBD) MacExpr (pMAC typ) "macro expression"
      [ pVarExpr ((:# typ) `o` MacVar) (MacTo typ)

      , pFun2 "AtPos" (pLIST (MacTo typ)) pINT ((:# typ) `oo` MacAtPos)
      , pFun3 "AtIdx" (pMAT (MacTo typ)) pINT pINT ((:# typ) `ooo` MacAtIdx)
      , pAtSlot "AtSlot" pTUPLE     pINT      ((:# typ) `oo` MacAtSlot)

      , pMacroExprT pE ((:# typ) `oo` MacMacro)

      , pFun1 "Rand" (pLIST (MacTo typ)) ((:# typ) `o` MacRand)

      , pIfThenElseExprT pBOOL (pMAC typ) ((:# typ) `ooo` MacIfThenElse) (MacTo typ)
      ]

    macOpTable = []
