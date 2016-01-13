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

module Feivel.Parse.Tuple (
  pTupleConst, pTupleExpr, pTypedTupleExpr, pTupleLiteral
) where

import Carl.Struct.Tuple

import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM

--import Data.List (intersperse)
import Text.Parsec.Expr (buildExpressionParser)
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pTupleLiteral :: [Type] -> (Type -> ParseM Expr) -> ParseM TupleExpr
pTupleLiteral typs pE = fmap TupleExpr $ pAtLocus $ pTupleLiteralOf typs pE

pTupleConst :: [Type] -> (Type -> ParseM Expr) -> ParseM TupleExpr
pTupleConst typs pC = fmap TupleExpr $ pAtLocus $ pTupleLiteralOf typs pC

pCommaSep :: [ParseM Expr] -> ParseM [Expr]
pCommaSep []  = return []
pCommaSep [p] = do
  x <- p
  return [x]
pCommaSep (p:ps) = do
  x  <- p
  _  <- char ','
  xs <- pCommaSep ps
  return (x:xs)

pTupleLiteralOf :: [Type] -> (Type -> ParseM Expr) -> ParseM (OfType TupleExprLeafS)
pTupleLiteralOf typs pE = do
    _  <- try $ char '('
    xs <- pCommaSep $ map pE typs
    _  <- char ')'
    return (TupleConst (Tuple xs) :# (TupleOf typs))

pTupleExpr ::
  (Type -> ParseM Expr) -> ParserDict -> ParseM TupleExpr
pTupleExpr pE dict = pTypedTupleExpr [XX] pE dict

pTypedTupleExpr :: [Type] -> (Type -> ParseM Expr) -> ParserDict -> ParseM TupleExpr
pTypedTupleExpr typs pE dict 
  = spaced $ buildExpressionParser tupleOpTable pTupleTerm
    where
      pBOOL  = parseBOOL  dict
      pINT   = parseINT   dict
      pLIST  = parseLIST  dict
      pMAT   = parseMAT   dict
      pTUPLE = parseTUPLE dict

      pTupleTerm = pTerm (pTupleLiteralOf typs pE) TupleExpr (pTUPLE typs) "tuple expression"
        [ pVarExpr
            ((:# (TupleOf typs)) `o` TupleVar)
            (TupleOf typs)

        , pMacroExprT pE
            ((:# (TupleOf typs)) `oo` TupleMacro)

        , pFun2 "AtPos"
            (pE $ ListOf (TupleOf typs))
            pINT
            ((:# (TupleOf typs)) `oo` TupleAtPos)

        , pFun3 "AtIdx"
            (pMAT (TupleOf typs))
            pINT
            pINT
            ((:# (TupleOf typs)) `ooo` TupleAtIdx)

        , pAtSlot "AtSlot"
            pTUPLE
            pINT
            ((:# (TupleOf typs)) `oo` TupleAtSlot)

        , pIfThenElseExprT
            pBOOL
            (pTUPLE typs)
            ((:# (TupleOf typs)) `ooo` TupleIfThenElse)
            (TupleOf typs)

        , pFun1 "Rand"
           (pE $ ListOf (TupleOf typs))
           ((:# (TupleOf typs)) `o` TupleRand)
        ]

      tupleOpTable = []
