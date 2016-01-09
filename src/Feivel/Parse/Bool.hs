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

module Feivel.Parse.Bool (
  pBoolConst, pBoolExpr
) where

import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))


pBoolConst :: ParseM BoolExpr
pBoolConst = fmap BoolExpr $ pAtLocus pBoolConst'

pBoolConst' :: ParseM BoolExprLeafS
pBoolConst' = fmap BoolConst pBool

pBoolExpr :: (Type -> ParseM Expr) -> ParseM IntExpr -> ParseM BoolExpr -> (Type -> ParseM ListExpr) -> (Type -> ParseM MatExpr) -> ([Type] -> ParseM TupleExpr) -> ParseM BoolExpr
pBoolExpr pE pINT pBOOL pLIST pMAT pTUPLE = spaced $ buildExpressionParser boolOpTable pBoolTerm
  where
    pBoolTerm = pTerm pBoolConst' BoolExpr pBOOL "boolean expression"
      [ pVarExpr BoolVar BB
      , pMacroExprT pE BoolMacro

      , pFun2   "AtPos"  (pLIST BB) pINT      BoolAtPos
      , pFun3   "AtIdx"  (pMAT BB)  pINT pINT BoolAtIdx
      , pAtSlot "AtSlot" pTUPLE     pINT      BoolAtSlot

      , pIfThenElseExprT pBOOL pBOOL BoolIfThenElse BB
    
      , pFun1 "IsDefined" pKey IsDefined

      , pFun1 "IsSquareFree" pINT IntSqFree

      , pFun1 "Rand" (pLIST BB) BoolRand

      , pFun2T "Elem"    pE pLIST ListElem
      , pFun1T "IsEmpty" pLIST ListIsEmpty

      , pFun1T "IsRow"    pMAT MatIsRow
      , pFun1T "IsCol"    pMAT MatIsCol
      , pFun1T "IsGJForm" pMAT MatIsGJForm

      , pFun2T "Equal"    pE pE BoolEq
      , pFun2T "NotEqual" pE pE BoolNEq
      , pFun2T' "LT"      pE pE BoolLT
      , pFun2T' "LEq"     pE pE BoolLEq
      , pFun2T' "GT"      pE pE BoolGT
      , pFun2T' "GEq"     pE pE BoolGEq

      , pFun2 "Matches" (pE SS) pText    Matches
      , pFun2 "Divides" pINT pINT IntDiv
      ]

    boolOpTable =
      [ [ Prefix (opParser1 Neg  BoolExpr "~")
        ]
      , [ Infix  (opParser2 Conj BoolExpr "&&") AssocLeft
        ]
      , [ Infix  (opParser2 Disj BoolExpr "||") AssocLeft
        ]
      , [ Infix  (opParser2 Imp  BoolExpr "->") AssocRight
        ]
      ]
