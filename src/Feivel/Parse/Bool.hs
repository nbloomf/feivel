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

module Feivel.Parse.Bool (
  pBoolConst, pBoolExpr
) where

import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Prim (try)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))


pBoolConst :: ParseM BoolExpr
pBoolConst = fmap BoolExpr $ pAtLocus pBoolConst'

pBoolConst' :: ParseM BoolExprLeafS
pBoolConst' = pConst pBool BoolConst

pBoolExpr :: (Type -> ParseM Expr) -> ParseM IntExpr -> ParseM BoolExpr -> ParseM BoolExpr
pBoolExpr pE pINT pBOOL = spaced $ buildExpressionParser boolOpTable pBoolTerm
  where
    pBoolTerm = pTerm' pBoolConst' BoolExpr pBOOL "boolean expression"
      [ pVarExpr BoolVar BB
      , pMacroExprT pE BoolMacro

      , pFun2 "AtPos" (pE $ ListOf BB) pINT BoolAtPos
      , pFun3 "AtIdx" (pE $ MatOf BB) pINT pINT BoolAtIdx

      , pIfThenElseExprT pBOOL pBOOL BoolIfThenElse BB
    
      , pFun1 "IsDefined" pKey IsDefined

      , pFun1 "IsSquareFree" pINT IntSqFree

      , pFun1 "Rand" (pE $ ListOf BB) BoolRand

      , pFun2T "Elem" pE (pE . ListOf) ListElem
      , pFun1T "IsEmpty" (pE . ListOf) ListIsEmpty

      , pFun1T "IsRow"    (pE . MatOf) MatIsRow
      , pFun1T "IsCol"    (pE . MatOf) MatIsCol
      , pFun1T "IsGJForm" (pE . MatOf) MatIsGJForm

      , pFun2T "Equal"    pE pE BoolEq
      , pFun2T "NotEqual" pE pE BoolNEq
      , pFun2T "LT"       pE pE BoolLT
      , pFun2T "LEq"      pE pE BoolLEq
      , pFun2T "GT"       pE pE BoolGT
      , pFun2T "GEq"      pE pE BoolGEq

      , pFun2 "Matches" (pE SS) pText    Matches
      , pFun2 "Divides" pINT pINT IntDiv
      ]

    boolOpTable =
      [ [ Prefix (opParser1' Neg BoolExpr "~")
        ]
      , [ Infix (opParser2' Conj BoolExpr "&&") AssocLeft
        ]
      , [ Infix (opParser2' Disj BoolExpr "||") AssocLeft
        ]
      , [ Infix (opParser2' Imp BoolExpr "->") AssocRight
        ]
      ]
