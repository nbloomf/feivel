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

import Feivel.Expr (Type(..), Expr(..), BoolExpr, BoolExprLeaf(..))
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Prim (try)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))


pBoolConst :: ParseM (BoolExpr Expr)
pBoolConst = pAtLocus pBoolConst'

pBoolConst' :: ParseM (BoolExprLeaf Expr)
pBoolConst' = pConst pBool BoolConst

pBoolExpr :: (Type -> ParseM Expr) -> ParseM (BoolExpr Expr)
pBoolExpr pE = spaced $ buildExpressionParser boolOpTable pBoolTerm
  where
    pBoolTerm = pTerm pBoolConst' (pBoolExpr pE) "boolean expression"
      [ pVarExpr BoolVar BB
      , pMacroExprT pE BoolMacro

      , pFun2 "AtPos" (pE $ ListOf BB) (pE ZZ) BoolAtPos
      , pFun3 "AtIdx" (pE $ MatOf BB) (pE ZZ) (pE ZZ) BoolAtIdx

      , pIfThenElseExprT pE (pBoolExpr pE) BoolIfThenElse BB
    
      , pFun1 "IsDefined" pKey IsDefined

      , pFun1 "IsSquareFree" (pE ZZ) IntSqFree

      , pFun1 "Rand" (pE $ ListOf BB) BoolRand

      , pFun2T "Elem" pE (pE . ListOf) ListElem
      , pFun1T "IsEmpty" (pE . ListOf) ListIsEmpty

      , pFun1 "IsRow" (pE $ MatOf XX) MatIsRow
      , pFun1 "IsCol" (pE $ MatOf XX) MatIsCol
      , pFun1T "IsGJForm" (pE . MatOf) MatIsGJForm

      , pFun2T "Equal"    pE pE BoolEq
      , pFun2T "NotEqual" pE pE BoolNEq
      , pFun2T "LT"       pE pE BoolLT
      , pFun2T "LEq"      pE pE BoolLEq
      , pFun2T "GT"       pE pE BoolGT
      , pFun2T "GEq"      pE pE BoolGEq

      , pFun2 "Matches" (pE SS) pText    Matches
      , pFun2 "Divides" (pE ZZ) (pE ZZ) IntDiv
      ]

    boolOpTable =
      [ [ Prefix (opParser1 Neg "~")
        ]
      , [ Infix (opParser2 Conj "&&") AssocLeft
        ]
      , [ Infix (opParser2 Disj "||") AssocLeft
        ]
      , [ Infix (opParser2 Imp  "->") AssocRight
        ]
      ]
