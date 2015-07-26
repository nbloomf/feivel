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

module Feivel.Parse.Str (
  pStrConst, pStrExpr
) where

import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))


pStrConst :: ParseM StrExpr
pStrConst = fmap StrExpr $ pAtLocus pStrConst'

pStrConst' :: ParseM (StrExprLeaf Expr StrExpr)
pStrConst' = pConst pText StrConst

pStrExpr :: (Type -> ParseM Expr) -> ParseM StrExpr -> ParseM StrExpr
pStrExpr pE pSTR = spaced $ buildExpressionParser strOpTable pStrTerm
  where
    pStrTerm = pTerm' pStrConst' StrExpr pSTR "string expression"
      [ pVarExpr StrVar SS
      , pMacroExprT pE StrMacro

      , pIfThenElseExprT pE pSTR StrIfThenElse SS

      , pFun2 "AtPos" (pE $ ListOf SS) (pE ZZ) StrAtPos
      , pFun3 "AtIdx" (pE $ MatOf SS) (pE ZZ) (pE ZZ) StrAtIdx
    
      , pFun2 "Strip"   pSTR pSTR StrStrip
      , pFun1 "Reverse" pSTR Reverse
      , pFun1 "ToUpper" pSTR ToUpper
      , pFun1 "ToLower" pSTR ToLower
      , pFun1 "Rot13"   pSTR Rot13

      , pFun1 "Rand"    (pE $ ListOf SS) StrRand

      , pFun1 "int" (pE ZZ) StrIntCast
    
      , pFun1 "Hex"    (pE ZZ) StrHex
      , pFun1 "Roman"  (pE ZZ) StrRoman
      , pFun1 "Base36" (pE ZZ) StrBase36
    
      , pFun1T "Tab" (pE . MatOf) StrTab

      , pFun2  "Decimal" (pE QQ) (pE ZZ) StrDecimal
      , pFun2T "Format"  (const pFormat) pE      StrFormat
      ]

    strOpTable =
      [ [Infix (opParser2' Concat StrExpr "++") AssocLeft
        ]
      ]
