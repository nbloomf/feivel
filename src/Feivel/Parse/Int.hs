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

module Feivel.Parse.Int (
  pIntConst, pIntExpr
) where

import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))


pIntConstLeaf :: ParseM IntExprLeafS
pIntConstLeaf = fmap IntConst pInteger

pIntConst :: ParseM IntExpr
pIntConst = fmap IntExpr $ pAtLocus pIntConstLeaf

pIntExpr :: (Type -> ParseM Expr) -> ParserDict -> ParseM IntExpr
pIntExpr pE dict = spaced $ buildExpressionParser intOpTable pIntTerm
  where
    pBOOL  = parseBOOL  dict
    pINT   = parseINT   dict
    pRAT   = parseRAT   dict
    pLIST  = parseLIST  dict
    pMAT   = parseMAT   dict
    pSTR   = parseSTR   dict
    pPOLY  = parsePOLY  dict
    pTUPLE = parseTUPLE dict

    pIntTerm = pTerm pIntConstLeaf IntExpr pINT "integer expression"
      [ pVarExpr IntVar ZZ
      , pMacroExprT pE IntMacro

      , pFun2   "AtPos"  (pLIST ZZ) pINT      IntAtPos
      , pFun3   "AtIdx"  (pMAT  ZZ) pINT pINT IntAtIdx
      , pAtSlot "AtSlot" pTUPLE     pINT      IntAtSlot
    
      , pIfThenElseExprT pBOOL pINT IntIfThenElse ZZ

      , pFun1 "SquarePart"     pINT IntSqPart
      , pFun1 "SquareFreePart" pINT IntSqFreePart
      , pFun1 "Rad"            pINT IntRad

      , pFun1 "Length" (pLIST XX) ListLen
      , pFun1 "Rand"   (pLIST ZZ) IntRand
      , pFun1 "Sum"    (pLIST ZZ) IntSum
      , pFun1 "Prod"   (pLIST ZZ) IntProd
      , pFun1 "Min"    (pLIST ZZ) IntMinim
      , pFun1 "Max"    (pLIST ZZ) IntMaxim
      , pFun1 "GCD"    (pLIST ZZ) IntGCDiv
      , pFun1 "LCM"    (pLIST ZZ) IntLCMul
    
      , pFun1 "Numerator"   pRAT RatNumer
      , pFun1 "Denominator" pRAT RatDenom
      , pFun1 "Floor"       pRAT RatFloor
    
      , pFun1 "StrLen" pSTR  StrLength

      , pFun1T "NumRows"    pMAT  MatNumRows
      , pFun1T "NumCols"    pMAT  MatNumCols
      , pFun1T "MatrixRank" pMAT  MatRank
      , pFun1T' "Degree"    pPOLY PolyDegree

      , pFun1 "PolyContent" (pPOLY ZZ) IntContent
    
      , pFun2 "Uniform"  pINT pINT IntObserveUniform
      , pFun2 "Binomial" pINT pRAT IntObserveBinomial
      , pFun1 "Poisson"  pRAT      IntObservePoisson

      , pFun1 "str" pSTR IntCastStr
      ]

    intOpTable =
      [ [ Infix (opParser2 IntPow IntExpr "^") AssocRight
        ]
      , [ Infix (opParser2 IntMult IntExpr "*") AssocLeft
        ]
      , [ Infix (opParser2 IntQuo IntExpr "div") AssocLeft
        , Infix (opParser2 IntMod IntExpr "mod") AssocLeft
        ]
      , [ Prefix (opParser1 IntNeg IntExpr "neg")
        , Prefix (opParser1 IntAbs IntExpr "abs")
        ]
      , [ Infix (opParser2 IntAdd IntExpr "+") AssocLeft
        , Infix (opParser2 IntSub IntExpr "-") AssocLeft
        ]
      , [ Infix (opParser2 IntMin IntExpr "min") AssocLeft
        , Infix (opParser2 IntMax IntExpr "max") AssocLeft
        , Infix (opParser2 IntGCD IntExpr "gcd") AssocLeft
        , Infix (opParser2 IntLCM IntExpr "lcm") AssocLeft
        ]
      , [ Infix (opParser2 IntChoose IntExpr "choose") AssocLeft]
      ]
