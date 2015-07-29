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

module Feivel.Parse.Int (
  pIntConst, pIntExpr
) where

import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))


pIntConstLeaf :: ParseM IntExprLeafS
pIntConstLeaf = pConst pInteger IntConst

pIntConst :: ParseM IntExpr
pIntConst = fmap IntExpr $ pAtLocus pIntConstLeaf

pIntExpr :: (Type -> ParseM Expr) -> ParseM BoolExpr -> ParseM IntExpr -> ParseM IntExpr
pIntExpr pE pBOOL pINT = spaced $ buildExpressionParser intOpTable pIntTerm
  where
    pIntTerm = pTerm pIntConstLeaf IntExpr pINT "integer expression"
      [ pVarExpr IntVar ZZ
      , pMacroExprT pE IntMacro

      , pFun2 "AtPos" (pE (ListOf ZZ)) pINT IntAtPos
      , pFun3 "AtIdx" (pE (MatOf ZZ)) pINT pINT IntAtIdx
    
      , pIfThenElseExprT pBOOL pINT IntIfThenElse ZZ

      , pFun1 "SquarePart"     pINT IntSqPart
      , pFun1 "SquareFreePart" pINT IntSqFreePart
      , pFun1 "Rad"            pINT IntRad

      , pFun1 "Length" (pE (ListOf XX)) ListLen
      , pFun1 "Rand"   (pE (ListOf ZZ)) IntRand
      , pFun1 "Sum"    (pE (ListOf ZZ)) IntSum
      , pFun1 "Prod"   (pE (ListOf ZZ)) IntProd
      , pFun1 "Min"    (pE (ListOf ZZ)) IntMinim
      , pFun1 "Max"    (pE (ListOf ZZ)) IntMaxim
      , pFun1 "GCD"    (pE (ListOf ZZ)) IntGCDiv
      , pFun1 "LCM"    (pE (ListOf ZZ)) IntLCMul
    
      , pFun1 "Numerator"   (pE QQ)  RatNumer
      , pFun1 "Denominator" (pE QQ)  RatDenom
      , pFun1 "Floor"       (pE QQ)  RatFloor
    
      , pFun1 "StrLen" (pE SS)  StrLength

      , pFun1T "NumRows"    (pE . MatOf) MatNumRows
      , pFun1T "NumCols"    (pE . MatOf) MatNumCols
      , pFun1T "MatrixRank" (pE . MatOf) MatRank

      , pFun1 "PolyContent" (pE (PolyOver ZZ)) IntContent
    
      , pFun2 "Uniform"  pINT pINT IntObserveUniform
      , pFun2 "Binomial" pINT (pE QQ) IntObserveBinomial
      , pFun1 "Poisson"  (pE QQ) IntObservePoisson

      , pFun1 "str" (pE SS) IntCastStr
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
