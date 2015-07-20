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

import Feivel.Grammar (Type(..), Expr(..), IntExpr, IntExprLeaf(..))
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))


pIntConstLeaf :: ParseM (IntExprLeaf Expr)
pIntConstLeaf = pConst pInteger IntConst

pIntConst :: ParseM (IntExpr Expr)
pIntConst = pAtLocus pIntConstLeaf

pIntExpr :: (Type -> ParseM Expr) -> ParseM (IntExpr Expr)
pIntExpr pE = spaced $ buildExpressionParser intOpTable pIntTerm
  where
    pIntTerm = pTerm pIntConstLeaf (pIntExpr pE) "integer expression"
      [ pVarExpr IntVar ZZ
      , pMacroExprT pE IntMacro

      , pFun2 "AtPos" (pE (ListOf ZZ)) (pE ZZ) IntAtPos
      , pFun3 "AtIdx" (pE (MatOf ZZ)) (pE ZZ) (pE ZZ) IntAtIdx
    
      , pIfThenElseExprT pE (pIntExpr pE) IntIfThenElse ZZ

      , pFun1 "SquarePart"     (pIntExpr pE) IntSqPart
      , pFun1 "SquareFreePart" (pIntExpr pE) IntSqFreePart
      , pFun1 "Rad"            (pIntExpr pE) IntRad

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

      , pFun1  "NumRows"    (pE (MatOf XX)) MatNumRows
      , pFun1  "NumCols"    (pE (MatOf XX)) MatNumCols
      , pFun1T "MatrixRank" (pE . MatOf)    MatRank

      , pFun1 "PolyContent" (pE (PolyOver ZZ)) IntContent
    
      , pFun2 "Uniform"  (pIntExpr pE) (pIntExpr pE) IntObserveUniform
      , pFun2 "Binomial" (pIntExpr pE) (pE QQ) IntObserveBinomial
      , pFun1 "Poisson"  (pE QQ) IntObservePoisson

      , pFun1 "str" (pE SS) IntCastStr
      ]

    intOpTable =
      [ [ Infix (opParser2 IntPow "^") AssocRight
        ]
      , [ Infix (opParser2 IntMult "*") AssocLeft
        ]
      , [ Infix (opParser2 IntQuo "div") AssocLeft
        , Infix (opParser2 IntMod "mod") AssocLeft
        ]
      , [ Prefix (opParser1 IntNeg "neg")
        , Prefix (opParser1 IntAbs "abs")
        ]
      , [ Infix (opParser2 IntAdd "+") AssocLeft
        , Infix (opParser2 IntSub "-") AssocLeft
        ]
      , [ Infix (opParser2 IntMin "min") AssocLeft
        , Infix (opParser2 IntMax "max") AssocLeft
        , Infix (opParser2 IntGCD "gcd") AssocLeft
        , Infix (opParser2 IntLCM "lcm") AssocLeft
        ]
      , [ Infix (opParser2 IntChoose "choose") AssocLeft]
      ]
