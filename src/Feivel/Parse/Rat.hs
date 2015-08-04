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

module Feivel.Parse.Rat (
  pRatConst, pRatExpr
) where

import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Prim (try)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))


pRatConst :: ParseM RatExpr
pRatConst = fmap RatExpr $ pAtLocus pRatConst'

pRatConst' :: ParseM RatExprLeafS
pRatConst' = fmap RatConst pRat

pRatExpr :: (Type -> ParseM Expr) -> ParseM BoolExpr -> ParseM IntExpr -> (Type -> ParseM ListExpr) -> ParseM RatExpr -> ParseM RatExpr
pRatExpr pE pBOOL pINT pLIST pRAT = spaced $ buildExpressionParser ratOpTable pRatTerm
  where
    pRatTerm = pTerm pRatConst' RatExpr pRAT "rational expression"
      [ pVarExpr RatVar QQ
      , pMacroExprT pE RatMacro

      , pFun2 "AtPos" (pLIST QQ) pINT RatAtPos
      , pFun3 "AtIdx" (pE $ MatOf QQ) pINT pINT RatAtIdx

      , pIfThenElseExprT pBOOL pRAT RatIfThenElse QQ

      , pFun1 "Rand"   (pLIST QQ) RatRand
      , pFun1 "Sum"    (pLIST QQ) RatSum
      , pFun1 "Prod"   (pLIST QQ) RatProd
      , pFun1 "Min"    (pLIST QQ) RatMinim
      , pFun1 "Max"    (pLIST QQ) RatMaxim

      , pFun1  "int"     (pE ZZ) RatCast
      , pFun2  "Pow"     pRAT pINT RatPow
      , pFun1T "Mean"    pLIST RatMean
      , pFun2  "Sqrt"    pRAT pINT RatSqrt
      , pFun1T "MeanDev" pLIST RatMeanDev
      , pFun2T "StdDev"  pLIST (const pINT) RatStdDev
      , pZScore

      , pFun1 "str" (pE SS) RatCastStr
      ]
      where
        pZScore = do
          try $ keyword "ZScore"
          keyword "("
          t <- pType
          keyword ";"
          p <- pRAT
          keyword ";"
          ks <- pLIST t
          keyword ";"
          n <- pINT
          keyword ")"
          return (RatZScore p ks n)
    
    ratOpTable =
      [ [ Prefix (opParser1 RatNeg  RatExpr "neg")
        , Prefix (opParser1 RatAbs  RatExpr "abs")
        ]
      , [ Infix  (opParser2 RatMult RatExpr "*")   AssocLeft
        , Infix  (opParser2 RatQuot RatExpr "/")   AssocLeft
        ]
      , [ Infix  (opParser2 RatAdd  RatExpr "+")   AssocLeft
        , Infix  (opParser2 RatSub  RatExpr "-")   AssocLeft
        ]
      , [ Infix  (opParser2 RatMin  RatExpr "min") AssocLeft
        , Infix  (opParser2 RatMax  RatExpr "max") AssocLeft
        ]
      ]
