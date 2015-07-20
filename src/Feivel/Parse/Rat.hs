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

import Feivel.Expr (Type(..), Expr(..), RatExpr, RatExprLeaf(..))
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Prim (try)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))


pRatConst :: ParseM (RatExpr Expr)
pRatConst = pAtLocus pRatConst'

pRatConst' :: ParseM (RatExprLeaf Expr)
pRatConst' = pConst pRat RatConst

pRatExpr :: (Type -> ParseM Expr) -> ParseM (RatExpr Expr)
pRatExpr pE = spaced $ buildExpressionParser ratOpTable pRatTerm
  where
    pRatTerm = pTerm pRatConst' (pRatExpr pE) "rational expression"
      [ pVarExpr RatVar QQ
      , pMacroExprT pE RatMacro

      , pFun2 "AtPos" (pE $ ListOf QQ) (pE ZZ) RatAtPos
      , pFun3 "AtIdx" (pE $ MatOf QQ) (pE ZZ) (pE ZZ) RatAtIdx

      , pIfThenElseExprT pE (pRatExpr pE) RatIfThenElse QQ

      , pFun1 "Rand"   (pE $ ListOf QQ) RatRand
      , pFun1 "Sum"    (pE $ ListOf QQ) RatSum
      , pFun1 "Prod"   (pE $ ListOf QQ) RatProd
      , pFun1 "Min"    (pE $ ListOf QQ) RatMinim
      , pFun1 "Max"    (pE $ ListOf QQ) RatMaxim

      , pFun1  "int" (pE ZZ) RatCast
      , pFun2  "Pow" (pRatExpr pE) (pE ZZ) RatPow
      , pFun1T "Mean" (pE . ListOf) RatMean
      , pFun2  "Sqrt" (pRatExpr pE) (pE ZZ) RatSqrt
      , pFun1T "MeanDev" (pE . ListOf) RatMeanDev
      , pFun2T "StdDev" (pE . ListOf) (const (pE ZZ)) RatStdDev
      , pZScore

      , pFun1 "str" (pE SS) RatCastStr
      ]
      where
        pZScore = do
          try $ keyword "ZScore"
          keyword "("
          t <- pType
          keyword ";"
          p <- (pRatExpr pE)
          keyword ";"
          ks <- pE (ListOf t)
          keyword ";"
          n <- pE ZZ
          keyword ")"
          return (RatZScore p ks n)
    
    ratOpTable =
      [ [ Prefix (opParser1 RatNeg "neg" )
        , Prefix (opParser1 RatAbs "abs")
        ]
      , [ Infix (opParser2 RatMult "*") AssocLeft
        , Infix (opParser2 RatQuot "/") AssocLeft
        ]
      , [ Infix (opParser2 RatAdd "+") AssocLeft
        , Infix (opParser2 RatSub "-") AssocLeft
        ]
      , [ Infix (opParser2 RatMin "min") AssocLeft
        , Infix (opParser2 RatMax "max") AssocLeft
        ]
      ]
