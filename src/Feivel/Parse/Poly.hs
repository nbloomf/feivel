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

module Feivel.Parse.Poly (
  pPolyConst, pTypedPolyExpr, pPolyExpr, pPolyLiteral
) where

import Feivel.Grammar (Type(..), Expr(..), PolyExpr, PolyExprLeaf(..))
import Feivel.Parse.Util
import Feivel.Parse.ParseM
import Feivel.Lib (nullP, fromListM, identityM, fromListP, Natural(..), Variable, Monomial)

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pPolyLiteral :: Type -> (Type -> ParseM Expr) -> ParseM (PolyExpr Expr)
pPolyLiteral typ pE = pAtLocus $ pPolyLiteralOf typ pE

pPolyConst :: Type -> (Type -> ParseM Expr) -> ParseM (PolyExpr Expr)
pPolyConst typ pC = pAtLocus $ pPolyLiteralOf typ pC

pPolyLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM (PolyExprLeaf Expr)
pPolyLiteralOf typ p = do
  try $ keyword "Poly"
  keyword "("
  ts <- sepBy1 (pPolyTerm $ p typ) (try $ char ';')
  keyword ")"
  return (PolyConst typ (fromListP ts))
  where
    pPolyTerm :: ParseM a -> ParseM (a, Monomial)
    pPolyTerm q = do
      c <- q
      x <- option identityM $ try (keyword ".") >> (pIdMon <|> pMonomial)
      return (c,x)
    
    pIdMon :: ParseM Monomial
    pIdMon = (try $ char '1') >> return identityM

    pMonomial :: ParseM Monomial
    pMonomial = do
      ps <- sepBy1 pPower (try $ keyword ".")
      return $ fromListM ps
    
    pPower :: ParseM (Variable, Natural)
    pPower = do
      x <- pVar
      k <- option 1 (try (keyword "^") >> pNatural)
      return (x, Nat k)

pPolyExpr :: (Type -> ParseM Expr) -> ParseM (PolyExpr Expr)
pPolyExpr pE = pTypedPolyExpr XX pE

pTypedPolyExpr :: Type -> (Type -> ParseM Expr) -> ParseM (PolyExpr Expr)
pTypedPolyExpr typ pE = spaced $ buildExpressionParser polyOpTable pPolyTerm
  where
    pPolyTerm = pTerm (pPolyLiteralOf typ pE) (pTypedPolyExpr typ pE) "polynomial expression"
      [ pVarExpr (PolyVar typ) (PolyOver typ)

      , pFun2 "AtPos" (pE $ ListOf (PolyOver typ)) (pE ZZ) (PolyAtPos typ)
      , pFun3 "AtIdx" (pE $ MatOf (PolyOver typ)) (pE ZZ) (pE ZZ) (PolyAtIdx typ)

      , pMacroExprT pE (PolyMacro typ)

      , pIfThenElseExprT pE (pTypedPolyExpr typ pE) (PolyIfThenElse typ) (PolyOver typ)

      , pFun1 "Rand" (pE $ ListOf (PolyOver typ)) (PolyRand typ)

      , pFun2 "Pow" (pTypedPolyExpr typ pE) (pE ZZ) (PolyPow typ)

      , pPolyNull

      , pFun2 "FromRoots" (pLiftAt pVar XX) (pE $ ListOf typ) (PolyFromRoots typ)

      , pPolyEvalPoly
      ]
      where
        pPolyEvalPoly = do
          try $ keyword "EvalPoly"
          keyword "("
          p <- pTypedPolyExpr typ pE
          keyword ";"
          xs <- sepBy1 pSubs (keyword ";")
          keyword ")"
          return (PolyEvalPoly typ p xs)
            where
              pSubs = do
                x <- try $ pVar
                keyword "<-"
                q <- pTypedPolyExpr typ pE
                return (x,q)

        pPolyNull = do
          try $ keyword "Null"
          return (PolyConst typ nullP)

    polyOpTable =
      [ [ Prefix (opParser1 (PolyNeg typ) "neg")
        ]
      , [ Infix (opParser2 (PolyMul typ) "*") AssocLeft
        ]
      , [ Infix (opParser2 (PolyAdd typ) "+") AssocLeft
        , Infix (opParser2 (PolySub typ) "-") AssocLeft
        ]
      ]

