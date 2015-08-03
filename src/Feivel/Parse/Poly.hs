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

import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM
import Feivel.Lib

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pPolyLiteral :: Type -> (Type -> ParseM Expr) -> ParseM PolyExpr
pPolyLiteral typ pE = fmap PolyExpr $ pAtLocus $ pPolyLiteralOf typ pE

pPolyConst :: Type -> (Type -> ParseM Expr) -> ParseM PolyExpr
pPolyConst typ pC = fmap PolyExpr $ pAtLocus $ pPolyLiteralOf typ pC

pMon :: ParseM (Monomial Variable)
pMon = pIdMon <|> pMonomial
  where
    pIdMon :: ParseM (Monomial Variable)
    pIdMon = (try $ char '1') >> return identity

    pMonomial :: ParseM (Monomial Variable)
    pMonomial = do
      ps <- sepBy1 pPower (try $ keyword ".")
      return $ makeMonomial ps
    
    pPower :: ParseM (Variable, Natural)
    pPower = do
      x <- pVar
      k <- option 1 (try (keyword "^") >> pNatural)
      return (x, Nat k)

pPolyLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM (PolyExprLeaf Expr BoolExpr IntExpr PolyExpr)
pPolyLiteralOf typ p = do
  try $ keyword "Poly"
  keyword "("
  ts <- sepBy1 (pPolyTerm $ p typ) (try $ char ';')
  keyword ")"
  return (PolyConst typ (fromTerms ts))
  where
    pPolyTerm :: ParseM a -> ParseM (a, Monomial Variable)
    pPolyTerm q = do
      c <- q
      x <- option identity $ try (keyword ".") >> pMon
      return (c,x)
    


pPolyExpr :: (Type -> ParseM Expr) -> (Type -> ParseM Expr) -> ParseM BoolExpr -> ParseM IntExpr -> (Type -> ParseM PolyExpr) -> ParseM PolyExpr
pPolyExpr pC pE pBOOL pINT pPOLY = pTypedPolyExpr XX pC pE pBOOL pINT pPOLY

pTypedPolyExpr :: Type -> (Type -> ParseM Expr) -> (Type -> ParseM Expr) -> ParseM BoolExpr -> ParseM IntExpr -> (Type -> ParseM PolyExpr) -> ParseM PolyExpr
pTypedPolyExpr typ pC pE pBOOL pINT pPOLY = spaced $ buildExpressionParser polyOpTable pPolyTerm
  where
    pPolyTerm = pTerm (pPolyLiteralOf typ pE) PolyExpr (pPOLY typ) "polynomial expression"
      [ pVarExpr (PolyVar typ) (PolyOver typ)

      , pFun2 "AtPos" (pE $ ListOf (PolyOver typ)) pINT (PolyAtPos typ)
      , pFun3 "AtIdx" (pE $ MatOf (PolyOver typ)) pINT pINT (PolyAtIdx typ)

      , pMacroExprT pE (PolyMacro typ)

      , pIfThenElseExprT pBOOL (pPOLY typ) (PolyIfThenElse typ) (PolyOver typ)

      , pFun1 "Rand" (pE $ ListOf (PolyOver typ)) (PolyRand typ)
      , pFun1 "Sum" (pE $ ListOf (PolyOver  typ)) (PolySum typ)

      , pFun2 "Pow" (pPOLY typ) pINT (PolyPow typ)

      , pPolyNull

      , pFun2 "FromRoots" pVar (pE $ ListOf typ) (PolyFromRoots typ)

      , pPolyEvalPoly
      , pPoly
      ]
      where
        pPoly = do
          try $ keyword "P"
          keyword "("
          terms <- sepBy1 foo (keyword "+")
          keyword ")"
          return (PolyConst typ (fromTerms terms))
            where
              foo = do
                a <- pC typ
                keyword "."
                m <- pMon
                return (a,m)

        pPolyEvalPoly = do
          try $ keyword "EvalPoly"
          keyword "("
          p <- pPOLY typ
          keyword ";"
          xs <- sepBy1 pSubs (keyword ";")
          keyword ")"
          return (PolyEvalPoly typ p xs)
            where
              pSubs = do
                x <- try $ pVar
                keyword "<-"
                q <- pPOLY typ
                return (x,q)

        pPolyNull = do
          try $ keyword "Null"
          return (PolyConst typ zeroPoly)

    polyOpTable =
      [ [ Prefix (opParser1 (PolyNeg typ) PolyExpr "neg")
        ]
      , [ Infix (opParser2 (PolyMul typ) PolyExpr "*") AssocLeft
        , Infix (opParser2 (PolyQuo typ) PolyExpr "quo") AssocLeft
        , Infix (opParser2 (PolyRem typ) PolyExpr "rem") AssocLeft
        ]
      , [ Infix (opParser2 (PolyAdd typ) PolyExpr "+") AssocLeft
        , Infix (opParser2 (PolySub typ) PolyExpr "-") AssocLeft
        ]
      ]
