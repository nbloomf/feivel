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
import Feivel.Lib (nullP, fromListM, identityM, fromListP, Natural(..), Variable, Monomial)

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pPolyLiteral :: Type -> (Type -> ParseM Expr) -> ParseM PolyExpr
pPolyLiteral typ pE = fmap PolyExpr $ pAtLocus $ pPolyLiteralOf typ pE

pPolyConst :: Type -> (Type -> ParseM Expr) -> ParseM PolyExpr
pPolyConst typ pC = fmap PolyExpr $ pAtLocus $ pPolyLiteralOf typ pC

pPolyLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM (PolyExprLeaf Expr IntExpr PolyExpr)
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

pPolyExpr :: (Type -> ParseM Expr) -> ParseM IntExpr -> (Type -> ParseM PolyExpr) -> ParseM PolyExpr
pPolyExpr pE pINT pPOLY = pTypedPolyExpr XX pE pINT pPOLY

pTypedPolyExpr :: Type -> (Type -> ParseM Expr) -> ParseM IntExpr -> (Type -> ParseM PolyExpr) -> ParseM PolyExpr
pTypedPolyExpr typ pE pINT pPOLY = spaced $ buildExpressionParser polyOpTable pPolyTerm
  where
    pPolyTerm = pTerm' (pPolyLiteralOf typ pE) PolyExpr (pPOLY typ) "polynomial expression"
      [ pVarExpr (PolyVar typ) (PolyOver typ)

      , pFun2 "AtPos" (pE $ ListOf (PolyOver typ)) pINT (PolyAtPos typ)
      , pFun3 "AtIdx" (pE $ MatOf (PolyOver typ)) pINT pINT (PolyAtIdx typ)

      , pMacroExprT pE (PolyMacro typ)

      , pIfThenElseExprT pE (pPOLY typ) (PolyIfThenElse typ) (PolyOver typ)

      , pFun1 "Rand" (pE $ ListOf (PolyOver typ)) (PolyRand typ)

      , pFun2 "Pow" (pPOLY typ) pINT (PolyPow typ)

      , pPolyNull

      , pFun2 "FromRoots" pVar (pE $ ListOf typ) (PolyFromRoots typ)

      , pPolyEvalPoly
      ]
      where
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
          return (PolyConst typ nullP)

    polyOpTable =
      [ [ Prefix (opParser1' (PolyNeg typ) PolyExpr "neg")
        ]
      , [ Infix (opParser2' (PolyMul typ) PolyExpr "*") AssocLeft
        ]
      , [ Infix (opParser2' (PolyAdd typ) PolyExpr "+") AssocLeft
        , Infix (opParser2' (PolySub typ) PolyExpr "-") AssocLeft
        ]
      ]
