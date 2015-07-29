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

module Feivel.Parse.Perm (
  pPermConst, pPermLiteral, pTypedPermExpr
) where

import Feivel.Store (locus)
import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM
import Feivel.Lib (fromCycles, idPerm)

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pPermLiteral :: Type -> (Type -> ParseM Expr) -> ParseM PermExpr
pPermLiteral typ pC = fmap PermExpr $ pAtLocus $ pPermLiteralOf typ pC

pPermConst :: Type -> (Type -> ParseM Expr) -> ParseM PermExpr
pPermConst typ pC = fmap PermExpr $ pAtLocus $ pPermLiteralOf typ pC

pPermLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM (PermExprLeaf Expr BoolExpr IntExpr PermExpr)
pPermLiteralOf typ pC = (string "id" >> return (PermConst typ idPerm)) <|> do
  start <- getPosition
  ts <- many1 pCycle
  end <- getPosition
  case fromCycles ts of
    Right q -> return (PermConst typ q)
    Left err -> reportParseErr (locus start end) err
    where
      pCycle = do
        _ <- try $ char '('
        xs <- sepBy1 (pC typ) whitespace
        _ <- char ')'
        return xs

pTypedPermExpr :: Type -> (Type -> ParseM Expr) -> ParseM BoolExpr -> ParseM IntExpr -> (Type -> ParseM PermExpr) -> ParseM PermExpr
pTypedPermExpr typ pE pBOOL pINT pPERM = spaced $ buildExpressionParser permOpTable pPermTerm
  where
    pPermTerm = pTerm' (pPermLiteralOf typ pE) PermExpr (pPERM typ) "permutation expression"
      [ pVarExpr (PermVar typ) (PermOf typ)

      , pFun2 "AtPos" (pE $ ListOf (PermOf typ)) pINT (PermAtPos typ) 
      , pFun3 "AtIdx" (pE $ MatOf (PermOf typ)) pINT pINT (PermAtIdx typ)

      , pMacroExprT pE (PermMacro typ)

      , pIfThenElseExprT pBOOL (pPERM typ) (PermIfThenElse typ) (PermOf typ)

      , pFun1 "Rand" (pE $ ListOf (PermOf typ)) (PermRand typ)
      ]

    permOpTable =
      [ [ Prefix (opParser1' (PermInvert typ) PermExpr "inv")
        ]
      , [ Infix (opParser2' (PermCompose typ) PermExpr "o") AssocLeft
        ]
      ]
