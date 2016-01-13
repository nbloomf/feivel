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

module Feivel.Parse.Perm (
  pPermConst, pPermLiteral, pTypedPermExpr
) where

import Feivel.Store (locus)
import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM
import Carl.Struct.Permutation (fromCycles, idPerm)

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pPermLiteral :: Type -> (Type -> ParseM Expr) -> ParseM PermExpr
pPermLiteral typ pC = fmap PermExpr $ pAtLocus $ pPermLiteralOf typ pC

pPermConst :: Type -> (Type -> ParseM Expr) -> ParseM PermExpr
pPermConst typ pC = fmap PermExpr $ pAtLocus $ pPermLiteralOf typ pC

pPermLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM (OfType PermExprLeafS)
pPermLiteralOf typ pC = (string "id" >> return (PermConst idPerm :# typ)) <|> do
  start <- getPosition
  ts <- many1 pCycle
  end <- getPosition
  case fromCycles ts of
    Right q -> return (PermConst q :# typ)
    Left err -> reportParseErr (locus start end) err
    where
      pCycle = do
        _ <- try $ char '('
        xs <- sepBy1 (pC typ) whitespace
        _ <- char ')'
        return xs

pTypedPermExpr :: Type -> (Type -> ParseM Expr) -> ParserDict -> ParseM PermExpr
pTypedPermExpr typ pE dict = spaced $ buildExpressionParser permOpTable pPermTerm
  where
    pBOOL  = parseBOOL  dict
    pINT   = parseINT   dict
    pLIST  = parseLIST  dict
    pMAT   = parseMAT   dict
    pPERM  = parsePERM  dict
    pTUPLE = parseTUPLE dict

    pPermTerm = pTerm (pPermLiteralOf typ pE) PermExpr (pPERM typ) "permutation expression"
      [ pVarExpr ((:# typ) `o` PermVar) (PermOf typ)

      , pFun2 "AtPos" (pLIST (PermOf typ)) pINT ((:# typ) `oo` PermAtPos)
      , pFun3 "AtIdx" (pMAT  (PermOf typ)) pINT pINT ((:# typ) `ooo` PermAtIdx)
      , pAtSlot "AtSlot" pTUPLE     pINT      ((:# typ) `oo` PermAtSlot)

      , pMacroExprT pE ((:# typ) `oo` PermMacro)

      , pIfThenElseExprT pBOOL (pPERM typ) ((:# typ) `ooo` PermIfThenElse) (PermOf typ)

      , pFun1 "Rand" (pLIST (PermOf typ)) ((:# typ) `o` PermRand)
      ]

    permOpTable =
      [ [ Prefix (opParser1 ((:# typ) `o`  PermInvert)  PermExpr "inv")
        ]
      , [ Infix  (opParser2 ((:# typ) `oo` PermCompose) PermExpr "o")   AssocLeft
        ]
      ]
