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

module Feivel.Parse.List (
  pListConst, pListExpr, pTypedListExpr, pListLiteral
) where

import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pListLiteral :: Type -> (Type -> ParseM Expr) -> ParseM ListExpr
pListLiteral typ pE = fmap ListExpr $ pAtLocus $ pListLiteralOf typ pE

pListConst :: Type -> (Type -> ParseM Expr) -> ParseM ListExpr
pListConst typ pC = fmap ListExpr $ pAtLocus $ pListLiteralOf typ pC

pListLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM (OfType ListExprLeafS)
pListLiteralOf typ pE = do
    xs <- pBraceList (pE typ)
    return (ListConst xs :# typ)

pListExpr :: (Type -> ParseM Expr) -> ParseM BoolExpr -> ParseM IntExpr -> (Type -> ParseM ListExpr) -> (Type -> ParseM MatExpr) -> ParseM ListExpr
pListExpr pE pBOOL pINT pLIST pMAT = pTypedListExpr XX pE pBOOL pINT pLIST pMAT

pTypedListExpr :: Type -> (Type -> ParseM Expr) -> ParseM BoolExpr -> ParseM IntExpr -> (Type -> ParseM ListExpr) -> (Type -> ParseM MatExpr) -> ParseM ListExpr
pTypedListExpr typ pE pBOOL pINT pLIST pMAT = spaced $ buildExpressionParser listOpTable pListTerm
  where
    pListTerm = pTerm (pListLiteralOf typ pE) ListExpr (pLIST typ) "list expression"
      [ pVarExpr ((:# typ) `o` ListVar) (ListOf typ)

      , pMacroExprT pE ((:# typ) `oo` ListMacro)

      , pFun2 "AtPos" (pE $ ListOf (ListOf typ)) pINT ((:# typ) `oo` ListAtPos)
      , pFun3 "AtIdx" (pMAT (ListOf typ)) pINT pINT ((:# typ) `ooo` ListAtIdx)

      , pIfThenElseExprT pBOOL (pLIST typ) ((:# typ) `ooo` ListIfThenElse) (ListOf typ)

      , pFun1 "Rand" (pE $ ListOf (ListOf typ)) ((:# typ) `o` ListRand)

      , pFun1 "Bezout" (pLIST typ) ((:# typ) `o` ListBezouts)

      , pFun1 "Reverse"  (pLIST typ) ((:# typ) `o` ListRev)
      , pFun1 "Sort"     (pLIST typ) ((:# typ) `o` ListSort)
      , pFun1 "Unique"   (pLIST typ) ((:# typ) `o` ListUniq)
      , pFun1 "Shuffle"  (pLIST typ) ((:# typ) `o` ListShuffle)
      , pListShuffles

      , pFun2 "GetRow" pINT (pMAT typ) ((:# typ) `oo` ListMatRow)
      , pFun2 "GetCol" pINT (pMAT typ) ((:# typ) `oo` ListMatCol)

      , pListPermsOf

      , pListRange
      , pListPivotColIndices typ
      , pListBuilder
      , pFun2 "Choose" pINT (pLIST typ) ((:# typ) `oo` ListChoose)
      , pListChoices
      , pListFilter
      ]
      where
        pListFilter = do
          _ <- try $ keyword "Filter"
          keyword "("
          k <- pKey
          keyword ";"
          g <- pE BB
          keyword ";"
          xs <- pLIST typ
          keyword ")"
          return (ListFilter k g xs :# typ)

        pListChoices = do
          _ <- try $ keyword "Choices"
          case typ of
            ListOf t -> do
              keyword "("
              n <- pINT
              keyword ";"
              xs <- pLIST t
              keyword ")"
              return (ListChoices n xs :# typ)
            _ -> error "pListChoices"

        pListShuffles = do
          _ <- try $ keyword "Shuffles"
          case typ of
            ListOf t -> do
              keyword "("
              xs <- pLIST t
              keyword ")"
              return (ListShuffles xs :# (ListOf t))
            _ -> error "pListShuffles"

        pListPermsOf = do
          _ <- try $ keyword "PermutationsOf"
          case typ of
            PermOf t -> do
              keyword "("
              xs <- pLIST t
              keyword ")"
              return (ListPermsOf xs :# (PermOf t))
            _ -> error "pListPermsOf"

        pListRange = if typ == ZZ
          then do
            try $ keyword "Range"
            (a,b) <- pTuple2 pINT pINT
            return (ListRange a b :# ZZ)
          else fail "pListRange"

        pListPivotColIndices ZZ = do
          try $ keyword "PivotCols"
          keyword "("
          t <- pType
          keyword ";"
          m <- pMAT t
          keyword ")"
          return (ListPivotColIndices m :# ZZ)
        pListPivotColIndices _ = fail "pListPivotColIndices"
    
        pListBuilder = do
          try $ keyword "Build"
          keyword "("
          expr <- pE typ
          keyword ";"
          gds <- sepBy1 (pListBind <|> pListGuard) (keyword ";")
          keyword ")"
          return (ListBuilder expr gds :# typ)
            where
              pListBind = do
                w <- try pType
                whitespace
                k <- pKey
                keyword "<-"
                ls <- pLIST w
                return $ Bind k ls
          
              pListGuard = do
                e <- try (pE BB)
                return $ Guard e
    
    listOpTable =
      [ [ Infix (opParser2 ((:# typ) `oo` ListCat)  ListExpr "++")   AssocLeft
        ]
      , [ Infix (opParser2 ((:# typ) `oo` ListToss) ListExpr "\\\\") AssocLeft
        ]
      ]
