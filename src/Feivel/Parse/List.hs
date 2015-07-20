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

import Feivel.Store (locus)
import Feivel.Grammar (Type(..), Expr(..), ListExpr, ListExprLeaf(..), ListGuard(..))
import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pListLiteral :: Type -> (Type -> ParseM Expr) -> ParseM (ListExpr Expr)
pListLiteral typ pE = pAtLocus $ pListLiteralOf typ pE

pListConst :: Type -> (Type -> ParseM Expr) -> ParseM (ListExpr Expr)
pListConst typ pC = pAtLocus $ pListLiteralOf typ pC

pListLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM (ListExprLeaf Expr)
pListLiteralOf typ pE = do
    xs <- pBraceList (pE typ)
    return (ListConst typ xs)

pListExpr :: (Type -> ParseM Expr) -> ParseM (ListExpr Expr)
pListExpr pE = pTypedListExpr XX pE

pTypedListExpr :: Type -> (Type -> ParseM Expr) -> ParseM (ListExpr Expr)
pTypedListExpr typ pE = spaced $ buildExpressionParser listOpTable pListTerm
  where
    pListTerm = pTerm (pListLiteralOf typ pE) (pTypedListExpr typ pE) "list expression"
      [ pVarExpr (ListVar typ) (ListOf typ)

      , pMacroExprT pE (ListMacro typ)

      , pFun2 "AtPos" (pE $ ListOf (ListOf typ)) (pE ZZ) (ListAtPos typ)
      , pFun3 "AtIdx" (pE $ MatOf (ListOf typ)) (pE ZZ) (pE ZZ) (ListAtIdx typ)

      , pIfThenElseExprT pE (pTypedListExpr typ pE) (ListIfThenElse typ) (ListOf typ)

      , pFun1 "Rand" (pE $ ListOf (ListOf typ)) (ListRand typ)

      , pFun1 "Reverse"  (pTypedListExpr typ pE) (ListRev typ)
      , pFun1 "Sort"     (pTypedListExpr typ pE) (ListSort typ)
      , pFun1 "Unique"   (pTypedListExpr typ pE) (ListUniq typ)
      , pFun1 "Shuffle"  (pTypedListExpr typ pE) (ListShuffle typ)
      , pListShuffles

      , pFun2 "GetRow" (pE ZZ) (pE $ MatOf typ) (ListMatRow typ)
      , pFun2 "GetCol" (pE ZZ) (pE $ MatOf typ) (ListMatCol typ)

      , pListPermsOf

      , pListRange
      , pListPivotColIndices typ
      , pListBuilder
      , pFun2 "Choose" (pE ZZ) (pTypedListExpr typ pE) (ListChoose typ)
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
          xs <- pTypedListExpr typ pE
          keyword ")"
          return (ListFilter typ k g xs)

        pListChoices = do
          _ <- try $ keyword "Choices"
          case typ of
            ListOf t -> do
              keyword "("
              n <- pE ZZ
              keyword ";"
              xs <- pTypedListExpr t pE
              keyword ")"
              return (ListChoices typ n xs)
            _ -> error "pListChoices"

        pListShuffles = do
          _ <- try $ keyword "Shuffles"
          case typ of
            ListOf t -> do
              keyword "("
              xs <- pTypedListExpr t pE
              keyword ")"
              return (ListShuffles typ xs)
            _ -> error "pListShuffles"

        pListPermsOf = do
          _ <- try $ keyword "PermutationsOf"
          case typ of
            PermOf t -> do
              keyword "("
              xs <- pTypedListExpr t pE
              keyword ")"
              return (ListPermsOf typ xs)
            _ -> error "pListPermsOf"

        pListRange = if typ == ZZ
          then do
            try $ keyword "Range"
            (a,b) <- pTuple2 (pE ZZ) (pE ZZ)
            return (ListRange ZZ a b)
          else fail "pListRange"

        pListPivotColIndices ZZ = do
          try $ keyword "PivotCols"
          keyword "("
          t <- pType
          keyword ";"
          m <- pE (MatOf t)
          keyword ")"
          return (ListPivotColIndices ZZ m)
        pListPivotColIndices _ = fail "pListPivotColIndices"
    
        pListBuilder = do
          start <- getPosition
          try $ keyword "Build"
          keyword "("
          expr <- pE typ
          keyword ";"
          gds <- sepBy1 (pListBind <|> pListGuard) (keyword ";")
          keyword ")"
          end <- getPosition
          return (ListBuilder typ expr gds)
            where
              pListBind = do
                w <- try pType
                whitespace
                k <- pKey
                keyword "<-"
                ls <- pTypedListExpr w pE
                return $ Bind k ls
          
              pListGuard = do
                e <- try (pE BB)
                return $ Guard e
    
    listOpTable =
      [ [ Infix (opParser2 (ListCat typ) "++") AssocLeft
        ]
      , [ Infix (opParser2 (ListToss typ) "\\\\") AssocLeft
        ]
      ]

