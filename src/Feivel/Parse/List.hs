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

pListLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM (ListExprLeaf Expr BoolExpr IntExpr ListExpr)
pListLiteralOf typ pE = do
    xs <- pBraceList (pE typ)
    return (ListConst typ xs)

pListExpr :: (Type -> ParseM Expr) -> ParseM BoolExpr -> ParseM IntExpr -> (Type -> ParseM ListExpr) -> ParseM ListExpr
pListExpr pE pBOOL pINT pLIST = pTypedListExpr XX pE pBOOL pINT pLIST

pTypedListExpr :: Type -> (Type -> ParseM Expr) -> ParseM BoolExpr -> ParseM IntExpr -> (Type -> ParseM ListExpr) -> ParseM ListExpr
pTypedListExpr typ pE pBOOL pINT pLIST = spaced $ buildExpressionParser listOpTable pListTerm
  where
    pListTerm = pTerm (pListLiteralOf typ pE) ListExpr (pLIST typ) "list expression"
      [ pVarExpr (ListVar typ) (ListOf typ)

      , pMacroExprT pE (ListMacro typ)

      , pFun2 "AtPos" (pE $ ListOf (ListOf typ)) pINT (ListAtPos typ)
      , pFun3 "AtIdx" (pE $ MatOf (ListOf typ)) pINT pINT (ListAtIdx typ)

      , pIfThenElseExprT pBOOL (pLIST typ) (ListIfThenElse typ) (ListOf typ)

      , pFun1 "Rand" (pE $ ListOf (ListOf typ)) (ListRand typ)

      , pFun1 "Reverse"  (pLIST typ) (ListRev typ)
      , pFun1 "Sort"     (pLIST typ) (ListSort typ)
      , pFun1 "Unique"   (pLIST typ) (ListUniq typ)
      , pFun1 "Shuffle"  (pLIST typ) (ListShuffle typ)
      , pListShuffles

      , pFun2 "GetRow" pINT (pE $ MatOf typ) (ListMatRow typ)
      , pFun2 "GetCol" pINT (pE $ MatOf typ) (ListMatCol typ)

      , pListPermsOf

      , pListRange
      , pListPivotColIndices typ
      , pListBuilder
      , pFun2 "Choose" pINT (pLIST typ) (ListChoose typ)
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
          return (ListFilter typ k g xs)

        pListChoices = do
          _ <- try $ keyword "Choices"
          case typ of
            ListOf t -> do
              keyword "("
              n <- pINT
              keyword ";"
              xs <- pLIST t
              keyword ")"
              return (ListChoices typ n xs)
            _ -> error "pListChoices"

        pListShuffles = do
          _ <- try $ keyword "Shuffles"
          case typ of
            ListOf _ -> do
              keyword "("
              xs <- pLIST typ
              keyword ")"
              return (ListShuffles typ xs)
            _ -> error "pListShuffles"

        pListPermsOf = do
          _ <- try $ keyword "PermutationsOf"
          case typ of
            PermOf _ -> do
              keyword "("
              xs <- pLIST typ
              keyword ")"
              return (ListPermsOf typ xs)
            _ -> error "pListPermsOf"

        pListRange = if typ == ZZ
          then do
            try $ keyword "Range"
            (a,b) <- pTuple2 pINT pINT
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
          try $ keyword "Build"
          keyword "("
          expr <- pE typ
          keyword ";"
          gds <- sepBy1 (pListBind <|> pListGuard) (keyword ";")
          keyword ")"
          return (ListBuilder typ expr gds)
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
      [ [ Infix (opParser2 (ListCat typ) ListExpr "++") AssocLeft
        ]
      , [ Infix (opParser2 (ListToss typ) ListExpr "\\\\") AssocLeft
        ]
      ]
