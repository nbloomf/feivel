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

module Feivel.Parse.Mat (
  pMatConst, pMatExpr, pTypedMatExpr, pMatLiteral
) where

import Feivel.Store (locus)
import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM
import Feivel.Lib (mFromRowList)

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pMatLiteral :: Type -> (Type -> ParseM Expr) -> ParseM MatExpr
pMatLiteral typ pE = fmap MatExpr $ pAtLocus $ pMatLiteralOf typ pE

pMatConst :: Type -> (Type -> ParseM Expr) -> ParseM MatExpr
pMatConst typ pC = fmap MatExpr $ pAtLocus $ pMatLiteralOf typ pC

pMatLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM (MatExprLeaf Expr MatExpr)
pMatLiteralOf typ p = do
  start <- getPosition
  xss <- pBrackList (pBrackList (p typ))
  end <- getPosition
  case mFromRowList xss of
    Left err -> reportParseErr (locus start end) err
    Right m -> return (MatConst typ m)

pMatExpr :: (Type -> ParseM Expr) -> (Type -> ParseM MatExpr) -> ParseM MatExpr
pMatExpr pE pMAT = pTypedMatExpr XX pE pMAT

pTypedMatExpr :: Type -> (Type -> ParseM Expr) -> (Type -> ParseM MatExpr) -> ParseM MatExpr
pTypedMatExpr typ pE pMAT = spaced $ buildExpressionParser matOpTable pMatTerm
  where
    pMatTerm = pTerm' (pMatLiteralOf typ pE) MatExpr (pMAT typ) "matrix expression"
      [ pVarExpr (MatVar typ) (MatOf typ)

      , pFun2 "AtPos" (pE $ ListOf (MatOf typ)) (pE ZZ) (MatAtPos typ)
      , pFun3 "AtIdx" (pE $ MatOf (MatOf typ)) (pE ZZ) (pE ZZ) (MatAtIdx typ)

      , pMacroExprT pE (MatMacro typ)

      , pIfThenElseExprT pE (pMAT typ) (MatIfThenElse typ) (MatOf typ)

      , pFun1 "Transpose" (pMAT typ) (MatTrans typ)

      , pFun1 "ShuffleRows" (pMAT typ) (MatShuffleRows typ)
      , pFun1 "ShuffleCols" (pMAT typ) (MatShuffleCols typ)

      , pFun1 "RowFromList" (pE $ ListOf typ) (MatRowFromList typ)
      , pFun1 "ColFromList" (pE $ ListOf typ) (MatColFromList typ)

      , pFun1 "Rand" (pE $ ListOf (MatOf typ)) (MatRand typ)

      , pFun2 "GetRow" (pE ZZ) (pMAT typ) (MatGetRow typ)
      , pFun2 "GetCol" (pE ZZ) (pMAT typ) (MatGetCol typ)

      , pMatBuilder

      , pFun2 "Id"    pType (pE ZZ) MatId
      , pFun4 "SwapE" pType (pE ZZ) (pE ZZ) (pE ZZ) MatSwapE
      , pMatScaleE
      , pMatAddE

      , pFun2 "Pow" (pMAT typ) (pE ZZ) (MatPow typ)

      , pFun3 "SwapRows" (pMAT typ) (pE ZZ) (pE ZZ) (MatSwapRows typ)
      , pFun3 "SwapCols" (pMAT typ) (pE ZZ) (pE ZZ) (MatSwapCols typ)
      , pFun3 "ScaleRow" (pMAT typ) (pE typ) (pE ZZ) (MatScaleRow typ)
      , pFun3 "ScaleCol" (pMAT typ) (pE typ) (pE ZZ) (MatScaleCol typ)
      , pFun4 "AddRow"   (pMAT typ) (pE typ) (pE ZZ) (pE ZZ) (MatAddRow typ)
      , pFun4 "AddCol"   (pMAT typ) (pE typ) (pE ZZ) (pE ZZ) (MatAddCol typ)
      , pFun2 "DelRow"   (pMAT typ) (pE ZZ) (MatDelRow typ)
      , pFun2 "DelCol"   (pMAT typ) (pE ZZ) (MatDelCol typ)

      , pFun1 "GJForm"   (pMAT typ) (MatGJForm typ)
      , pFun1 "GJFactor" (pMAT typ) (MatGJFactor typ)
      ]
      where
        pMatScaleE = do
          try $ keyword "ScaleE"
          keyword "("
          t <- pType
          keyword ";"
          n <- pE ZZ
          keyword ";"
          k <- pE ZZ
          keyword ";"
          e <- pE t
          keyword ")"
          return (MatScaleE t n k e)

        pMatAddE = do
          try $ keyword "AddE"
          keyword "("
          t <- pType
          keyword ";"
          n <- pE ZZ
          keyword ";"
          i <- pE ZZ
          keyword ";"
          j <- pE ZZ
          keyword ";"
          e <- pE t
          keyword ")"
          return (MatAddE t n i j e)

        pMatBuilder = do
          try $ keyword "Build"
          keyword "("
          e <- pE typ
          keyword ";"
          tr <- pType
          whitespace
          kr <- pKey
          keyword "<-"
          lr <- pE $ ListOf tr
          keyword ";"
          tc <- pType
          whitespace
          kc <- pKey
          keyword "<-"
          lc <- pE $ ListOf tc
          keyword ")"
          return (MatBuilder typ e kr lr kc lc)
    
    matOpTable =
      [ [ Prefix (opParser1' (MatNeg typ) MatExpr "neg")
        ]
      , [ Infix (opParser2' (MatMul typ) MatExpr "*") AssocLeft
        ]
      , [ Infix (opParser2' (MatAdd typ) MatExpr "+") AssocLeft
        ]
      , [ Infix (opParser2' (MatHCat typ) MatExpr "hcat") AssocLeft
        , Infix (opParser2' (MatVCat typ) MatExpr "vcat") AssocLeft
        ]
      ]
