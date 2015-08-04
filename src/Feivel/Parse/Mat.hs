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

pMatLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM (OfType MatExprLeafS)
pMatLiteralOf typ p = do
  start <- getPosition
  xss <- pBrackList (pBrackList (p typ))
  end <- getPosition
  case mFromRowList xss of
    Left err -> reportParseErr (locus start end) err
    Right m -> return (MatConst m :# typ)

pMatExpr :: (Type -> ParseM Expr) -> ParseM BoolExpr -> ParseM IntExpr -> (Type -> ParseM ListExpr) -> (Type -> ParseM MatExpr) -> ParseM MatExpr
pMatExpr pE pBOOL pINT pLIST pMAT = pTypedMatExpr XX pE pBOOL pINT pLIST pMAT

pTypedMatExpr :: Type -> (Type -> ParseM Expr) -> ParseM BoolExpr -> ParseM IntExpr -> (Type -> ParseM ListExpr) -> (Type -> ParseM MatExpr) -> ParseM MatExpr
pTypedMatExpr typ pE pBOOL pINT pLIST pMAT = spaced $ buildExpressionParser matOpTable pMatTerm
  where
    pMatTerm = pTerm (pMatLiteralOf typ pE) MatExpr (pMAT typ) "matrix expression"
      [ pVarExpr ((:# typ) `o` MatVar) (MatOf typ)

      , pFun2 "AtPos" (pLIST (MatOf typ)) pINT ((:# typ) `oo` MatAtPos)
      , pFun3 "AtIdx" (pE $ MatOf (MatOf typ)) pINT pINT ((:# typ) `ooo` MatAtIdx)

      , pMacroExprT pE ((:# typ) `oo` MatMacro)

      , pIfThenElseExprT pBOOL (pMAT typ) ((:# typ) `ooo` MatIfThenElse) (MatOf typ)

      , pFun1 "Transpose" (pMAT typ) ((:# typ) `o` MatTrans)

      , pFun1 "ShuffleRows" (pMAT typ) ((:# typ) `o` MatShuffleRows)
      , pFun1 "ShuffleCols" (pMAT typ) ((:# typ) `o` MatShuffleCols)

      , pFun1 "RowFromList" (pLIST typ) ((:# typ) `o` MatRowFromList)
      , pFun1 "ColFromList" (pLIST typ) ((:# typ) `o` MatColFromList)

      , pFun1 "Rand" (pLIST (MatOf typ)) ((:# typ) `o` MatRand)

      , pFun2 "GetRow" pINT (pMAT typ) ((:# typ) `oo` MatGetRow)
      , pFun2 "GetCol" pINT (pMAT typ) ((:# typ) `oo` MatGetCol)

      , pMatBuilder

      , pFun1 "Id"    pINT ((:# typ) `o` MatId)
      , pFun3 "SwapE" pINT pINT pINT ((:# typ) `ooo` MatSwapE)
      , pFun3 "ScaleE" pINT pINT (pE typ) ((:# typ) `ooo` MatScaleE)
      , pMatAddE

      , pFun2 "Pow" (pMAT typ) pINT ((:# typ) `oo` MatPow)

      , pFun3 "SwapRows" (pMAT typ) pINT pINT ((:# typ) `ooo` MatSwapRows)
      , pFun3 "SwapCols" (pMAT typ) pINT pINT ((:# typ) `ooo` MatSwapCols)
      , pFun3 "ScaleRow" (pMAT typ) (pE typ) pINT ((:# typ) `ooo` MatScaleRow)
      , pFun3 "ScaleCol" (pMAT typ) (pE typ) pINT ((:# typ) `ooo` MatScaleCol)
      , pFun4 "AddRow"   (pMAT typ) (pE typ) pINT pINT ((:# typ) `oooo` MatAddRow)
      , pFun4 "AddCol"   (pMAT typ) (pE typ) pINT pINT ((:# typ) `oooo` MatAddCol)
      , pFun2 "DelRow"   (pMAT typ) pINT ((:# typ) `oo` MatDelRow)
      , pFun2 "DelCol"   (pMAT typ) pINT ((:# typ) `oo` MatDelCol)

      , pFun1 "GJForm"   (pMAT typ) ((:# typ) `o` MatGJForm)
      , pFun1 "GJFactor" (pMAT typ) ((:# typ) `o` MatGJFactor)
      ]
      where
        pMatAddE = do
          try $ keyword "AddE"
          keyword "("
          t <- pType
          keyword ";"
          n <- pINT
          keyword ";"
          i <- pINT
          keyword ";"
          j <- pINT
          keyword ";"
          e <- pE t
          keyword ")"
          return (MatAddE n i j e :# t)

        pMatBuilder = do
          try $ keyword "Build"
          keyword "("
          e <- pE typ
          keyword ";"
          tr <- pType
          whitespace
          kr <- pKey
          keyword "<-"
          lr <- pLIST tr
          keyword ";"
          tc <- pType
          whitespace
          kc <- pKey
          keyword "<-"
          lc <- pLIST tc
          keyword ")"
          return (MatBuilder e kr lr kc lc :# typ)
    
    matOpTable =
      [ [ Prefix (opParser1 ((:# typ) `o`  MatNeg)  MatExpr "neg")
        ]
      , [ Infix  (opParser2 ((:# typ) `oo` MatMul)  MatExpr "*")    AssocLeft
        ]
      , [ Infix  (opParser2 ((:# typ) `oo` MatAdd)  MatExpr "+")    AssocLeft
        ]
      , [ Infix  (opParser2 ((:# typ) `oo` MatHCat) MatExpr "hcat") AssocLeft
        , Infix  (opParser2 ((:# typ) `oo` MatVCat) MatExpr "vcat") AssocLeft
        ]
      ]
