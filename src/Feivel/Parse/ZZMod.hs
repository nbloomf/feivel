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

module Feivel.Parse.ZZMod (
  pZZModConst, pZZModExpr
) where

import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM
import Feivel.Lib (zzmod)

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))


pZZModConst :: Integer -> ParseM ZZModExpr
pZZModConst n = fmap ZZModExpr $ pAtLocus (pZZModConst' n)

pZZModConst' :: Integer -> ParseM (OfType ZZModExprLeafS)
pZZModConst' n = do
  a <- pInteger
  return (ZZModConst (a `zzmod` n) :# (ZZMod n))

pZZModExpr :: (Type -> ParseM Expr) -> Integer -> ParseM BoolExpr -> ParseM IntExpr -> (Integer -> ParseM ZZModExpr) -> ParseM ZZModExpr
pZZModExpr pE n pBOOL pINT pMOD = spaced $ buildExpressionParser zzModOpTable pZZModTerm
  where
    pZZModTerm = pTerm (pZZModConst' n) ZZModExpr (pMOD n) "integer expression"
      [ pVarExpr ((:# (ZZMod n)) `o` ZZModVar) (ZZMod n)
      , pMacroExprT pE ((:# (ZZMod n)) `oo` ZZModMacro)

      , pFun2 "AtPos" (pE $ ListOf (ZZMod n)) pINT ((:# (ZZMod n)) `oo` ZZModAtPos)
      , pFun3 "AtIdx" (pE $ MatOf (ZZMod n)) pINT pINT ((:# (ZZMod n)) `ooo` ZZModAtIdx)
    
      , pIfThenElseExprT pBOOL (pMOD n) ((:# (ZZMod n)) `ooo` ZZModIfThenElse) (ZZMod n)

      , pFun1 "int" (pE ZZ) ((:# (ZZMod n)) `o` ZZModCast)

      , pFun2 "Pow" (pMOD n) pINT ((:# (ZZMod n)) `oo` ZZModPow)

      , pFun1 "Sum"    (pE $ ListOf (ZZMod n)) ((:# (ZZMod n)) `o` ZZModSum)
      , pFun1 "Prod"   (pE $ ListOf (ZZMod n)) ((:# (ZZMod n)) `o` ZZModProd)
      ]

    zzModOpTable =
      [ [ Infix (opParser2 ((:# (ZZMod n)) `oo` ZZModMult) ZZModExpr "*") AssocLeft
        ]
      , [ Prefix (opParser1 ((:# (ZZMod n)) `o` ZZModNeg) ZZModExpr "neg")
        , Prefix (opParser1 ((:# (ZZMod n)) `o` ZZModInv) ZZModExpr "inv")
        ]
      , [ Infix (opParser2 ((:# (ZZMod n)) `oo` ZZModAdd) ZZModExpr "+") AssocLeft
        , Infix (opParser2 ((:# (ZZMod n)) `oo` ZZModSub) ZZModExpr "-") AssocLeft
        ]
      ]
