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

pZZModConst' :: Integer -> ParseM (ZZModExprLeaf Expr IntExpr ZZModExpr)
pZZModConst' n = do
  a <- pInteger
  return (ZZModConst (ZZMod n) (a `zzmod` n))

pZZModExpr :: (Type -> ParseM Expr) -> Integer -> ParseM IntExpr -> (Integer -> ParseM ZZModExpr) -> ParseM ZZModExpr
pZZModExpr pE n pINT pMOD = spaced $ buildExpressionParser zzModOpTable pZZModTerm
  where
    pZZModTerm = pTerm' (pZZModConst' n) ZZModExpr (pMOD n) "integer expression"
      [ pVarExpr (ZZModVar (ZZMod n)) (ZZMod n)
      , pMacroExprT pE (ZZModMacro (ZZMod n))

      , pFun2 "AtPos" (pE $ ListOf (ZZMod n)) pINT (ZZModAtPos (ZZMod n))
      , pFun3 "AtIdx" (pE $ MatOf (ZZMod n)) pINT pINT (ZZModAtIdx (ZZMod n))
    
      , pIfThenElseExprT pE (pMOD n) (ZZModIfThenElse (ZZMod n)) (ZZMod n)

      , pFun1 "int" (pE ZZ) (ZZModCast (ZZMod n))

      , pFun2 "Pow" (pMOD n) pINT (ZZModPow (ZZMod n))

      , pFun1 "Sum"    (pE $ ListOf (ZZMod n)) (ZZModSum (ZZMod n))
      , pFun1 "Prod"   (pE $ ListOf (ZZMod n)) (ZZModProd (ZZMod n))
      ]

    zzModOpTable =
      [ [ Infix (opParser2' (ZZModMult (ZZMod n)) ZZModExpr "*") AssocLeft
        ]
      , [ Prefix (opParser1' (ZZModNeg (ZZMod n)) ZZModExpr "neg")
        , Prefix (opParser1' (ZZModInv (ZZMod n)) ZZModExpr "inv")
        ]
      , [ Infix (opParser2' (ZZModAdd (ZZMod n)) ZZModExpr "+") AssocLeft
        , Infix (opParser2' (ZZModSub (ZZMod n)) ZZModExpr "-") AssocLeft
        ]
      ]
