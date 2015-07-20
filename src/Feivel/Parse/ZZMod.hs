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

import Feivel.Expr (Type(..), Expr(..), ZZModExpr, ZZModExprLeaf(..))
import Feivel.Parse.Util
import Feivel.Parse.ParseM
import Feivel.Lib (zzmod)

import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))


pZZModConst :: Integer -> ParseM (ZZModExpr Expr)
pZZModConst n = pAtLocus (pZZModConst' n)

pZZModConst' :: Integer -> ParseM (ZZModExprLeaf Expr)
pZZModConst' n = do
  a <- pInteger
  return (ZZModConst (ZZMod n) (a `zzmod` n))

pZZModExpr :: (Type -> ParseM Expr) -> Integer -> ParseM (ZZModExpr Expr)
pZZModExpr pE n = spaced $ buildExpressionParser zzModOpTable pZZModTerm
  where
    pZZModTerm = pTerm (pZZModConst' n) (pZZModExpr pE n) "integer expression"
      [ pVarExpr (ZZModVar (ZZMod n)) (ZZMod n)
      , pMacroExprT pE (ZZModMacro (ZZMod n))

      , pFun2 "AtPos" (pE $ ListOf (ZZMod n)) (pE ZZ) (ZZModAtPos (ZZMod n))
      , pFun3 "AtIdx" (pE $ MatOf (ZZMod n)) (pE ZZ) (pE ZZ) (ZZModAtIdx (ZZMod n))
    
      , pIfThenElseExprT pE (pZZModExpr pE n) (ZZModIfThenElse (ZZMod n)) (ZZMod n)

      , pFun1 "int" (pE ZZ) (ZZModCast (ZZMod n))

      , pFun2 "Pow" (pZZModExpr pE n) (pE ZZ) (ZZModPow (ZZMod n))

      , pFun1 "Sum"    (pE $ ListOf (ZZMod n)) (ZZModSum (ZZMod n))
      , pFun1 "Prod"   (pE $ ListOf (ZZMod n)) (ZZModProd (ZZMod n))
      ]

    zzModOpTable =
      [ [ Infix (opParser2 (ZZModMult (ZZMod n)) "*") AssocLeft
        ]
      , [ Prefix (opParser1 (ZZModNeg (ZZMod n)) "neg")
        , Prefix (opParser1 (ZZModInv (ZZMod n)) "inv")
        ]
      , [ Infix (opParser2 (ZZModAdd (ZZMod n)) "+") AssocLeft
        , Infix (opParser2 (ZZModSub (ZZMod n)) "-") AssocLeft
        ]
      ]
