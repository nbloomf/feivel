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

module Feivel.Parse.Expr (
  pDoc, pREPL,

  pTypedConst,
    pStrConst, pIntConst, pBoolConst, pRatConst, pZZModConst,
    pListLiteral, pMatLiteral, pPolyLiteral, pPermLiteral,

  pTypedExpr,
    pIntExpr, pRatExpr, pStrExpr, pBoolExpr, pZZModExpr,
    pTypedListExpr, pTypedMatExpr, pTypedPolyExpr, pTypedPermExpr
) where


import Feivel.Store
import Feivel.Grammar

import Feivel.Parse.Util
import Feivel.Parse.ParseM
import Feivel.Parse.Int
import Feivel.Parse.Str
import Feivel.Parse.Rat
import Feivel.Parse.Bool
import Feivel.Parse.ZZMod
import Feivel.Parse.Perm
import Feivel.Parse.Poly
import Feivel.Parse.Mat
import Feivel.Parse.List
import Feivel.Parse.Doc
import Feivel.Parse.Mac

import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)

{---------------}
{- Contents    -}
{-  :Expr      -}
{-  :REPL      -}
{---------------}

pINT :: ParseM IntExpr
pINT = pIntExpr pTypedExpr pINT

pSTR :: ParseM StrExpr
pSTR = pStrExpr pTypedExpr pSTR

pBOOL :: ParseM BoolExpr
pBOOL = pBoolExpr pTypedExpr pBOOL

pRAT :: ParseM RatExpr
pRAT = pRatExpr pTypedExpr pRAT

pMOD :: Integer -> ParseM ZZModExpr
pMOD n = pZZModExpr pTypedExpr n pMOD

pLIST :: Type -> ParseM ListExpr
pLIST typ = pTypedListExpr typ pTypedExpr pLIST

pMAT :: Type -> ParseM MatExpr
pMAT typ = pTypedMatExpr typ pTypedExpr pMAT

pPOLY :: Type -> ParseM PolyExpr
pPOLY typ = pTypedPolyExpr typ pTypedExpr pPOLY

pPERM :: Type -> ParseM PermExpr
pPERM typ = pTypedPermExpr typ pTypedExpr pPERM

pMAC :: Type -> ParseM MacExpr
pMAC typ = pTypedMacExpr typ pTypedExpr pBrackDocE pMAC

{---------}
{- :Expr -}
{---------}

pTypedExpr :: Type -> ParseM Expr
pTypedExpr DD = fmap DocE  (pDoc      pTypedExpr)
pTypedExpr SS = fmap StrE  pSTR
pTypedExpr ZZ = fmap IntE  pINT
pTypedExpr BB = fmap BoolE pBOOL
pTypedExpr QQ = fmap RatE  pRAT

pTypedExpr (ZZMod    n) = fmap ZZModE (pMOD  n)
pTypedExpr (ListOf   t) = fmap ListE  (pLIST t)
pTypedExpr (MatOf    t) = fmap MatE   (pMAT  t)
pTypedExpr (MacTo    t) = fmap MacE   (pMAC  t)
pTypedExpr (PermOf   t) = fmap PermE  (pPERM t)
pTypedExpr (PolyOver t) = fmap PolyE  (pPOLY t)

pTypedExpr XX = choice
  [ pTypedExpr ZZ
  , pTypedExpr QQ
  , pTypedExpr BB
  , pTypedExpr SS
  , fmap ListE (pListExpr pTypedExpr pLIST)
  , fmap MatE  (pMatExpr  pTypedExpr pMAT)
  , fmap PolyE (pPolyExpr pTypedExpr pPOLY)
  ]

pTypedConst :: Type -> ParseM Expr
pTypedConst SS           = fmap StrE   pStrConst
pTypedConst ZZ           = fmap IntE   pIntConst
pTypedConst BB           = fmap BoolE  pBoolConst
pTypedConst QQ           = fmap RatE   pRatConst
pTypedConst (ZZMod n)    = fmap ZZModE (pZZModConst n)

pTypedConst (ListOf   t) = fmap ListE  (pListConst t pTypedConst)
pTypedConst (MatOf    t) = fmap MatE   (pMatConst  t pTypedConst)
pTypedConst (PolyOver t) = fmap PolyE  (pPolyConst t pTypedConst)
pTypedConst (PermOf   t) = fmap PermE  (pPermConst t pTypedConst)
pTypedConst (MacTo    t) = fmap MacE   (pMacConst  t pTypedConst pBrackDocE)

pTypedConst _ = error "pTypedConst"



{---------}
{- :REPL -}
{---------}

pREPL :: ParseM (Doc Expr)
pREPL = do
  x <- choice $ map (fmap Doc . pAtLocus)
         [ pDefineREPL
         , pNakedExprREPL
         , pVarExpr NakedKey DD
         , pImportREPL
         ]
  eof
  return x
  where
    pDefineREPL = do
      try $ keyword "define"
      t <- pType
      whitespace
      k <- pKey
      keyword ":="
      v <- if t == DD then (pBrackDocE pTypedExpr) else pTypedExpr t
      return (Define t k v (Doc (Empty :@ NullLocus)))

    pNakedExprREPL = do
      x <- try (pTypedNakedExpr pTypedExpr)
      return (NakedExpr x)

    pImportREPL = do
      try $ keyword "import"
      path <- pParens pPath
      qual <- option Nothing (try (keyword "as") >> pToken >>= return . Just)
      return (Import path qual (Doc (Empty :@ NullLocus)))
