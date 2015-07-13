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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Feivel.Glyph (
  Glyph, toGlyph
) where

import Feivel.EvalM
import Feivel.Expr
import Feivel.Locus
import Feivel.Lib
import Feivel.Error

import Data.List (intersperse)


{----------}
{- :Glyph -}
{----------}

class Glyph t where
  toGlyph :: t -> EvalM String



{--------------}
{- :Instances -}
{--------------}

instance Glyph Expr where
  toGlyph expr = case expr of
    IntE   x -> toGlyph x
    StrE   x -> toGlyph x
    BoolE  x -> toGlyph x
    RatE   x -> toGlyph x
    ListE  x -> toGlyph x
    MatE   x -> toGlyph x
    DocE   x -> toGlyph x
    PolyE  x -> toGlyph x
    PermE  x -> toGlyph x
    ZZModE x -> toGlyph x
    MacE   x -> toGlyph x


instance Glyph IntExpr where
  toGlyph (IntConst n :@ _) = return $ show n
  toGlyph x = error $ "toGlyph: IntExpr: " ++ show x


instance Glyph StrExpr where
  toGlyph (StrConst (Text s) :@ _) = return s
  toGlyph x = error $ "toGlyph: StrExpr: " ++ show x


instance Glyph BoolExpr where
  toGlyph (BoolConst True  :@ _) = return "#t"
  toGlyph (BoolConst False :@ _) = return "#f"
  toGlyph x = error $ "toGlyph: BoolExpr: " ++ show x


instance Glyph RatExpr where
  toGlyph (RatConst x :@ _) = return $ show x
  toGlyph x = error $ "toGlyph: RatExpr: " ++ show x


instance Glyph ZZModExpr where
  toGlyph (ZZModConst a :@ _) = return $ showZZMod a
  toGlyph x = error $ "toGlyph: ZZModExpr: " ++ show x


instance Glyph ListExpr where
  toGlyph (ListConst _ xs :@ _) = do
    ys <- sequence $ map toGlyph xs
    return $ "{" ++ concat (intersperse ";" ys) ++ "}"
  toGlyph x = error $ "toGlyph: ListExpr: " ++ show x


instance Glyph MatExpr where
  toGlyph (MatConst _ m :@ _) = do
    n <- mSeq $ fmap toGlyph m
    case mShowStr n of
      Left err -> reportErr (error "Glyph instance of MatExpr") err
      Right x  -> return x
  toGlyph x = error $ "toGlyph: MatExpr: " ++ show x


instance Glyph PolyExpr where
  toGlyph (PolyConst _ px :@ _) = do
    qx <- polySeq $ mapCoef toGlyph px
    return $ showStrP qx
  toGlyph x = error $ "toGlyph: PolyExpr: " ++ show x


instance Glyph PermExpr where
  toGlyph (PermConst _ px :@ _) = do
    qx <- seqPerm $ mapPerm toGlyph px
    return $ showPerm qx
  toGlyph x = error $ "toGlyph: PermExpr: " ++ show x


instance Glyph MacExpr where
  toGlyph (MacConst _ _ e _ :@ _) = toGlyph e
  toGlyph _ = error "toGlyph: MacExpr"


instance Glyph Doc where
  toGlyph (Empty     :@ _) = return ""
  toGlyph (DocText (Text s) :@ _) = return s
  toGlyph x = error $ "toGlyph: Doc: " ++ show x
