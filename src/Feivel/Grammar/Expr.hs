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
{-# LANGUAGE FlexibleInstances    #-}

module Feivel.Grammar.Expr (
  ToExpr, toExpr,

  -- Expression Types
  Expr(..),
  Doc,       DocLeaf(..),
  StrExpr(..),   StrExprLeaf(..), Format(..),
  IntExpr(..),   IntExprLeaf(..),
  BoolExpr(..),  BoolExprLeaf(..),
  RatExpr(..),   RatExprLeaf(..),
  ZZModExpr(..), ZZModExprLeaf(..),

  ListExpr(..),  ListExprLeaf(..), ListGuard(..),
  MatExpr(..),   MatExprLeaf(..),
  PolyExpr(..),  PolyExprLeaf(..),
  PermExpr(..),  PermExprLeaf(..),
  MacExpr,   MacExprLeaf(..)
) where

import Feivel.Lib (Rat(..), ZZModulo(..), Text(..))

import Feivel.Grammar.Util
import Feivel.Grammar.Doc
import Feivel.Grammar.ZZMod
import Feivel.Grammar.Perm
import Feivel.Grammar.Mac
import Feivel.Grammar.Poly
import Feivel.Grammar.Rat
import Feivel.Grammar.Mat
import Feivel.Grammar.Str
import Feivel.Grammar.Bool
import Feivel.Grammar.List
import Feivel.Grammar.Int

{- :IntExpr -}

newtype IntExpr = IntExpr
  { unIntExpr :: AtLocus (IntExprLeaf Expr IntExpr)
  } deriving (Eq, Show)

instance HasLocus IntExpr where
  locusOf = locusOf . unIntExpr

instance Typed IntExpr where typeOf _ = ZZ


{- :StrExpr -}

newtype StrExpr = StrExpr
  { unStrExpr :: AtLocus (StrExprLeaf Expr StrExpr)
  } deriving (Eq, Show)

instance HasLocus StrExpr where
  locusOf = locusOf . unStrExpr

instance Typed StrExpr where typeOf _ = SS


{- :BoolExpr -}

newtype BoolExpr = BoolExpr
  { unBoolExpr :: AtLocus (BoolExprLeaf Expr BoolExpr)
  } deriving (Eq, Show)

instance HasLocus BoolExpr where
  locusOf = locusOf . unBoolExpr

instance Typed BoolExpr where typeOf _ = BB


{- :RatExpr -}

newtype RatExpr = RatExpr
  { unRatExpr :: AtLocus (RatExprLeaf Expr RatExpr)
  } deriving (Eq, Show)

instance HasLocus RatExpr where
  locusOf = locusOf . unRatExpr

instance Typed RatExpr where typeOf _ = QQ


data Expr
  = DocE   (Doc       Expr)
  | BoolE  BoolExpr
  | StrE   StrExpr
  | IntE   IntExpr
  | RatE   RatExpr
  | ZZModE (ZZModExpr Expr)
  | ListE  (ListExpr  Expr)
  | MatE   (MatExpr   Expr)
  | PolyE  (PolyExpr  Expr)
  | PermE  (PermExpr  Expr)
  | MacE   (MacExpr   Expr)
  deriving (Eq, Show)

instance HasLocus Expr where
  locusOf (DocE   x) = locusOf x
  locusOf (StrE   x) = locusOf x
  locusOf (IntE   x) = locusOf x
  locusOf (BoolE  x) = locusOf x
  locusOf (RatE   x) = locusOf x
  locusOf (ZZModE x) = locusOf x
  locusOf (ListE  x) = locusOf x
  locusOf (MatE   x) = locusOf x
  locusOf (PolyE  x) = locusOf x
  locusOf (PermE  x) = locusOf x
  locusOf (MacE   x) = locusOf x

{- ToExpr -}

class ToExpr t where
  toExpr :: t -> Expr

instance ToExpr Expr where toExpr = id
instance ToExpr (Doc       Expr) where toExpr = DocE
instance ToExpr BoolExpr         where toExpr = BoolE
instance ToExpr StrExpr          where toExpr = StrE
instance ToExpr IntExpr          where toExpr = IntE
instance ToExpr RatExpr          where toExpr = RatE
instance ToExpr (ZZModExpr Expr) where toExpr = ZZModE
instance ToExpr (ListExpr  Expr) where toExpr = ListE
instance ToExpr (MatExpr   Expr) where toExpr = MatE
instance ToExpr (PolyExpr  Expr) where toExpr = PolyE
instance ToExpr (PermExpr  Expr) where toExpr = PermE
instance ToExpr (MacExpr   Expr) where toExpr = MacE

-- NB: Not a fan of "no locus" here
-- NB: Is this even needed? Can we use put instead?
instance ToExpr Text where toExpr t = StrE $ StrExpr $ StrConst t :@ NullLocus

instance Typed Expr where
  typeOf (StrE   x) = typeOf x
  typeOf (IntE   x) = typeOf x
  typeOf (RatE   x) = typeOf x
  typeOf (BoolE  x) = typeOf x
  typeOf (ListE  x) = typeOf x
  typeOf (MacE   x) = typeOf x
  typeOf (DocE   x) = typeOf x
  typeOf (MatE   x) = typeOf x
  typeOf (PolyE  x) = typeOf x
  typeOf (PermE  x) = typeOf x
  typeOf (ZZModE x) = typeOf x

instance HasLocus Text where
  locusOf _ = NullLocus
