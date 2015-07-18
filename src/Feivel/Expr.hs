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

{-# LANGUAGE FlexibleInstances #-}

module Feivel.Expr (
  ToExpr, toExpr,

  -- Expression Types
  Doc, DocLeaf(..),

  Expr(..),
  StrExpr, StrExprLeaf(..), Format(..),
  IntExpr, IntExprLeaf(..),
  BoolExpr, BoolExprLeaf(..),
  RatExpr, RatExprLeaf(..),
  ZZModExpr, ZZModExprLeaf(..),

  ListExpr, ListExprLeaf(..), ListGuard(..),
  MatExpr, MatExprLeaf(..),
  PolyExpr, PolyExprLeaf(..),
  PermExpr, PermExprLeaf(..),
  MacExpr, MacExprLeaf(..),

  -- Errors
  ExprErr(..)
) where


import Feivel.Key
import Feivel.Type
import Feivel.Locus
import Feivel.Lib

import Feivel.Expr.Doc
import Feivel.Expr.ZZMod
import Feivel.Expr.Perm
import Feivel.Expr.Mac
import Feivel.Expr.Poly
import Feivel.Expr.Rat
import Feivel.Expr.Mat
import Feivel.Expr.Str
import Feivel.Expr.Bool
import Feivel.Expr.List
import Feivel.Expr.Int

{------------------}
{- Contents       -}
{-   :Expr        -}
{-  :ExprErr      -}
{------------------}


{---------}
{- :Expr -}
{---------}

data Expr
  = DocE  (Doc      Expr)
  | BoolE (BoolExpr Expr)
  | StrE  (StrExpr  Expr)
  | IntE  (IntExpr  Expr)
  | RatE  (RatExpr  Expr)
 
  | ZZModE (ZZModExpr Expr)
 
  | ListE  (ListExpr Expr)
  | MatE   (MatExpr  Expr)
  | PolyE  (PolyExpr Expr)
  | PermE  (PermExpr Expr)
  | MacE   (MacExpr  Expr)
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

instance ToExpr Expr      where toExpr = id
instance ToExpr (Doc       Expr) where toExpr = DocE
instance ToExpr (BoolExpr  Expr) where toExpr = BoolE
instance ToExpr (StrExpr   Expr) where toExpr = StrE
instance ToExpr (IntExpr   Expr) where toExpr = IntE
instance ToExpr (RatExpr   Expr) where toExpr = RatE
instance ToExpr (ZZModExpr Expr) where toExpr = ZZModE
instance ToExpr (ListExpr  Expr) where toExpr = ListE
instance ToExpr (MatExpr   Expr) where toExpr = MatE
instance ToExpr (PolyExpr  Expr) where toExpr = PolyE
instance ToExpr (PermExpr  Expr) where toExpr = PermE
instance ToExpr (MacExpr   Expr) where toExpr = MacE

-- NB: Not a fan of "no locus" here
-- NB: Are these even needed? Can we use put instead?
instance ToExpr Integer  where toExpr k = IntE   $ IntConst k :@ NullLocus
instance ToExpr Rat      where toExpr r = RatE   $ RatConst r :@ NullLocus
instance ToExpr String   where toExpr s = StrE   $ StrConst (Text s) :@ NullLocus
instance ToExpr Text     where toExpr t = StrE   $ StrConst t :@ NullLocus
instance ToExpr Bool     where toExpr b = BoolE  $ BoolConst b :@ NullLocus
instance ToExpr ZZModulo where toExpr (ZZModulo a n) = ZZModE $ ZZModConst (ZZMod n) (ZZModulo a n) :@ NullLocus


{------------}
{- :ExprErr -}
{------------}

data ExprErr
 = UnevaluatedExpression
 | BailMessage String
 deriving (Eq, Show)
