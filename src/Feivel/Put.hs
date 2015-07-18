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

{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Feivel.Put (
  Put, put
) where

import Feivel.Expr
import Feivel.Locus
import Feivel.Type
import Feivel.Lib
import Feivel.Typed



{-----------}
{- :Inject -}
{-----------}

class Put a where
  put :: Locus -> a -> Expr



instance Put IntExpr where
  put loc (x :@ _) = IntE (x :@ loc)

instance Put (ZZModExpr Expr) where
  put loc (x :@ _) = ZZModE (x :@ loc)

instance Put (PermExpr Expr) where
  put loc (x :@ _) = PermE (x :@ loc)

instance Put ListExpr where
  put loc (x :@ _) = ListE (x :@ loc)

instance Put PolyExpr where
  put loc (x :@ _) = PolyE (x :@ loc)

instance Put MatExpr where
  put loc (x :@ _) = MatE (x :@ loc)

instance Put MacExpr where
  put loc (x :@ _) = MacE (x :@ loc)

instance Put RatExpr where
  put loc (x :@ _) = RatE (x :@ loc)

instance Put BoolExpr where
  put loc (x :@ _) = BoolE (x :@ loc)

instance Put StrExpr where
  put loc (x :@ _) = StrE (x :@ loc)



{--------------}
{- :Constants -}
{--------------}

instance Put Integer where
  put loc x = IntE $ IntConst x :@ loc

instance Put Text where
  put loc x = StrE $ StrConst x :@ loc

instance Put Bool where
  put loc x = BoolE $ BoolConst x :@ loc

instance Put Rat where
  put loc x = RatE $ RatConst x :@ loc

instance Put ZZModulo where
  put loc (ZZModulo a n) = ZZModE $ ZZModConst (ZZMod n) (ZZModulo a n) :@ loc



{-----------------}
{- :Constructors -}
{-----------------}

instance (Put a, Typed a) => Put [a] where
  put loc x = ListE $ ListConst typ (map (put loc) x) :@ loc
    where
      typ = case x of
              (a:_) -> typeOf a
              []    -> XX

instance (Put a, Typed a) => Put (Poly a) where
  put loc x = PolyE $ PolyConst typ (fmap (put loc) x) :@ loc
    where
      typ = case coefficientsP x of
              (c:_) -> typeOf c
              []    -> XX

instance (Put a, Typed a) => Put (Matrix a) where
  put loc x = MatE $ MatConst typ (fmap (put loc) x) :@ loc
    where
      typ = case toListM x of
              (a:_) -> typeOf a
              []    -> XX

instance (Put a) => Put (Perm a) where
  put loc x = PermE $ PermConst undefined (mapPerm (put loc) x) :@ loc
