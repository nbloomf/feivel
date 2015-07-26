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

module Feivel.Grammar.Put (
  Put, put, putType
) where

import Feivel.Grammar.Expr
import Feivel.Grammar.Util
import Feivel.Lib



{-----------}
{- :Inject -}
{-----------}

class Put a where
  put     ::         Locus -> a -> Expr
  putType :: Type -> Locus -> a -> Expr

  -- Default: ignore the type
  putType _ = put


instance Put Expr where
  put _ = id


instance Put (IntExpr Expr) where
  put loc (IntExpr (x :@ _)) = IntE $ IntExpr (x :@ loc)

instance Put (ZZModExpr Expr) where
  put loc (x :@ _) = ZZModE (x :@ loc)

instance Put (PermExpr Expr) where
  put loc (x :@ _) = PermE (x :@ loc)

instance Put (ListExpr Expr) where
  put loc (x :@ _) = ListE (x :@ loc)

instance Put (PolyExpr Expr) where
  put loc (x :@ _) = PolyE (x :@ loc)

instance Put (MatExpr Expr) where
  put loc (x :@ _) = MatE (x :@ loc)

instance Put (MacExpr Expr) where
  put loc (x :@ _) = MacE (x :@ loc)

instance Put (RatExpr Expr) where
  put loc (RatExpr (x :@ _)) = RatE $ RatExpr (x :@ loc)

instance Put (BoolExpr Expr) where
  put loc (BoolExpr (x :@ _)) = BoolE $ BoolExpr (x :@ loc)

instance Put (StrExpr Expr) where
  put loc (StrExpr (x :@ _)) = StrE $ StrExpr (x :@ loc)



{--------------}
{- :Constants -}
{--------------}

instance Put Integer where
  put loc x = IntE $ IntExpr $ IntConst x :@ loc

instance Put Text where
  put loc x = StrE $ StrExpr $ StrConst x :@ loc

instance Put Bool where
  put loc x = BoolE $ BoolExpr $ BoolConst x :@ loc

instance Put Rat where
  put loc x = RatE $ RatExpr $ RatConst x :@ loc

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

  putType typ loc x = ListE $ ListConst typ (map (put loc) x) :@ loc


instance (Put a, Typed a) => Put (Poly a) where
  put loc x = PolyE $ PolyConst typ (fmap (put loc) x) :@ loc
    where
      typ = case coefficientsP x of
              (c:_) -> typeOf c
              []    -> XX

  putType typ loc x = PolyE $ PolyConst typ (fmap (put loc) x) :@ loc


instance (Put a, Typed a) => Put (Matrix a) where
  put loc x = MatE $ MatConst typ (fmap (put loc) x) :@ loc
    where
      typ = case toListM x of
              (a:_) -> typeOf a
              []    -> XX

  putType typ loc x = MatE $ MatConst typ (fmap (put loc) x) :@ loc


instance (Put a) => Put (Perm a) where
  put loc x = PermE $ PermConst undefined (mapPerm (put loc) x) :@ loc

  putType typ loc x = PermE $ PermConst typ (mapPerm (put loc) x) :@ loc
