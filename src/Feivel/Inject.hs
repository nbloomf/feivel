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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Feivel.Inject (
  Inject, inject
) where

import Feivel.Expr
import Feivel.Locus
import Feivel.Type
import Feivel.Lib



{-----------}
{- :Inject -}
{-----------}

class Inject a b where
  inject :: (HasLocus b) => Locus -> a -> b

instance (HasLocus a) => Inject a a where
  inject _ a = a


instance Inject Integer IntExpr where
  inject loc x = IntConst x :@ loc

instance Inject String StrExpr where
  inject loc x = StrConst (Text x) :@ loc

instance Inject Text StrExpr where
  inject loc x = StrConst x :@ loc

instance Inject Bool BoolExpr where
  inject loc x = BoolConst x :@ loc

instance Inject Rat RatExpr where
  inject loc x = RatConst x :@ loc

instance Inject ZZModulo ZZModExpr where
  inject loc (ZZModulo a n) = ZZModConst (ZZMod n) (ZZModulo a n) :@ loc



{-------------}
{- :ListExpr -}
{-------------}

instance Inject [Integer] ListExpr where
  inject loc xs = (ListConst ZZ $ map foo xs) :@ loc
    where foo x = toExpr (IntConst x :@ loc)

instance Inject [String] ListExpr where
  inject loc xs = (ListConst SS $ map foo xs) :@ loc
    where foo x = toExpr (StrConst (Text x) :@ loc)

instance Inject [Text] ListExpr where
  inject loc xs = (ListConst SS $ map foo xs) :@ loc
    where foo x = toExpr (StrConst x :@ loc)

instance Inject [Bool] ListExpr where
  inject loc xs = (ListConst BB $ map foo xs) :@ loc
    where foo x = toExpr (BoolConst x :@ loc)

instance Inject [Rat] ListExpr where
  inject loc xs = (ListConst QQ $ map foo xs) :@ loc
    where foo x = toExpr (RatConst x :@ loc)



{------------}
{- :MatExpr -}
{------------}

instance Inject (Matrix Integer) MatExpr where
  inject loc x = (MatConst ZZ $ fmap toExpr x) :@ loc

instance Inject (Matrix ZZModulo) MatExpr where
  inject loc x = (MatConst (ZZMod n) $ fmap toExpr x) :@ loc
    where
      as = toListM x
      n = case as of
        [] -> 0
        ((ZZModulo _ m):_) -> m
      

instance Inject (Matrix Rat) MatExpr where
  inject loc x = (MatConst QQ $ fmap toExpr x) :@ loc

instance Inject (Matrix Bool) MatExpr where
  inject loc x = (MatConst BB $ fmap toExpr x) :@ loc

instance Inject (Matrix (Poly Integer)) MatExpr where
  inject loc x = (MatConst (PolyOver ZZ) $ fmap toExpr foo) :@ loc
    where foo = fmap (inject loc) x :: Matrix PolyExpr

instance Inject (Matrix (Poly Rat)) MatExpr where
  inject loc x = (MatConst (PolyOver QQ) $ fmap toExpr foo) :@ loc
    where foo = fmap (inject loc) x :: Matrix PolyExpr

instance Inject (Matrix (Poly Bool)) MatExpr where
  inject loc x = (MatConst (PolyOver BB) $ fmap toExpr foo) :@ loc
    where foo = fmap (inject loc) x :: Matrix PolyExpr



{-------------}
{- :PolyExpr -}
{-------------}

instance Inject (Poly Integer) PolyExpr where
  inject loc x = (PolyConst ZZ $ fmap toExpr x) :@ loc

instance Inject (Poly Rat) PolyExpr where
  inject loc x = (PolyConst QQ $ fmap toExpr x) :@ loc

instance Inject (Poly Bool) PolyExpr where
  inject loc x = (PolyConst BB $ fmap toExpr x) :@ loc



{-------------}
{- :PermExpr -}
{-------------}

instance Inject (Perm Integer) PermExpr where
  inject loc x = (PermConst ZZ $ mapPerm toExpr x) :@ loc

instance Inject (Perm String) PermExpr where
  inject loc x = (PermConst SS $ mapPerm toExpr x) :@ loc

instance Inject (Perm Text) PermExpr where
  inject loc x = (PermConst SS $ mapPerm toExpr x) :@ loc

instance Inject (Perm Rat) PermExpr where
  inject loc x = (PermConst QQ $ mapPerm toExpr x) :@ loc



{--------}
{- :Doc -}
{--------}

instance Inject String Doc where
  inject loc = \x -> DocText (Text x) :@ loc