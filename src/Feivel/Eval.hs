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
{-# LANGUAGE FlexibleContexts      #-}

module Feivel.Eval (
  module Feivel.Eval.EvalM,
  module Feivel.Eval.Eval,

  evalToGlyph
) where


import Feivel.Eval.EvalM
import Feivel.Eval.Eval
import Feivel.Eval.Util
import Feivel.Eval.Expr

{-
dispatchMatrixRingType
  :: Locus -> Type -> (forall a. (Ringoid a, URingoid a, Inject (Matrix a) MatExpr) => a -> EvalM MatExpr) -> EvalM MatExpr
dispatchMatrixRingType loc u fun = case u of
  ZZ          -> fun zeroZZ
  QQ          -> fun zeroQQ
  BB          -> fun zeroBB
  ZZMod n     -> fun (zeroMod n)
  PolyOver ZZ -> fun (constP zeroZZ)
  PolyOver QQ -> fun (constP zeroQQ)
  PolyOver BB -> fun (constP zeroBB)
  _           -> reportErr loc $ NumericTypeExpected u
-}
