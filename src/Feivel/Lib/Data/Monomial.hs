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

module Feivel.Lib.Data.Monomial (
  Monomial(),

  makeMonomial, identity, variable, removeVar,

  isIdentity, monomialSupport, degree, degreeOf,

  multiply, monomialProduct, powers
) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Feivel.Lib.AlgErr
import Feivel.Lib.Data.Natural
import Feivel.Lib.Canon
import Feivel.Lib.Orders


newtype Monomial a = Monomial
  { unMonomial :: M.Map a Natural
  } deriving Ord

toPowers :: Monomial a -> [(a, Natural)]
toPowers = M.toList . unMonomial

-- Internal Constructor
fromPowers :: (Ord a) => [(a, Natural)] -> Monomial a
fromPowers = Monomial . M.fromList

instance (Ord a) => Canon (Monomial a) where
  canon = fromPowers . filter (\(_,n) -> not $ natIsZero n) . toPowers

instance (Ord a) => Eq (Monomial a) where
  m1 == m2 = (unMonomial $ canon m1) == (unMonomial $ canon m2)



{-----------------}
{- :Constructors -}
{-----------------}

makeMonomial :: (Ord a) => [(a, Natural)] -> Monomial a
makeMonomial = canon . fromPowers

variable :: (Ord a) => a -> Monomial a
variable x = fromPowers [(x, Nat 1)]

identity :: (Ord a) => Monomial a
identity = fromPowers []

removeVar :: (Ord a) => a -> Monomial a -> Monomial a
removeVar x = Monomial . M.delete x . unMonomial



{----------}
{- :Query -}
{----------}

isIdentity :: (Ord a) => Monomial a -> Bool
isIdentity = (identity ==)

monomialSupport :: (Ord a) => Monomial a -> [a]
monomialSupport = map fst . toPowers . canon

degree :: Monomial a -> Natural
degree = natSum . map snd . toPowers

degreeOf :: (Ord a) => a -> Monomial a -> Natural
degreeOf x = fromMaybe (Nat 0) . M.lookup x . unMonomial



{---------}
{- :Show -}
{---------}

showSepBy :: (Ord a) => String -> (a -> String) -> Monomial a -> String
showSepBy sep f m = bar $ concat $ map foo $ toPowers $ canon m
  where
    foo (x, Nat k) = sep ++ f x ++ if k==1 then "" else ("^" ++ show k)

    bar "" = ".1"
    bar s  = s

instance (Ord a, Show a) => Show (Monomial a) where
  show = showSepBy "." show



{---------------}
{- :Arithmetic -}
{---------------}

multiply :: (Ord a) => Monomial a -> Monomial a -> Monomial a
multiply m1 m2 = Monomial $ M.unionWith natAdd (unMonomial m1) (unMonomial m2)

monomialProduct :: (Ord a) => [Monomial a] -> Monomial a
monomialProduct = foldr multiply identity

powers :: (Ord a) => Monomial a -> [Monomial a]
powers m = iterate (multiply m) identity

monomialLCM :: (Ord a) => Monomial a -> Monomial a -> Monomial a
monomialLCM m1 m2 = Monomial $ M.unionWith natMax (unMonomial m1) (unMonomial m2)

monomialDivides :: (Ord a) => Monomial a -> Monomial a -> Bool
monomialDivides a' b' = b == (monomialLCM a b)
  where
    a = canon a'
    b = canon b'

--divide :: (Ord a) => Monomial a -> Monomial a -> Either AlgErr (Monomial a)
--divide m1 m2 = do
--  let s = M.toList $ M.unionWith natSub (unMonomial m1) (unMonomial m2)
--  undefined



{-------------------}
{- :MonomialOrders -}
{-------------------}

mLex, mRevLex, mGLex :: (Ord a) => Monomial a -> Monomial a -> Ordering
mLex m1 m2 = lexicographic2 (toPowers m1) (toPowers m2)

mRevLex m1 m2 = mLex m2 m1

mGLex m1 m2 = case compare (degree m1) (degree m2) of
  LT -> LT
  GT -> GT
  EQ -> mLex m1 m2