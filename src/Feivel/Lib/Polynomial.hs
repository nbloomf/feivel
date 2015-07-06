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

{-# OPTIONS_GHC -XTypeSynonymInstances #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module Feivel.Lib.Polynomial (
  Monomial, fromListM, Natural(..), Variable(..), identityM,

  Poly, fromListP, constP, polySeq, showP, mapCoef, showOUP, showByOUP, revlexM, varP,
  evalPolyAtPolyP, evalPolyAtScalarP, glexM, multidegM, showStrP, nullP,
  evalPolyAtPolysP,

  variablesP, isUnivariateP, isConstantP, fromRootsP, toScalarP,
  leadingTermByP, degreeByP, leadingTermByRevLexP, degreeByRevLexP, isRootP
) where

import qualified Data.Map as Map
import Data.List (intersperse, sortBy, maximumBy, union)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM, join)

import Feivel.Lib.Ring
import Feivel.Lib.Either ()
import Feivel.Lib.Pair
import Feivel.Lib.Canon
import Feivel.Lib.Monad
import Feivel.Lib.AlgErr ()



{-------------------}
{- :Contents       -}
{-   :Monomial     -}
{-   :Polynomial   -}
{-   :RingInstance -}
{-------------------}

newtype Natural = Nat { unNat :: Integer }
  deriving (Eq, Ord, Show)

natAdd :: Natural -> Natural -> Natural
natAdd (Nat a) (Nat b) = Nat (a+b)

newtype Variable = Var { unVar :: String }
  deriving (Eq, Ord, Show)



{-------------}
{- :Monomial -}
{-------------}

type Monomial = Map.Map Variable Natural

instance Canon Monomial where
  canon = Map.fromList . filter (\(_,k) -> k /= Nat 0) . toListM

showM :: String -> Monomial -> String
showM sep m = bar $ concat $ intersperse sep $ map foo $ toListM $ canon m
  where
    foo (Var x, Nat k)
      | k == 1 = x
      | otherwise = x ++ "^" ++ show k

    bar "" = "1"
    bar s  = s


{- Construct -}

toListM :: Monomial -> [(Variable, Natural)]
toListM = Map.toList

fromListM :: [(Variable, Natural)] -> Monomial
fromListM = productM . map (Map.fromList . (:[]))

identityM :: Monomial
identityM = Map.fromList []

varM :: Variable -> Monomial
varM x = Map.fromList [(x, Nat 1)]


{- Query -}

supportM :: Monomial -> [Variable]
supportM = map fst . toListM . canon

multidegM :: Monomial -> Integer
multidegM = sum . map (unNat . snd) . toListM . canon

isIdM :: Monomial -> Bool
isIdM = null . supportM

degOfM :: Variable -> Monomial -> Integer
degOfM x = unNat . fromMaybe (Nat 0) . Map.lookup x


{- Combine -}

multiplyM :: Monomial -> Monomial -> Monomial
multiplyM m1 m2 = Map.unionWith (natAdd) m1 m2

productM :: [Monomial] -> Monomial
productM = foldr multiplyM identityM


{- Mutate -}

removeVarM :: Variable -> Monomial -> Monomial
removeVarM x m = Map.delete x m


{- Monomial Orders -}

lex2 :: (Ord a, Ord b) => [(a,b)] -> [(a,b)] -> Ordering
lex2 [] [] = EQ
lex2 [] _  = LT
lex2 _  [] = GT
lex2 ((a1,b1):ps1) ((a2,b2):ps2)
  | a1 <  a2 = LT
  | a1 >  a2 = GT
  | a1 == a2 && b1 < b2 = LT
  | a1 == a2 && b1 > b2 = GT
  | otherwise = lex2 ps1 ps2

lexM :: Monomial -> Monomial -> Ordering
lexM m1 m2 = lex2 (toListM m1) (toListM m2)

revlexM :: Monomial -> Monomial -> Ordering
revlexM m1 m2 = lexM m2 m1

glexM :: Monomial -> Monomial -> Ordering
glexM m1 m2 = case compare (multidegM m1) (multidegM m2) of
  LT -> LT
  GT -> GT
  EQ -> lexM m1 m2



{---------------}
{- :Polynomial -}
{---------------}

type Poly a = Map.Map Monomial a

toListP :: Poly a -> [(a, Monomial)]
toListP = map swap . Map.toList

fromListP :: [(a, Monomial)] -> Poly a
fromListP = Map.fromList . map swap


{- Construct -}

nullP :: Poly a
nullP = fromListP []

constP :: a -> Poly a
constP c = fromListP [(c, identityM)]

oneP :: (URingoid a) => Poly a
oneP = fromListP [(rOne, identityM)]

varP :: (URingoid a) => Variable -> Poly a
varP x = fromListP [(rOne, varM x)]


{- Query -}

isZeroP :: Poly a -> Bool
isZeroP = null . toListP

variablesP :: Poly a -> [Variable]
variablesP = foldr union [] . map (supportM . snd) . toListP

isUnivariateP :: Poly a -> Bool
isUnivariateP p = case variablesP p of
  [_] -> True
  _   -> False

isConstantP :: Poly a -> Bool
isConstantP = null . variablesP

coefAtP :: Monomial -> Poly a -> Maybe a
coefAtP m p = Map.lookup m p


{- Monad-ish -}

mapVar :: (Monomial -> Monomial) -> Poly a -> Poly a
mapVar = Map.mapKeys

mapCoef :: (a -> b) -> Poly a -> Poly b
mapCoef = Map.map

polySeq :: (Functor m, Monad m) => Poly (m a) -> m (Poly a)
polySeq p = fmap Map.fromList $ sequence $ map seqSnd $ Map.toList p


-- List of terms using the supplied monomial order
termsByP :: (Monomial -> Monomial -> Ordering) -> Poly a -> [(a, Monomial)]
termsByP ord = (sortBy foo) . toListP
  where foo (_,m1) (_,m2) = ord m1 m2

leadingTermByP :: (Monomial -> Monomial -> Ordering) -> Poly a -> (a, Monomial)
leadingTermByP ord = (maximumBy foo) . toListP
  where foo (_,m1) (_,m2) = ord m1 m2

degreeByP :: (Monomial -> Monomial -> Ordering) -> Poly a -> Integer
degreeByP ord = multidegM . snd . leadingTermByP ord

leadingTermByRevLexP :: Poly a -> (a, Monomial)
leadingTermByRevLexP = leadingTermByP revlexM

degreeByRevLexP :: Poly a -> Integer
degreeByRevLexP = degreeByP revlexM


{- Arithmetic -}

mulMonomialP :: Monomial -> Poly a -> Poly a
mulMonomialP x = mapVar (multiplyM x)

mulCoefP :: (Ringoid a, CRingoid a) => a -> Poly a -> Either AlgErr (Poly a)
mulCoefP a = polySeq . mapCoef (rMul a)

mulTermP :: (Ringoid a, CRingoid a) => (a, Monomial) -> Poly a -> Either AlgErr (Poly a)
mulTermP (a,x) p = mulCoefP a $ mulMonomialP x p

addP :: (Ringoid a) => Poly a -> Poly a -> Either AlgErr (Poly a)
addP p q = polySeq $ Map.unionWith (opM2 rAdd) (mapCoef return p) (mapCoef return q)

sumP :: (Ringoid a) => [Poly a] -> Either AlgErr (Poly a)
sumP = foldM addP nullP

negP :: (Ringoid a) => Poly a -> Poly a
negP p = mapCoef rNeg p

multiplyP :: (Ringoid a, CRingoid a) => Poly a -> Poly a -> Either AlgErr (Poly a)
multiplyP p q = (sequence . map (`mulTermP` p) . toListP $ q) >>= sumP

productP :: (Ringoid a, CRingoid a, URingoid a) => [Poly a] -> Either AlgErr (Poly a)
productP = foldM multiplyP oneP

powerP :: (Ringoid a, CRingoid a, URingoid a) => Poly a -> Integer -> Either AlgErr (Poly a)
powerP p n = productP [p | _ <- [1..n]]

fromRootsP :: (Ringoid a, CRingoid a, URingoid a) => Variable -> [a] -> Either AlgErr (Poly a)
fromRootsP x cs = do
  let foo c = do
        u <- rLOneOf c
        return $ fromListP [(u, varM x), (rNeg c, identityM)]
  ps <- sequence $ map foo cs
  productP ps

toScalarP :: (Ringoid a) => Poly a -> Either AlgErr a
toScalarP q
  | isZeroP q = Right rZero
  | isConstantP q = case coefAtP identityM q of
      Just a -> Right a
      Nothing -> Right rZero
  | otherwise = Left (PolyNotConstant "")


{-----------}
{- Ringoid -}
{-----------}

instance (Ringoid a, CRingoid a) => Ringoid (Poly a) where
  rAdd p q = case addP p q of
    Left err -> Left (RingoidAddErr $ show err)
    Right x  -> return x

  rMul p q = case multiplyP p q of
    Left err -> Left (RingoidMulErr $ show err)
    Right x  -> return x

  rNeg = negP

  rIsZero = and . map (rIsZero . fst) . toListP

  rZero = nullP
  rNeutOf _ = return nullP
  rLAnnOf _ = return nullP
  rRAnnOf _ = return nullP


instance (Ringoid a, CRingoid a) => CRingoid (Poly a)


instance (Ringoid a, CRingoid a, URingoid a) => URingoid (Poly a) where
  rOne = oneP
  rIsOne = error "rIsOne: Polynomial"

  rIsUnit = error "rIsUnit: Polynomial"

  rInv = error "rInv: Polynomial"

  rLOneOf _ = return oneP
  rROneOf _ = return oneP

  rInjInt n = do
    k <- rInjInt n
    return $ constP k


-- Pretty-print a polynomial over any set
showByP :: (Monomial -> Monomial -> Ordering) -> (a -> String) -> Poly a -> String
showByP ord f = mung . concat . intersperse ";" . map foo . termsByP ord
  where
    foo (a,x) = f a ++ "." ++ showM "." x

    mung str = if str == "" then "Null" else "Poly(" ++ str ++ ")"

showP :: (Show a) => Poly a -> String
showP = showByP revlexM show

showStrP :: Poly String -> String
showStrP = showByP revlexM id


{- Evaluation -}

evalTermAtPolyP :: (Ringoid a, CRingoid a, URingoid a)
  => Poly a -> Variable -> (a, Monomial) -> Either AlgErr (Poly a)
evalTermAtPolyP p x (a,m) = do
  let k = x `degOfM` m
  q <- powerP p k
  let n = (a, removeVarM x m)
  mulTermP n q

evalPolyAtPolyP :: (Ringoid a, CRingoid a, URingoid a)
  => Poly a -> Variable -> Poly a -> Either AlgErr (Poly a)
evalPolyAtPolyP p x = join . fmap sumP . sequence . map (evalTermAtPolyP p x) . toListP

evalPolyAtPolysP :: (Ringoid a, CRingoid a, URingoid a)
  => [(Variable, Poly a)] -> Poly a -> Either AlgErr (Poly a)
evalPolyAtPolysP [] p = Right p
evalPolyAtPolysP ((x,q):xs) p = do
  s <- evalPolyAtPolyP q x p
  evalPolyAtPolysP xs s

evalTermAtScalarP :: (Ringoid a, CRingoid a, URingoid a)
  => a -> Variable -> (a, Monomial) -> Either AlgErr (a, Monomial)
evalTermAtScalarP c x (a,m) = do
  let k = x `degOfM` m
  b <- rPow c k
  d <- rMul a b
  return (d, removeVarM x m)

evalPolyAtScalarP :: (Ringoid a, CRingoid a, URingoid a)
  => a -> Variable -> Poly a -> Either AlgErr (Poly a)
evalPolyAtScalarP c x = join . fmap sumP . sequence . map (fmap (fromListP . (:[])) . evalTermAtScalarP c x) . toListP


isRootP :: (Ringoid a, CRingoid a, URingoid a) => a -> Variable -> Poly a -> Bool
isRootP a x p = case evalPolyAtScalarP a x p of
  Left _ -> False
  Right q -> rIsZero q


-- Pretty-print a polynomial over an ordered unital ring (e.g. ZZ)
showByOUP :: (Ringoid a, URingoid a, ORingoid a)
  => String -> (Monomial -> Monomial -> Ordering) -> (a -> String) -> Poly a -> String
showByOUP sep ord f = concat . foo . termsByP ord
  where
    foo [] = [""]
    foo (t:ts) = firstTerm t : map otherTerms ts

    firstTerm (a,m)
     | rIsZero a               = "0"

     | isIdM m && rIsOne a    = "1"
     | isIdM m && rIsPos a    = f (rAbs a)
     | isIdM m && rIsNegOne a = "-1"
     | isIdM m                = "-" ++ f (rAbs a)

     | rIsOne a    = showM sep m
     | rIsPos a    = f (rAbs a) ++ showM sep m
     | rIsNegOne a = "-" ++ showM sep m
     | otherwise   = "-" ++ f (rAbs a) ++ showM sep m

    otherTerms (a,m)
     | rIsZero a                = ""

     | isIdM m   && rIsOne a    = " + 1"
     | isIdM m   && rIsPos a    = " + " ++ f (rAbs a)
     | isIdM m   && rIsNegOne a = " - 1"
     | isIdM m                  = " - " ++ f (rAbs a)

     | rIsOne a    = " + " ++ showM sep m
     | rIsPos a    = " + " ++ f (rAbs a) ++ sep ++ showM sep m
     | rIsNegOne a = " - " ++ showM sep m
     | otherwise   = " - " ++ f (rAbs a) ++ sep ++ showM sep m

showOUP :: (Show a, Ringoid a, ORingoid a, URingoid a) => Poly a -> String
showOUP = showByOUP "." revlexM show
