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

module Feivel.Lib.Struct.Polynomial (
{-  Monomial, fromListM, Natural(..), Variable(..), identityM,

  Poly, fromListP, constP, polySeq, showP, mapCoef, showOUP, showByOUP, revlexM, varP,
  evalPolyAtPolyP, evalPolyAtScalarP, glexM, multidegM, showStrP, nullP,
  evalPolyAtPolysP, canonP,

  variablesP, isUnivariateP, isConstantP, fromRootsP, toScalarP,
  leadingTermByP, degreeByP, leadingTermByRevLexP, degreeByRevLexP, isRootP,
  leadingCoefByRevLexP, leadingCoefByP,

  coefficientsP, contentP, fromCoefsP, varX, univariateLongDiv-}
) where

import qualified Data.Map as M
import Data.List (intersperse, sortBy, maximumBy, union, nub)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM, join)

import Feivel.Lib.Algebra.Ring
import Feivel.Lib.Either ()
import Feivel.Lib.Pair
import Feivel.Lib.Canon
import Feivel.Lib.Monad
import Feivel.Lib.AlgErr ()
import Feivel.Lib.Write.LaTeX

import Feivel.Lib.Data.Natural
import Feivel.Lib.Data.Monomial



newtype Variable = Var
  { unVar :: String
  } deriving (Eq, Ord)

instance Show Variable where
  show = unVar


newtype Poly a = Poly
  { unPoly :: M.Map (Monomial Variable) a
  } deriving Show

toTerms :: Poly a -> [(a, Monomial Variable)]
toTerms = map swap . M.toList . unPoly



{-------------}
{- :Monadish -}
{-------------}

mapVar :: (Monomial Variable -> Monomial Variable) -> Poly a -> Poly a
mapVar f = Poly . M.mapKeys f . unPoly

mapCoef :: (a -> b) -> Poly a -> Poly b
mapCoef f = Poly . M.map f . unPoly

polySeq :: (Functor m, Monad m) => Poly (m a) -> m (Poly a)
polySeq = fmap (Poly . M.fromList) . sequence . map seqSnd . M.toList . unPoly



{-----------------}
{- :Constructors -}
{-----------------}

zeroPoly :: Poly a
zeroPoly = Poly $ M.fromList []

fromTerm :: (Ringoid a) => (a, Monomial Variable) -> Poly a
fromTerm (a, x) = if rIsZero a
  then zeroPoly
  else Poly $ M.fromList [(canon x, a)]

constPoly :: (Ringoid a) => a -> Poly a
constPoly c = fromTerm (c, identity)

varPoly :: (Ringoid a, URingoid a) => String -> Poly a
varPoly x = fromTerm (rOne, variable $ Var x)

onePoly :: (Ringoid a, URingoid a) => Poly a
onePoly = constPoly rOne

addPoly :: (Ringoid a) => Poly a -> Poly a -> Either AlgErr (Poly a)
addPoly p q = do
  let a = unPoly $ mapCoef return p
  let b = unPoly $ mapCoef return q
  polySeq $ Poly $ M.unionWith (opM2 rAdd) a b

sumPoly :: (Ringoid a) => [Poly a] -> Either AlgErr (Poly a)
sumPoly = foldM addPoly zeroPoly

fromTerms :: (Ringoid a) => [(a, Monomial Variable)] -> Either AlgErr (Poly a)
fromTerms = sumPoly . map fromTerm . filter (\(c,_) -> not $ rIsZero c)



{-------------}
{- :Querying -}
{-------------}

coefficients :: (Ringoid a, Canon a) => Poly a -> Either AlgErr [a]
coefficients p = do
  q <- canonPoly p
  return $ map fst $ toTerms q

canonPoly :: (Ringoid a, Canon a) => Poly a -> Either AlgErr (Poly a)
canonPoly = fromTerms . map (\(a,x) -> (canon a, canon x)) . toTerms

instance (Eq a, Ringoid a, Canon a) => Eq (Poly a) where
  p == q = case check of
    Left _  -> False
    Right t -> t
    where
      check = do
        a <- canonPoly p
        b <- canonPoly q
        return $ (unPoly a) == (unPoly b)

isZeroPoly :: (Eq a, Ringoid a, Canon a) => Poly a -> Bool
isZeroPoly = (zeroPoly ==)

monomials :: (Ringoid a, Canon a) => Poly a -> Either AlgErr [Monomial Variable]
monomials p = do
  q <- canonPoly p
  return $ map snd $ toTerms q

variables :: (Ringoid a, Canon a) => Poly a -> Either AlgErr [Variable]
variables p = do
  ms <- monomials p
  return $ nub $ concatMap monomialSupport ms

isUnivariate :: (Ringoid a, Canon a) => Poly a -> Either AlgErr Bool
isUnivariate p = do
  vars <- variables p
  case vars of
    []  -> return True
    [_] -> return True
    _   -> return False

isConstant :: (Ringoid a, Canon a) => Poly a -> Either AlgErr Bool
isConstant p = do
  vars <- variables p
  case vars of
    [] -> return True
    _  -> return False

contentPoly :: (Ringoid t, GCDoid t, Canon t) => Poly t -> Either AlgErr t
contentPoly p = canonPoly p >>= coefficients >>= rGCDs

-- List of terms using the supplied monomial order
termsBy :: (Ringoid a, Canon a) => (Monomial Variable -> Monomial Variable -> Ordering) -> Poly a
  -> Either AlgErr [(a, Monomial Variable)]
termsBy ord p = do
  q <- canonPoly p
  let foo (_,m1) (_,m2) = ord m1 m2
  return $ sortBy foo $ toTerms q

leadingTermBy :: (Ringoid a, Canon a, Eq a) => (Monomial Variable -> Monomial Variable -> Ordering)
  -> Poly a -> Either AlgErr (a, Monomial Variable)
leadingTermBy ord p = do
  let foo (_,m1) (_,m2) = ord m1 m2
  if isZeroPoly p
    then Left ZeroPolynomial
    else return $ maximumBy foo $ toTerms p

leadingCoefBy :: (Ringoid a, Canon a, Eq a) => (Monomial Variable -> Monomial Variable -> Ordering)
  -> Poly a -> Either AlgErr a
leadingCoefBy ord p = do
  (a,_) <- leadingTermBy ord p
  return a

leadingDegreeBy :: (Ringoid a, Canon a, Eq a) => (Monomial Variable -> Monomial Variable -> Ordering)
  -> Poly a -> Either AlgErr Natural
leadingDegreeBy ord p = do
  (_,m) <- leadingTermBy ord p
  return $ degree m



{---------------}
{- :Arithmetic -}
{---------------}

mulMonomial :: Monomial Variable -> Poly a -> Poly a
mulMonomial x = mapVar (multiply x)

mulCoef :: (Ringoid a, CRingoid a) => a -> Poly a -> Either AlgErr (Poly a)
mulCoef a = polySeq . mapCoef (rMul a)

mulTerm :: (Ringoid a, CRingoid a) => (a, Monomial Variable) -> Poly a -> Either AlgErr (Poly a)
mulTerm (a,x) p = mulCoef a $ mulMonomial x p

negPoly :: (Ringoid a) => Poly a -> Poly a
negPoly p = mapCoef rNeg p

multiplyPoly :: (Ringoid a, CRingoid a) => Poly a -> Poly a -> Either AlgErr (Poly a)
multiplyPoly p q = (sequence $ map (`mulTerm` p) $ toTerms q) >>= sumPoly

productPoly :: (Ringoid a, CRingoid a, URingoid a) => [Poly a] -> Either AlgErr (Poly a)
productPoly = foldM multiplyPoly onePoly

powerPoly :: (Ringoid a, CRingoid a, URingoid a) => Poly a -> Integer -> Either AlgErr (Poly a)
powerPoly p n = productPoly [p | _ <- [1..n]]



{---------------}
{- :Evaluation -}
{---------------}

-- At a polynomial

evalTermAtPoly :: (Ringoid a, CRingoid a, URingoid a)
  => Poly a -> Variable -> (a, Monomial Variable) -> Either AlgErr (Poly a)
evalTermAtPoly p x (a,m) = do
  let Nat k = x `degreeOf` m
  q <- powerPoly p k
  let n = (a, removeVar x m)
  mulTerm n q

evalPolyAtPoly :: (Ringoid a, CRingoid a, URingoid a)
  => Poly a -> Variable -> Poly a -> Either AlgErr (Poly a)
evalPolyAtPoly p x = join . fmap sumPoly . sequence . map (evalTermAtPoly p x) . toTerms

evalPolyAtPolys :: (Ringoid a, CRingoid a, URingoid a)
  => [(Variable, Poly a)] -> Poly a -> Either AlgErr (Poly a)
evalPolyAtPolys [] p = Right p
evalPolyAtPolys ((x,q):xs) p = do
  s <- evalPolyAtPoly q x p
  evalPolyAtPolys xs s



{--------------}
{- :Instances -}
{--------------}

-- :Ringoid
instance (Ringoid a, CRingoid a, Canon a, Eq a) => Ringoid (Poly a) where
  rAdd p q = case addPoly p q of
    Left err -> Left (RingoidAddErr $ show err)
    Right x  -> return x

  rMul p q = case multiplyPoly p q of
    Left err -> Left (RingoidMulErr $ show err)
    Right x  -> return x

  rNeg = negPoly

  rIsZero = isZeroPoly

  rZero = zeroPoly
  rNeutOf _ = return zeroPoly
  rLAnnOf _ = return zeroPoly
  rRAnnOf _ = return zeroPoly


-- :CRingoid
instance (Ringoid a, CRingoid a) => CRingoid (Poly a)


-- :URingoid
instance (Ringoid a, CRingoid a, URingoid a, Canon a, Eq a) => URingoid (Poly a) where
  rOne = onePoly
  rIsOne = (onePoly ==)

  rIsUnit = error "rIsUnit: Polynomial"

  rInv = error "rInv: Polynomial"

  rLOneOf _ = return onePoly
  rROneOf _ = return onePoly

  rInjInt n = do
    k <- rInjInt n
    return $ constPoly k

{-
{-------------------}
{- :Contents       -}
{-   :Monomial     -}
{-   :Polynomial   -}
{-   :RingInstance -}
{-------------------}


instance (LaTeX a, Ringoid a, ORingoid a, URingoid a) => LaTeX (Poly a) where
  latex = showByOUP "" revlexM latex


{---------------}
{- :Polynomial -}
{---------------}

{- Query -}

coefAtP :: Monomial -> Poly a -> Maybe a
coefAtP m p = Map.lookup m p




{- Arithmetic -}

fromCoefsP :: (Ringoid a) => Monomial -> [a] -> Poly a
fromCoefsP m cs = fromListP $ filter (\(a,_) -> not $ rIsZero a) $ zip cs (powersM m)



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





instance (Ringoid a, Canon a) => Canon (Poly a) where
  canon p = fromListP $ filter (\(a,_) -> not $ rIsZero a) $ map (\(a,m) -> (canon a, m)) $ toListP p







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
showByOUP sep ord f = bar . concat . foo . termsByP ord
  where
    bar [] = "0"
    bar s  = s

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


univariateLongDiv :: (Ringoid a, CRingoid a, URingoid a, Canon a) => Poly a -> Poly a -> Either AlgErr (Poly a, Poly a)
univariateLongDiv a b
  | rIsZero a = return (rZero, rZero)
  | rIsZero b = Left $ PolyDivByZero "univariateLongDiv 3"
  | not (isUnivariateP a) || not (isUnivariateP b) = Left $ PolyDivErr "univariateLongDiv 1"
  | (variablesP a) /= (variablesP b) = Left $ PolyDivErr "univariateLongDiv 2"
  | isConstantP b = do
      b0 <- leadingCoefByP glexM b
      binv <- rInv b0
      q <- rMul a (constP binv)
      return (q, rZero)
  | otherwise = do
      let [x] = variablesP a
      n <- degreeByP glexM a
      m <- degreeByP glexM b
      if n < m
        then return (rZero, a)
        else do
          an <- leadingCoefByP glexM a
          bm <- leadingCoefByP glexM b
          bminv <- rInv bm
          c <- rMul bminv an
          t <- natSub n m
          let h = fromListP [(c, fromListM [(x, t)])]
          s <- rMul h b
          abar <- rSub a s >>= canonP
          (qbar, r) <- univariateLongDiv abar b
          q <- rAdd qbar h
          Right (q,r)



instance (Ringoid a, CRingoid a, URingoid a, Canon a) => EDoid (Poly a) where
  rDivAlg a b = do
    p <- canonP a
    q <- canonP b
    univariateLongDiv p q

  rNorm p
    | not $ isUnivariateP p = Left PolyNotUnivariate
    | otherwise = fmap unNat $ degreeByP glexM p


instance (Ringoid a, CRingoid a, URingoid a, Canon a) => BDoid (Poly a) where
  rBezout = rEuclidBezout

-}