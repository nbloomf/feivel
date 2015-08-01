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
  Poly(), Variable(Var), getCoefficients, fromTerm, fromTerms,

  mapCoef, polySeq,

  coefficients, leadingDegreeBy,

  constPoly, zeroPoly, fromRoots, fromCoefficients, varPoly,

  sumPoly, contentPoly,

  evalPolyAtPolys,

  showStrP
{- constP, polySeq, showP, showOUP, showByOUP, varP,
  evalPolyAtScalarP, showStrP, nullP,
  evalPolyAtPolysP, canonP,

  variablesP, isUnivariateP, isConstantP, fromRootsP, toScalarP,
  leadingTermByP, degreeByP, leadingTermByRevLexP, degreeByRevLexP, isRootP,
  leadingCoefByRevLexP, leadingCoefByP,

  contentP, fromCoefsP, univariateLongDiv-}
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

{-------------------}
{- :Contents       -}
{-   :Monadish     -}
{-   :Constructors -}
{-   :Querying     -}
{-   :Arithmetic   -}
{-   :Evaluation   -}
{-   :Instances    -}
{-------------------}



newtype Variable = Var
  { unVar :: String
  } deriving (Eq, Ord)

instance Show Variable where
  show = unVar


newtype Poly a = Poly
  { unPoly :: M.Map (Monomial Variable) a
  } deriving (Eq, Show)

toTerms :: Poly a -> [(a, Monomial Variable)]
toTerms = map swap . M.toList . unPoly

getCoefficients :: Poly a -> [a]
getCoefficients = map fst . toTerms

coefficientOf :: (Ringoid a) => Monomial Variable -> Poly a -> a
coefficientOf m p = fromMaybe rZero $ M.lookup m (unPoly p)



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

fromTerm :: (a, Monomial Variable) -> Poly a
fromTerm (a, x) = Poly $ M.fromList [(canon x, a)]

fromTerms :: [(a, Monomial Variable)] -> Poly a
fromTerms = Poly . M.fromList . map (\(a,x) -> (canon x, a))

reducedFromTerm :: (Ringoid a) => (a, Monomial Variable) -> Poly a
reducedFromTerm (a, x) = if rIsZero a
  then zeroPoly
  else Poly $ M.fromList [(canon x, a)]

constPoly :: (Ringoid a) => a -> Poly a
constPoly c = fromTerm (c, identity)

varPoly :: (Ringoid a, URingoid a) => Variable -> Poly a
varPoly x = fromTerm (rOne, variable x)

onePoly :: (Ringoid a, URingoid a) => Poly a
onePoly = constPoly rOne

addPoly :: (Ringoid a) => Poly a -> Poly a -> Either AlgErr (Poly a)
addPoly p q = do
  let a = unPoly $ mapCoef return p
  let b = unPoly $ mapCoef return q
  polySeq $ Poly $ M.unionWith (opM2 rAdd) a b

sumPoly :: (Ringoid a) => [Poly a] -> Either AlgErr (Poly a)
sumPoly = foldM addPoly zeroPoly

reducedFromTerms :: (Ringoid a) => [(a, Monomial Variable)] -> Either AlgErr (Poly a)
reducedFromTerms = sumPoly . map fromTerm . filter (\(c,_) -> not $ rIsZero c)

fromCoefficients :: (Ringoid a) => Monomial Variable -> [a] -> Either AlgErr (Poly a)
fromCoefficients m cs = reducedFromTerms $ zip cs (powers m)



{-------------}
{- :Querying -}
{-------------}

coefficients :: (Ringoid a, Canon a) => Poly a -> Either AlgErr [a]
coefficients p = do
  q <- canonPoly p
  return $ map fst $ toTerms q

canonPoly :: (Ringoid a, Canon a) => Poly a -> Either AlgErr (Poly a)
canonPoly = reducedFromTerms . map (\(a,x) -> (canon a, canon x)) . toTerms

equalPoly :: (Eq a, Ringoid a, Canon a) => Poly a -> Poly a -> Bool
equalPoly p q = case check of
  Left _  -> False
  Right t -> t
  where
    check = do
      a <- canonPoly p
      b <- canonPoly q
      return $ (unPoly a) == (unPoly b)

isZeroPoly :: (Eq a, Ringoid a, Canon a) => Poly a -> Bool
isZeroPoly = equalPoly zeroPoly

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
termsBy :: (Monomial Variable -> Monomial Variable -> Ordering) -> Poly a
  -> Either AlgErr [(a, Monomial Variable)]
termsBy ord p = do
  let foo (_,m1) (_,m2) = ord m1 m2
  return $ sortBy foo $ toTerms p

leadingTermBy :: (Ringoid a, Canon a, Eq a) => (Monomial Variable -> Monomial Variable -> Ordering)
  -> Poly a -> Either AlgErr (a, Monomial Variable)
leadingTermBy ord p = do
  q <- canonPoly p
  let foo (_,m1) (_,m2) = ord m1 m2
  if isZeroPoly q
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

fromRoots :: (Ringoid a, CRingoid a, URingoid a) => Variable -> [a] -> Either AlgErr (Poly a)
fromRoots x cs = do
  let foo c = addPoly (varPoly x) (negPoly $ constPoly c)
  ps <- sequence $ map foo cs
  productPoly ps

univariateLongDiv :: (Ringoid a, CRingoid a, URingoid a, Canon a, Eq a)
  => Poly a -> Poly a -> Either AlgErr (Poly a, Poly a)
univariateLongDiv a' b' = do
  a <- canonPoly a'
  b <- canonPoly b'
  if isZeroPoly a
    then return (zeroPoly, zeroPoly)
    else do
      if isZeroPoly b
        then Left $ PolyDivByZero "univariateLongDiv 1"
        else do
          ta <- isUnivariate a
          tb <- isUnivariate b
          if not (ta && tb)
            then Left $ PolyDivErr "univariateLongDiv 2"
            else do
              u <- isConstant b
              if u
                then do
                  b0 <- toScalar b >>= rInv
                  q  <- rMul a (constPoly b0)
                  return (q, rZero)
                else do
                  v <- isConstant a
                  if v
                    then return (zeroPoly, a)
                    else do
                      [va] <- variables a
                      [vb] <- variables b
                      if va /= vb
                        then Left $ PolyDivErr "univariateLongDiv 3"
                        else do
                          n <- leadingDegreeBy mGLex a
                          m <- leadingDegreeBy mGLex b
                          if n < m
                            then return (zeroPoly, a)
                            else do
                              an <- leadingCoefBy mGLex a
                              bminv <- leadingCoefBy mGLex b >>= rInv
                              c <- rMul bminv an
                              t <- natSub n m
                              let h = reducedFromTerm (c, makeMonomial [(va, t)])
                              s <- rMul h b
                              abar <- rSub a s >>= canonPoly
                              (qbar, r) <- univariateLongDiv abar b
                              q <- rAdd qbar h >>= canonPoly
                              Right (q,r)



{---------------}
{- :Evaluation -}
{---------------}

toScalar :: (Ringoid a, Canon a, Eq a) => Poly a -> Either AlgErr a
toScalar q = do
  if isZeroPoly q
    then Right rZero
    else do
      t <- isConstant q
      if t
        then Right $ identity `coefficientOf` q
        else Left (PolyNotConstant "")


-- At a scalar

evalTermAtScalar :: (Ringoid a, CRingoid a, URingoid a)
  => a -> Variable -> (a, Monomial Variable) -> Either AlgErr (a, Monomial Variable)
evalTermAtScalar c x (a,m) = do
  let Nat k = x `degreeOf` m
  b <- rPow c k
  d <- rMul a b
  return (d, removeVar x m)

evalPolyAtScalar :: (Ringoid a, CRingoid a, URingoid a, Canon a)
  => a -> Variable -> Poly a -> Either AlgErr (Poly a)
evalPolyAtScalar c x p = do
  us <- sequence $ map (evalTermAtScalar c x) (toTerms p)
  reducedFromTerms us

isRootOf :: (Ringoid a, CRingoid a, URingoid a, Canon a, Eq a)
  => a -> Variable -> Poly a -> Bool
isRootOf a x p = case evalPolyAtScalar a x p of
  Left _  -> False
  Right q -> rIsZero q


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
  rIsOne = equalPoly onePoly

  rIsUnit = error "rIsUnit: Polynomial"

  rInv = error "rInv: Polynomial"

  rLOneOf _ = return onePoly
  rROneOf _ = return onePoly

  rInjInt n = do
    k <- rInjInt n
    return $ constPoly k


-- :EDoid
instance (Ringoid a, CRingoid a, URingoid a, Canon a, Eq a) => EDoid (Poly a) where
  rDivAlg = univariateLongDiv

  rNorm p = do
    t <- isUnivariate p
    if t
      then fmap unNat $ leadingDegreeBy mGLex p
      else Left PolyNotUnivariate


-- :BDoid
instance (Ringoid a, CRingoid a, URingoid a, Canon a, Eq a) => BDoid (Poly a) where
  rBezout = rEuclidBezout



{-------------}
{- :Printing -}
{-------------}

-- Pretty-print a polynomial over any set
showByP :: (Monomial Variable -> Monomial Variable -> Ordering)
  -> String -> (a -> String) -> Poly a -> Either AlgErr String
showByP ord sep f p = do
  ts <- termsBy ord p
  return $ mung $ concat $ intersperse ";" $ map foo ts
  where
    foo (a,x) = f a ++ showSepBy sep show x

    mung str = if str == "" then "Null" else "Poly(" ++ str ++ ")"

showP :: (Show a) => Poly a -> Either AlgErr String
showP = showByP mRevLex "." show

showStrP :: Poly String -> Either AlgErr String
showStrP = showByP mRevLex "." id


-- Pretty-print a polynomial over an ordered unital ring (e.g. ZZ)
showByOUP :: (Ringoid a, URingoid a, ORingoid a, Canon a)
  => String -> (Monomial Variable -> Monomial Variable -> Ordering) -> (a -> String)
    -> Poly a -> Either AlgErr String
showByOUP sep ord f p = do
  ts <- termsBy ord p
  return $ bar $ concat $ foo ts
    where
      bar [] = "0"
      bar s  = s

      foo [] = [""]
      foo (t:ts) = firstTerm t : map otherTerms ts

      firstTerm (a,m)
       | rIsZero a               = "0"

       | isIdentity m && rIsOne a    = "1"
       | isIdentity m && rIsPos a    = f (rAbs a)
       | isIdentity m && rIsNegOne a = "-1"
       | isIdentity m                = "-" ++ f (rAbs a)

       | rIsOne a    = showSepBy sep show m
       | rIsPos a    = f (rAbs a) ++ showSepBy sep show m
       | rIsNegOne a = "-" ++ showSepBy sep show m
       | otherwise   = "-" ++ f (rAbs a) ++ showSepBy sep show m

      otherTerms (a,m)
       | rIsZero a                = ""

       | isIdentity m && rIsOne a    = " + 1"
       | isIdentity m && rIsPos a    = " + " ++ f (rAbs a)
       | isIdentity m && rIsNegOne a = " - 1"
       | isIdentity m                = " - " ++ f (rAbs a)

       | rIsOne a    = " + " ++ showSepBy sep show m
       | rIsPos a    = " + " ++ f (rAbs a) ++ showSepBy sep show m
       | rIsNegOne a = " - " ++ showSepBy sep show m
       | otherwise   = " - " ++ f (rAbs a) ++ showSepBy sep show m

showOUP :: (Show a, Ringoid a, ORingoid a, URingoid a, Canon a)
  => Poly a -> Either AlgErr String
showOUP = showByOUP "." mRevLex show


instance (LaTeX a, Ringoid a, ORingoid a, URingoid a, Canon a) => LaTeX (Poly a) where
  latex p = case showByOUP "" mRevLex latex p of
    Left err  -> show err
    Right str -> str


instance (Ringoid a, Canon a) => Canon (Poly a) where
  canon = fromTerms . filter (\(a,_) -> not $ rIsZero a) . map (\(a,m) -> (canon a, m)) . toTerms

{-


-}