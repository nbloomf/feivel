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

module Feivel.Lib.Algebra.Ring (
  AlgErr(..),

  Ringoid,
    rAdd, rMul, rNeg, rZero, rIsZero, rSub, rSum, rDot, rEQ, rProd, rNeutOf, rLAnnOf, rRAnnOf,

    rAddT, rMulT, rNegT, rSubT, rSumT, rDotT,

  RingoidDiv,
    rDivides,

    rDividesT,

  CRingoid,

  ORingoid,
    rLT, rGT, rLEQ, rGEQ,
    rMin, rMax, rMinim, rMaxim,
    rIsPos, rIsNeg, rAbs, rIsNegOne,

    rAbsT, rMinT, rMaxT, rMinimT, rMaximT,

  URingoid,
    rOne, rIsOne, rUProd, rInjInt, rChoose, rInv, rDiv, rPosPow, rPow, rMean, rIsUnit, rLOneOf, rROneOf,

    rDivT, rUProdT, rChooseT, rPowT, rMeanT, rIntMeanT, rMeanDevT, rIntMeanDevT, rInvT, rPosPowT,

  URingoidAssoc,
    rAssoc,

  GCDoid,
    rGCD, rLCM, rGCDs, rLCMs,

    rGCDT, rLCMT, rGCDsT, rLCMsT,

  BDoid,
    rBezout, rBezouts,

  UFDoid,
    rFactor, rRad, rIsSquare, rIsSqFree, rSqPart, rSqFreePart,

    rRadT, rIsSquareT, rIsSqFreeT, rSqPartT, rSqFreePartT,

  EDoid,
    rNorm, rDivAlg, rQuo, rRem,

    rNormT, rDivAlgT, rQuoT, rRemT,
    rEuclidAlg, rEuclidGCD, rEuclidGCDs, rEuclidBezout, rEuclidBezouts,
    rEuclidAlgT, rEuclidGCDT, rEuclidGCDsT, rEuclidBezoutT, rEuclidBezoutsT,

  Fieldoid,

  DagRingoid,
    rDag,

  BipRingoid,
    rBipIn, rBipOut
) where

import Control.Monad.Instances ()
import Data.Numbers.Primes (primes)

import Feivel.Lib.AlgErr

{------------------}
{- Contents       -}
{-   :Heirarchy   -}
{-     :Ringoid   -}
{-     :ORingoid  -}
{-     :CRingoid  -}
{-     :URingoid  -}
{-     :Domainoid -}
{-     :GCDoid    -}
{-     :BDoid     -}
{-     :UFDoid    -}
{-     :EDoid     -}
{-     :Fieldoid  -}
{-   :Instances   -}
{-     :Integer   -}
{-     :Bool      -}
{------------------}

{------------}
{- :Ringoid -}
{------------}

class Ringoid t where
  rAdd :: t -> t -> Either AlgErr t
  rMul :: t -> t -> Either AlgErr t
  rNeg :: t -> t

  rZero   :: t
  rIsZero :: t -> Bool

  rNeutOf :: t -> Either AlgErr t
  rLAnnOf :: t -> Either AlgErr t
  rRAnnOf :: t -> Either AlgErr t

class RingoidDiv t where
  rDivides :: t -> t -> Either AlgErr Bool


rEQ :: (Ringoid t) => t -> t -> Either AlgErr Bool
rEQ a b = do
  z <- rSub a b
  return $ rIsZero z

rSub :: (Ringoid t) => t -> t -> Either AlgErr t
rSub a b = rAdd a (rNeg b)

rSum :: (Ringoid t) => [t] -> Either AlgErr t
rSum []     = Right rZero
rSum [t]    = Right t
rSum (t:ts) = do
  s <- rSum ts
  rAdd t s

rProd :: (Ringoid t) => [t] -> Either AlgErr t
rProd []     = Left (RingoidEmptyListErr "product")
rProd [t]    = return t
rProd (t:ts) = do
  s <- rProd ts
  rMul t s

rDot :: (Ringoid t) => [t] -> [t] -> Either AlgErr t
rDot xs ys = do
  zs <- sequence $ zipWith rMul xs ys
  rSum zs

rPosPow :: (Ringoid t) => t -> Integer -> Either AlgErr t
rPosPow t k
  | k < 0     = Left (RingoidNegativeExponentErr k)
  | k == 0    = Left RingoidZeroExponentErr
  | otherwise = rProd [t | _ <- [1..k]]
      


{-  -}

rNegT :: (Ringoid t) => t -> t -> Either AlgErr t
rNegT _ = return . rNeg

rAddT, rMulT, rSubT :: (Ringoid t) => t -> t -> t -> Either AlgErr t
rAddT _ = rAdd
rMulT _ = rMul
rSubT _ = rSub

rSumT :: (Ringoid t) => t -> [t] -> Either AlgErr t
rSumT _ = rSum

rDividesT :: (RingoidDiv t) => t -> t -> t -> Either AlgErr Bool
rDividesT _ = rDivides

rDotT :: (Ringoid t) => t -> [t] -> [t] -> Either AlgErr t
rDotT _ = rDot

rPosPowT :: (Ringoid t) => t -> t -> Integer -> Either AlgErr t
rPosPowT _ = rPosPow



{-------------}
{- :ORingoid -}
{-------------}

class ORingoid t where
  rLT :: t -> t -> Either AlgErr Bool

  rIsPos :: t -> Bool
  rIsNeg :: t -> Bool


rAbs :: (Ringoid t, ORingoid t) => t -> t
rAbs t = if rIsNeg t then (rNeg t) else t

rGT :: (ORingoid t) => t -> t -> Either AlgErr Bool
rGT x y = rLT y x

rLEQ :: (Ringoid t, ORingoid t) => t -> t -> Either AlgErr Bool
rLEQ x y = do
  p <- rLT x y
  q <- rEQ x y
  return (p || q)

rGEQ :: (Ringoid t, ORingoid t) => t -> t -> Either AlgErr Bool
rGEQ x y = do
  p <- rGT x y
  q <- rEQ x y
  return (p || q)

rMax :: (ORingoid t) => t -> t -> Either AlgErr t
rMax a b = do
  p <- rLT a b
  if p then return b else return a

rMin :: (ORingoid t) => t -> t -> Either AlgErr t
rMin a b = do
  p <- rLT a b
  if p then return a else return b

rMaxim :: (ORingoid t) => [t] -> Either AlgErr t
rMaxim []     = Left (RingoidEmptyListErr "maximum")
rMaxim [t]    = return t
rMaxim (t:ts) = do
  s <- rMaxim ts
  rMax t s

rMinim :: (ORingoid t) => [t] -> Either AlgErr t
rMinim []     = Left (RingoidEmptyListErr "minimum")
rMinim [t]    = return t
rMinim (t:ts) = do
  s <- rMinim ts
  rMin t s


{-  -}

rAbsT :: (ORingoid t, Ringoid t) => t -> t -> Either AlgErr t
rAbsT _ = return . rAbs

rMinT, rMaxT :: (ORingoid t) => t -> t -> t -> Either AlgErr t
rMinT _ = rMin
rMaxT _ = rMax

rMaximT, rMinimT :: (ORingoid t) => t -> [t] -> Either AlgErr t
rMaximT _ = rMaxim
rMinimT _ = rMinim



{-------------}
{- :CRingoid -}
{-------------}

class CRingoid t



{-------------}
{- :URingoid -}
{-------------}

class URingoid t where
  rOne   :: t
  rIsOne :: t -> Bool

  rIsUnit :: t -> Either AlgErr Bool

  rInjInt :: Integer -> Either AlgErr t

  rInv :: t -> Either AlgErr t

  rLOneOf :: t -> Either AlgErr t
  rROneOf :: t -> Either AlgErr t

class URingoidAssoc t where
  rAssoc :: t -> t -> Either AlgErr Bool


rIsNegOne :: (Ringoid t, URingoid t) => t -> Bool
rIsNegOne a = rIsOne (rNeg a)

rDiv :: (Ringoid t, URingoid t, CRingoid t) => t -> t -> Either AlgErr t
rDiv a b = do
  c <- rInv b
  rMul a c

rUProd :: (Ringoid t, URingoid t) => [t] -> Either AlgErr t
rUProd []     = return rOne
rUProd [t]    = return t
rUProd (t:ts) = do
  s <- rUProd ts
  rMul t s

rChoose :: (URingoid t) => Integer -> Integer -> Either AlgErr t
rChoose n k
  | n <= 0 || k < 0 || n < k = rInjInt 0
  | otherwise = rInjInt $ (product [(k+1)..n]) `div` (product [1..n-k])

rPow :: (Ringoid t, URingoid t) => t -> Integer -> Either AlgErr t
rPow t n
  | n > 0  = rPosPow t n
  | n == 0 = return rOne
  | otherwise = do
      s <- rInv t
      rPosPow s (-n)

rMean :: (Ringoid t, URingoid t, CRingoid t) => [t] -> Either AlgErr t
rMean [] = undefined
rMean ts = do
  let n = sum $ map (const 1) ts
  m <- rInjInt n
  s <- rSum ts
  rDiv s m

rMeanDev :: (Ringoid t, URingoid t, CRingoid t, ORingoid t) => [t] -> Either AlgErr t
rMeanDev [] = undefined
rMeanDev ts = do
  mu <- rMean ts
  as <- sequence $ map (`rSub` mu) ts
  rMean $ map rAbs as

rIntMean :: (Ringoid t, URingoid t, CRingoid t) => [Integer] -> Either AlgErr t
rIntMean [] = Left (RingoidEmptyListErr "integer mean")
rIntMean ks = do
  ts <- sequence $ map rInjInt ks
  rMean ts

rIntMeanDev :: (Ringoid t, URingoid t, CRingoid t, ORingoid t) => [Integer] -> Either AlgErr t
rIntMeanDev [] = Left (RingoidEmptyListErr "integer mean deviation")
rIntMeanDev ks = do
  ts <- sequence $ map rInjInt ks
  rMeanDev ts


rInvT :: (Ringoid t, URingoid t, CRingoid t) => t -> t -> Either AlgErr t
rInvT _ = rInv

rDivT :: (Ringoid t, URingoid t, CRingoid t) => t -> t -> t -> Either AlgErr t
rDivT _ = rDiv

rUProdT :: (Ringoid t, URingoid t) => t -> [t] -> Either AlgErr t
rUProdT _ = rUProd

rChooseT :: (URingoid t) => t -> Integer -> Integer -> Either AlgErr t
rChooseT _ = rChoose

rPowT :: (Ringoid t, URingoid t) => t -> t -> Integer -> Either AlgErr t
rPowT _ = rPow

rMeanT :: (Ringoid t, URingoid t, CRingoid t) => t -> [t] -> Either AlgErr t
rMeanT _ = rMean

rMeanDevT :: (Ringoid t, URingoid t, CRingoid t, ORingoid t)
  => t -> [t] -> Either AlgErr t
rMeanDevT _ = rMeanDev

rIntMeanT :: (Ringoid t, URingoid t, CRingoid t) => t -> [Integer] -> Either AlgErr t
rIntMeanT _ = rIntMean

rIntMeanDevT :: (Ringoid t, URingoid t, CRingoid t, ORingoid t)
  => t -> [Integer] -> Either AlgErr t
rIntMeanDevT _ = rIntMeanDev



{--------------}
{- :Domainoid -}
{--------------}

class Domainoid t



{-----------}
{- :GCDoid -}
{-----------}

class GCDoid t where
  rGCD :: t -> t -> Either AlgErr t
  rLCM :: t -> t -> Either AlgErr t

rGCDs :: (Ringoid t, GCDoid t) => [t] -> Either AlgErr t
rGCDs []     = return rZero
rGCDs [t]    = return t
rGCDs (t:ts) = rGCDs ts >>= rGCD t

rLCMs :: (URingoid t, GCDoid t) => [t] -> Either AlgErr t
rLCMs []     = return rOne
rLCMs [t]    = return t
rLCMs (t:ts) = rLCMs ts >>= rLCM t


{-  -}

rGCDT :: (GCDoid t) => t -> t -> t -> Either AlgErr t
rGCDT _ = rGCD

rLCMT :: (GCDoid t) => t -> t -> t -> Either AlgErr t
rLCMT _ = rLCM

rGCDsT :: (Ringoid t, GCDoid t) => t -> [t] -> Either AlgErr t
rGCDsT _ = rGCDs

rLCMsT :: (URingoid t, GCDoid t) => t -> [t] -> Either AlgErr t
rLCMsT _ = rLCMs



{----------}
{- :BDoid -}
{----------}

class BDoid t where
  -- rBezout a b = Right (h,k) <==> ah+bk = gcd(a,b)
  rBezout :: t -> t -> Either AlgErr (t,t)


rBezouts :: (Ringoid t, URingoid t, GCDoid t, BDoid t) => [t] -> Either AlgErr [t]
rBezouts [] = Right []
rBezouts [_] = return [rOne]
rBezouts [a,b] = do
  (h,k) <- rBezout a b
  return [h,k]
rBezouts (a:as) = do
  d <- rGCDs as
  (b1,b2) <- rBezout a d
  ts <- rBezouts as
  us <- sequence $ map (rMul b2) ts
  return (b1:us)



{-----------}
{- :UFDoid -}
{-----------}

class UFDoid t where
  rFactor :: t -> Either AlgErr (t, [(t, Integer)])

  rIsSquare   :: t -> Either AlgErr Bool
  rIsSqFree   :: t -> Either AlgErr Bool
  rSqPart     :: t -> Either AlgErr t
  rSqFreePart :: t -> Either AlgErr t


rPrimeFactors :: (UFDoid t) => t -> Either AlgErr [t]
rPrimeFactors t = do
  (_,xs) <- rFactor t
  return $ map fst xs

rRad :: (Ringoid t, URingoid t, UFDoid t) => t -> Either AlgErr t
rRad t = do
  ps <- rPrimeFactors t
  rUProd ps

rIsSquareT, rIsSqFreeT :: (UFDoid t) => t -> t -> Either AlgErr Bool
rIsSquareT _ = rIsSquare
rIsSqFreeT _ = rIsSqFree

rSqPartT, rSqFreePartT :: (UFDoid t) => t -> t -> Either AlgErr t
rSqPartT _ = rSqPart
rSqFreePartT _ = rSqFreePart

rRadT :: (Ringoid t, URingoid t, UFDoid t) => t -> t -> Either AlgErr t
rRadT _ = rRad



{----------}
{- :EDoid -}
{----------}

class EDoid t where
  rNorm :: t -> Either AlgErr Integer
  rDivAlg :: t -> t -> Either AlgErr (t,t)


rQuo :: (EDoid t) => t -> t -> Either AlgErr t
rQuo a b = do
  (q,_) <- rDivAlg a b
  return q

rRem :: (EDoid t) => t -> t -> Either AlgErr t
rRem a b = do
  (_,r) <- rDivAlg a b
  return r

rEuclidAlg :: (Ringoid t, EDoid t) => t -> t -> Either AlgErr [(t,t)]
rEuclidAlg a b = do
  (q,r) <- rDivAlg a b
  if rIsZero r
    then return [(q,r)]
    else do
      ts <- rEuclidAlg b r
      return $ (q,r):ts

rEuclidGCD :: (Ringoid t, EDoid t) => t -> t -> Either AlgErr t
rEuclidGCD a b = do
  ts <- rEuclidAlg a b
  case tail $ reverse ts of
    [] -> return b
    (_,r):_ -> return r

rEuclidGCDs :: (Ringoid t, EDoid t) => [t] -> Either AlgErr t
rEuclidGCDs []     = return rZero
rEuclidGCDs [t]    = return t
rEuclidGCDs (t:ts) = rEuclidGCDs ts >>= rEuclidGCD t

rEuclidBezout :: (Ringoid t, URingoid t, EDoid t) => t -> t -> Either AlgErr (t,t)
rEuclidBezout a b = do
  if rIsZero b
    then do
      return (rOne, rZero)
    else do
      (q,r) <- rDivAlg a b
      (h,k) <- rEuclidBezout b r
      u <- rMul q k
      v <- rSub h u
      return (k,v)

rEuclidBezouts :: (Ringoid t, URingoid t, EDoid t) => [t] -> Either AlgErr [t]
rEuclidBezouts [] = Right []
rEuclidBezouts [_] = return [rOne]
rEuclidBezouts [a,b] = do
  (h,k) <- rEuclidBezout a b
  return [h,k]
rEuclidBezouts (a:as) = do
  d <- rEuclidGCDs as
  (b1,b2) <- rEuclidBezout a d
  ts <- rEuclidBezouts as
  us <- sequence $ map (rMul b2) ts
  return (b1:us)


{-  -}

rNormT :: (EDoid t) => t -> t -> Either AlgErr Integer
rNormT _ = rNorm

rDivAlgT :: (EDoid t) => t -> t -> t -> Either AlgErr (t,t)
rDivAlgT _ = rDivAlg

rQuoT, rRemT :: (EDoid t) => t -> t -> t -> Either AlgErr t
rQuoT _ = rQuo
rRemT _ = rRem

rEuclidAlgT :: (Ringoid t, EDoid t) => t -> t -> t -> Either AlgErr [(t,t)]
rEuclidAlgT _ = rEuclidAlg

rEuclidGCDT :: (Ringoid t, EDoid t) => t -> t -> t -> Either AlgErr t
rEuclidGCDT _ = rEuclidGCD

rEuclidGCDsT :: (Ringoid t, EDoid t) => t -> [t] -> Either AlgErr t
rEuclidGCDsT _ = rEuclidGCDs

rEuclidBezoutT :: (Ringoid t, URingoid t, EDoid t) => t -> t -> t -> Either AlgErr (t,t)
rEuclidBezoutT _ = rEuclidBezout

rEuclidBezoutsT :: (Ringoid t, URingoid t, EDoid t) => t -> [t] -> Either AlgErr [t]
rEuclidBezoutsT _ = rEuclidBezouts



{-------------}
{- :Fieldoid -}
{-------------}

class Fieldoid t



{------------------}
{- :DaggerRingoid -}
{------------------}

class DagRingoid t where
  rDag :: t -> Either AlgErr t



{---------------------}
{- :BiproductRingoid -}
{---------------------}

class BipRingoid t where
  rBipIn  :: t -> t -> Either AlgErr t -- vcat
  rBipOut :: t -> t -> Either AlgErr t -- hcat



{--------------}
{- :Instances -}
{--------------}

{------------}
{- :Integer -}
{------------}

instance Ringoid Integer where
  rAdd a b  = return (a+b)
  rMul a b  = return (a*b)
  rNeg a    = (-a)
  rZero     = 0
  rIsZero a = (0 == a)

  rNeutOf _ = return 0
  rLAnnOf _ = return 0
  rRAnnOf _ = return 0


instance RingoidDiv Integer where
  rDivides 0 0 = return True
  rDivides _ 0 = return False
  rDivides a b = return (rem b a == 0)


instance ORingoid Integer where
  rLT a b = return (a < b)

  rIsPos a = (0 < a)
  rIsNeg a = (a < 0)


instance CRingoid Integer


instance URingoid Integer where
  rOne = 1
  rIsOne a = (1 == a)

  rIsUnit a = return (a==1 || a==(-1))
  rInjInt a = return a

  rInv 1    = return 1
  rInv (-1) = return (-1)
  rInv a    = Left (RingoidNotInvertibleErr $ show a)

  rLOneOf _ = return 1
  rROneOf _ = return 1


instance URingoidAssoc Integer where
  rAssoc a b = Right (a==b || a==(-b))


instance Domainoid Integer


instance GCDoid Integer where
  rGCD a 0 = return (abs a)
  rGCD 0 b = return (abs b)
  rGCD a b = rEuclidGCD a b >>= (return . rAbs)

  rLCM 0 _ = return 0
  rLCM _ 0 = return 0
  rLCM a b = return (lcm a b)


instance BDoid Integer where
  rBezout = rEuclidBezout


instance UFDoid Integer where
  rFactor 0 = Left undefined
  rFactor n = do
    let u  = if n > 0 then 1 else (-1)
    let ps = qux (abs n)
    return (u,ps)
    where
      foo p = zip (iterate (p*) p) [1..]
      
      baz h p = let (_,k) = head $ dropWhile (\(t,_) -> h`rem`t == 0) (foo p) in
        (k-1, h `quot` (p^(k-1)))
      
      qux = reverse . mung primes []
        where
          mung _      xs 1 = xs
          mung (p:ps) xs m
            = let (k,t) = baz m p in
                if k==0
                  then mung ps xs m
                  else mung ps ((p,k):xs) t
          mung _      _  _ = error "Integer UFDoid instance: rFactor"

  rIsSquare n
    | n < 0  = Right False
    | n == 0 = Right True
    | otherwise = Right $ foo 1
    where
      foo k = case compare (k^(2::Integer)) n of
                GT -> False
                EQ -> True
                LT -> foo (k+1)

  rIsSqFree n = do
    k <- rSqPart n
    return (k == 1)

  rSqPart n
    | n < 0  = rSqPart (-n)
    | n == 0 = Right 0
    | otherwise = Right $ last $ filter (\k -> n`rem`k == 0) $ takeWhile (<= n) $ map (^(2::Integer)) [1..n]

  rSqFreePart n = do
    d <- rSqPart n
    return (div n d)


instance EDoid Integer where
  rNorm a = return (abs a)

  rDivAlg _ 0 = Left (RingoidDivideByZeroErr "")
  rDivAlg a b
    | a >= 0 = do
        let q = (signum b) * (quot a (abs b))
        let r = mod a (abs b)
        return (q,r)
    | otherwise = do
        let q = negate $ (signum b) * ((quot (-a) (abs b)) + 1)
        let r = (abs b) - (mod (-a) (abs b))
        return (q,r)



{---------}
{- :Bool -}
{---------}

instance Ringoid Bool where
  -- xor
  rAdd True  True  = return False
  rAdd True  False = return True
  rAdd False True  = return True
  rAdd False False = return False

  rMul p q = return (p && q)
  rNeg     = id

  rZero     = False
  rIsZero t = (t == False)

  rNeutOf _ = return False
  rLAnnOf _ = return False
  rRAnnOf _ = return False


instance RingoidDiv Bool where
  -- implies
  rDivides True  True  = return True
  rDivides True  False = return True
  rDivides False True  = return False
  rDivides False False = return True


instance CRingoid Bool


instance URingoid Bool where
  rOne = True
  rIsOne t = (t == True)

  rIsUnit = return . rIsOne

  rInjInt k = return (odd k)

  rInv True  = return True
  rInv False = Left (RingoidNotInvertibleErr "#f")

  rLOneOf _ = return True
  rROneOf _ = return True


instance URingoidAssoc Bool where
  rAssoc a b = return (a == b)


instance Domainoid Bool


instance GCDoid Bool where
  rGCD p q = return (p && q)
  rLCM _ _ = return True


instance BDoid Bool where
  rBezout False False = return (False, False)
  rBezout False True  = return (False, True)
  rBezout True  False = return (True,  False)
  rBezout True  True  = return (True,  False)


instance EDoid Bool where
  rNorm False = return 0
  rNorm True  = return 1

  rDivAlg _     False = Left (RingoidDivideByZeroErr "#f")
  rDivAlg True  True  = return (True, False)
  rDivAlg False True  = return (False, False)


instance Fieldoid Bool



{-----------}
{- :Tuples -}
{-----------}

instance (Ringoid a, Ringoid b) => Ringoid (a,b) where
  rAdd (a1,b1) (a2,b2) = do
    x <- rAdd a1 a2
    y <- rAdd b1 b2
    return (x,y)

  rNeg (a,b) = (rNeg a, rNeg b)

  rMul (a1,b1) (a2,b2) = do
    x <- rMul a1 a2
    y <- rMul b1 b2
    return (x,y)

  rZero = (rZero, rZero)

  rIsZero (a,b) = (rIsZero a) && (rIsZero b)

  rNeutOf (a,b) = do
    x <- rNeutOf a
    y <- rNeutOf b
    return (x,y)

  rLAnnOf (a,b) = do
    x <- rLAnnOf a
    y <- rLAnnOf b
    return (x,y)

  rRAnnOf (a,b) = do
    x <- rRAnnOf a
    y <- rRAnnOf b
    return (x,y)
