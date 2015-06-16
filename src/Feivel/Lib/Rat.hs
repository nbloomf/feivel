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

{-# OPTIONS_GHC -XTypeOperators #-}

module Feivel.Lib.Rat (
  Rat(..), toRat, makeRat, digits, ratRed, toDouble, canon,

  ratFlr, ratNum, ratDen,

  ratFlrSqt, ratSqt,

  ratStdDev, ratZScore, ratIntStdDev,
  ratIntZScore
) where

{-------------------}
{- :Types          -}
{- :Instances      -}
{- :Arithmetic     -}
{- :Approximations -}
{-------------------}

import Prelude hiding (abs)
import Control.Monad.Instances ()

import Feivel.Lib.Ring
import Feivel.Lib.AlgErr ()
import Feivel.Lib.Canon

{----------}
{- :Types -}
{----------}

data Rat = Integer :/: Integer

toRat :: Integer -> Rat
toRat k = k:/:1

toDouble :: Rat -> Double
toDouble (a:/:b) = (fromIntegral a) / (fromIntegral b)

makeRat :: Integer -> Integer -> Either AlgErr Rat
makeRat _ 0 = Left (RingoidDivideByZeroErr "makeRat")
makeRat a b = Right $ canon $ a:/:b

ratRed :: Rat -> Either AlgErr Rat
ratRed (_:/:0) = Left (RingoidDivideByZeroErr "ratRed")
ratRed p       = Right $ canon p

ratNum :: Rat -> Either AlgErr Integer
ratNum p = foo $ canon p
  where foo (a:/:_) = Right a

ratDen :: Rat -> Either AlgErr Integer
ratDen p = foo $ canon p
  where foo (_:/:b) = Right b



{--------------}
{- :Instances -}
{--------------}

instance Eq Rat where
  (a:/:b) == (c:/:d) = a*d == b*c

instance Ord Rat where
 p <= q = a*d <= c*b
  where
    (a:/:b) = canon p
    (c:/:d) = canon q

instance Show Rat where
  show p = foo $ canon p
    where
      foo (a:/:b)
       | a == 0    = "0"
       | b == 1    = show a
       | otherwise = show a ++ "/" ++ show b

instance Canon Rat where
  canon = fixSign . reduce
    where
      reduce (_ :/: 0) = error "division by zero"
      reduce (a :/: b) = (div a d) :/: (div b d)
        where d = gcd a b
  
      fixSign (0 :/: _) = 0 :/: 1
      fixSign (_ :/: 0) = error "division by zero"
      fixSign (h :/: k)
        | h > 0 && k > 0 = h :/: k
        | h > 0 && k < 0 = (-h) :/: (-k)
        | h < 0 && k > 0 = h :/: k
        | otherwise      = (-h) :/: (-k)


digits :: Integer -> Rat -> String
digits k p
 = if p < (0:/:1) 
    then "-" ++ digits k (neg p)
    else foo $ snarf k $ ratDigits p
  where
    snarf t = zipWith (\ _ x -> x) [0..t]

    foo []     = "0"
    foo [n]    = show n
    foo (n:ds) = show n ++ "." ++ concatMap show ds

    ratDigits (a:/:b) = q : (ratDigits ((10*r) :/: b))
      where
        q = quot a b; r = rem a b



{---------------}
{- :Arithmetic -}
{---------------}

instance Ringoid Rat where
  rAdd (a:/:b) (c:/:d) = return $ canon $ (a*d + b*c) :/: (b*d)
  rMul (a:/:b) (c:/:d) = return $ canon $ (a*c) :/: (b*d)
  rNeg (a:/:b) = (-a) :/: b

  rNeutOf _ = return (0:/:1)
  rLAnnOf _ = return (0:/:1)
  rRAnnOf _ = return (0:/:1)

  rZero = 0:/:1
  rIsZero (a:/:_) = (a == 0)


instance RingoidDiv Rat where
  rDivides (0:/:_) _ = return False
  rDivides _       _ = return True


instance ORingoid Rat where
  rLT p q = return (p < q)

  rIsPos p = ((0:/:1) < p)
  rIsNeg p = (p < (0:/:1))


instance CRingoid Rat


instance URingoid Rat where
  rOne     = 1:/:1
  rIsOne p = (p == (1:/:1))

  rIsUnit (0:/:_) = return False
  rIsUnit _       = return True

  rLOneOf _ = return (1:/:1)
  rROneOf _ = return (1:/:1)

  rInjInt k = return (k:/:1)

  rInv (0:/:b) = Left (RingoidDivideByZeroErr $ show b)
  rInv (a:/:b) = return (b:/:a)


--

neg :: Rat -> Rat
neg (a:/:b) = (-a) :/: b



(%+) :: Rat -> Rat -> Rat
(a:/:b) %+ (c:/:d) = (a*d + b*c) :/: (b*d)


(%*) :: Rat -> Rat -> Rat
(a:/:b) %* (c:/:d) = canon $ (a*c) :/: (b*d)

prd :: [Rat] -> Rat
prd = foldr (%*) (1:/:1)


(%-) :: Rat -> Rat -> Rat
(a:/:b) %- (c:/:d) = (a*d - b*c) :/: (b*d)


inv :: Rat -> Rat
inv (a:/:b) = b:/:a


(%/) :: Rat -> Rat -> Rat
(a:/:b) %/ (c:/:d) = (a*d) :/: (b*c)




square :: Rat -> Rat
square p = p %* p

(%^) :: Rat -> Integer -> Rat
p %^ n
 | n >= 0 = prd [p | _ <- [1..n]]
 | otherwise = inv $ p %^ (-n)


flr :: Rat -> Integer
flr (a:/:b) = a`div`b

-- floor(p)
ratFlr :: Rat -> Either AlgErr Integer
ratFlr (a:/:0) = Left (RingoidDivideByZeroErr $ show a)
ratFlr (a:/:b) = Right $ a`div`b

-- floor(sqrt(p))
ratFlrSqt :: Rat -> Either AlgErr Integer
ratFlrSqt p
 | p < (0:/:1) = Left (EvenRootOfNegative $ show p)
 | p < (1:/:1) = Right 0
 | p < (4:/:1) = Right 1
 | p < (9:/:1) = Right 2
 | otherwise   = Right $ foo 2 (flr p)
    where
      foo a b = if b-a == 1 then a
        else if x == (0:/:1) then m
          else if x < (0:/:1) then foo m b
            else foo a m
        where
          m = flr $ (a+b) :/: 2
          x = (square (m:/:1)) %- p




{-------------------}
{- :Approximations -}
{-------------------}

type Epsilon = Rat
data CauchyReal = (Rat, Epsilon) :>: CauchyReal

within :: CauchyReal -> Epsilon -> Rat
((x,err) :>: rest) `within` bound
 = if err < bound then x else rest `within` bound

digitsOf :: Integer -> CauchyReal -> Rat
n `digitsOf` p = p `within` (1 :/: (10^n))

build :: ((Rat, Epsilon) -> (Rat, Epsilon)) -> (Rat, Epsilon) -> CauchyReal
build f x = x :>: (build f (f x))



cauchySqt :: Rat -> Either AlgErr CauchyReal
cauchySqt p = do
  m <- ratFlrSqt p
  let x0 = (m :/: 1) %+ (1:/:2)
  let e0 = (1:/:2)
  let heron (xn, en) =
        ( (xn %+ (p %/ xn)) %/ (2:/:1)
        , (en %^ 2) %/ ((2:/:1) %* ((1:/:1) %+ en)))
  return $ build heron (x0, e0)

ratSqt :: Rat -> Integer -> Either AlgErr Rat
ratSqt p d = do
  x <- cauchySqt p
  return $ d `digitsOf` x


ratStdDev :: [Rat] -> Integer -> Either AlgErr Rat
ratStdDev []  _ = Left (RingoidEmptyListErr "ratStdDev")
ratStdDev [_] _ = Left (RingoidSingletonListErr "ratStdDev")
ratStdDev ps  k = do
  let n = sum $ map (const 1) ps
  mu <- rMean ps
  s  <- rSum [square (p %- mu) | p <- ps]
  t  <- rDiv s (n:/:1)
  sigma <- cauchySqt t
  return $ k `digitsOf` sigma

ratIntStdDev :: [Integer] -> Integer -> Either AlgErr Rat
ratIntStdDev ks d = ratStdDev (map (:/:1) ks) d

ratZScore :: Rat -> [Rat] -> Integer -> Either AlgErr Rat
ratZScore x ps err = do
  mu    <- rMean ps
  sigma <- ratStdDev ps err
  (x %- mu) `rDiv` sigma

ratIntZScore :: Rat -> [Integer] -> Integer -> Either AlgErr Rat
ratIntZScore x ks err = ratZScore x (map (:/: 1) ks) err





{-
--


sameSign :: Rat -> Rat -> Bool
sameSign p q = p %* q < zeroRat



{-
mung :: (b -> (b,a)) -> b -> [a]
mung f x = y : mung f x'
  where (x', y) = f x

-- f must be cts and have a root in [lo,hi], where lo <= hi.
bisect' :: (Rat -> Rat) -> (Rat, Rat) -> ((Rat, Epsilon), (Rat, Rat))
bisect' f (lo,hi) = (((lo %+ hi) %/ (inj 2), (lo %- hi) %/ (inj 2)), (lo', hi'))
  where
    (lo', hi') = let mid = (lo %+ hi) %/ (inj 2) in
      if sameSign lo mid then (mid, hi) else (lo, mid)

bisect :: (Rat -> Rat) -> (Rat, Rat) -> CauchyReal
bisect f (lo,hi) = mung (bisect' f) (lo,hi)
-}





floorCurtRat :: Rat -> Integer
floorCurtRat p
 | p < zeroRat  = negate $ floorCurtRat $ mnsRat p
 | p == zeroRat = 0
 | p < oneRat   = 0
 | p == oneRat  = 1
 | p < (inj 2)  = 1
 | otherwise   = foo 0 (floorRat p)
  where
    foo a b
     | b-a == 1 = a
     | otherwise = let x = floorRat ((a+b) :/: 2) in
                   if ((inj x) %^ 3) == p
                    then x
                    else if ((inj x) %^ 3) < p
                          then foo x b
                          else foo a x

-}