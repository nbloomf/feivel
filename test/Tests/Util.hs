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

module Tests.Util where

import Test.QuickCheck

import Control.Monad
import Control.Monad.Instances ()

arb2 :: (Arbitrary a) => a -> Gen (a,a)
arb2 _ = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

arb3 :: (Arbitrary a) => a -> Gen (a,a,a)
arb3 _ = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a,b,c)

{---------------}
{- :Properties -}
{---------------}

checkEither :: Either err Bool -> Bool
checkEither (Left _)  = error "checkEither"
checkEither (Right p) = p

{-----------}
{- :Binary -}
{-----------}

isIdempotentBy :: (a -> a -> Either err a) -> (a -> a -> Either err Bool) -> a -> Bool
isIdempotentBy op eq a = checkEither foo
  where
    foo = do
      t <- a `op` a
      t `eq` a


isAssociativeBy :: (a -> a -> Either err a) -> (a -> a -> Either err Bool) -> (a,a,a) -> Bool
isAssociativeBy op eq (x,y,z) = checkEither foo
  where
    foo = do
      t <- (x `op` y) >>= (`op` z)
      u <- (x `op`) =<< (y `op` z)
      t `eq` u


isCommutativeBy :: (a -> a -> Either err a) -> (a -> a -> Either err Bool) -> (a,a) -> Bool
isCommutativeBy op eq (x,y) = checkEither foo
  where
    foo = do
      t <- x `op` y
      u <- y `op` x
      t `eq` u


isLDistributiveOverBy ::
  (a -> a -> Either err a) -> (a -> a -> Either err a) -> (a -> a -> Either err Bool)
    -> (a,a,a) -> Bool
isLDistributiveOverBy op up eq (x,y,z) = checkEither foo
  where
    foo = do
      t <- (x `op`) =<< (y `up` z)
      u <- join $ liftM2 up (x `op` y) (x `op` z)
      t `eq` u


isRDistributiveOverBy ::
  (a -> a -> Either err a) -> (a -> a -> Either err a) -> (a -> a -> Either err Bool)
    -> (a,a,a) -> Bool
isRDistributiveOverBy op up eq (x,y,z) = checkEither foo
  where
    foo = do
      t <- (x `up` y) >>= (`op` z)
      u <- join $ liftM2 up (x `op` z) (y `op` z)
      t `eq` u


isLIdentityUnderBy :: (a -> a -> Either err a) -> (a -> a -> Either err Bool) -> (a,a) -> Bool
isLIdentityUnderBy op eq (x,y) = checkEither foo
  where
    foo = do
      t <- x `op` y
      t `eq` y


isRIdentityUnderBy :: (a -> a -> Either err a) -> (a -> a -> Either err Bool) -> (a,a) -> Bool
isRIdentityUnderBy op eq (x,y) = checkEither foo
  where
    foo = do
      t <- x `op` y
      t `eq` x


isLZeroUnderBy :: (a -> a -> Either err a) -> (a -> a -> Either err Bool) -> (a,a) -> Bool
isLZeroUnderBy op eq (x,y) = checkEither foo
  where
    foo = do
      t <- x `op` y
      t `eq` x


isRZeroUnderBy :: (a -> a -> Either err a) -> (a -> a -> Either err Bool) -> (a,a) -> Bool
isRZeroUnderBy op eq (x,y) = checkEither foo
  where
    foo = do
      t <- x `op` y
      t `eq` y

isSatisfiedAfter :: (a -> Bool) -> (a -> a -> Either err a) -> (a,a) -> Bool
isSatisfiedAfter p op (x,y) = checkEither foo
  where
    foo = do
      t <- x `op` y
      return $ p t


{----------}
{- :Unary -}
{----------}

isIdempotentUBy :: (a -> a) -> (a -> a -> Either err Bool) -> a -> Bool
isIdempotentUBy op eq a = checkEither foo
  where
    foo = (op $ op a) `eq` (op a)

isInvolutiveUBy :: (a -> a) -> (a -> a -> Either err Bool) -> a -> Bool
isInvolutiveUBy op eq a = checkEither foo
  where
    foo = (op $ op a) `eq` a

isDistributiveUOverBy
  :: (a -> a) -> (a -> a -> Either err a) -> (a -> a -> Either err Bool)
    -> (a,a) -> Bool
isDistributiveUOverBy op up eq (a,b) = checkEither foo
  where
    foo = do
      t <- a `up` b
      u <- (op a) `up` (op b)
      (op t) `eq` u

testDivAlgRem :: (a -> a -> Either err (a,a)) -> (a -> Either err Integer)
  -> (a -> Bool) -> (a,a) -> Bool
testDivAlgRem divAlg norm isZero (a,b) = checkEither remainder
  where
    remainder = do
      (_,r) <- divAlg a b
      if isZero r
        then return True
        else do
          nr <- norm r
          nb <- norm b
          return (nr < nb)

testDivAlgQuot :: (a -> a -> Either err (a,a)) -> (a -> a -> Either err a)
  -> (a -> a -> Either err a) -> (a -> a -> Either err Bool) -> (a,a) -> Bool
testDivAlgQuot divAlg mul add eq (a,b) = checkEither checkRes
  where
    checkRes = do
      (q,r) <- divAlg a b
      qb <- q `mul` b
      a' <- qb `add` r
      eq a a'
