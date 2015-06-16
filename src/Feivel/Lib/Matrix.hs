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

{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

module Feivel.Lib.Matrix (
  Matrix(Null), Dim, Index, mCell,

  -- Construct
  mRowMajorFromList, mFromRowList, mRowFromList, mColFromList, mSingleton,
  mFromMap, mConst, mFromMapM, mToRowList,

  mEntryOf, mIsNull, mIsSquare,

  mNumRows, mNumCols,

  mIsRow, mIsCol,

  mTranspose, mHCat, mVCat, mHCats, mVCats,

  mSwapRows, mSwapCols, mDelRow, mDelCol,

  mShowStr, tabulateWithM,

  mAddRow, mAddCol, mScaleRow, mScaleCol,

  mSeq, mAny, mAll, mCombine, mConvolve,

  mEScale, mEAdd, mESwap, mEId,
  mEScaleT, mEAddT, mESwapT, mEIdT,

  gjPivots, gjREForm, mClearAbovePivot, mClearAbovePivots, gjREFactor,
  mGJForm, mGJFactor, mIsGaussJordanForm

) where

{--------------------}
{- Contents         -}
{-   :General       -}
{-     :Types       -}
{-     :Construct   -}
{-     :Query       -}
{-     :Structure   -}
{-     :Monad       -}
{-     :Mutate      -}
{-     :Display     -}
{-     :Arithmetic  -}
{-   :Algebra       -}
{-     :Ringoid     -}
{-     :URingoid    -}
{-     :GaussJordan -}
{--------------------}

import Feivel.Lib.Ring
import Feivel.Lib.Monad

import GHC.Arr
import Control.Monad.Instances ()
import Data.List (groupBy, findIndex, intercalate, genericLength, isPrefixOf)
import Control.Monad (foldM, zipWithM)
import Control.Monad.ListM (foldM1)


{----------}
{- :Types -}
{----------}

type Dim   = (Integer, Integer)
type Index = (Integer, Integer)

data Matrix a
  = Matrix (Array Index a) | Null
  deriving Eq



{--------------}
{- :Construct -}
{--------------}

-- From Element

mCell :: a -> Matrix a
mCell x = Matrix $ array ((1,1),(1,1)) [((1,1),x)]

-- From List

-- This should be only one of two constructors which call ``array''.
mRowMajorFromList :: Integer -> Integer -> [a] -> Either AlgErr (Matrix a)
mRowMajorFromList r c as
  | r <= 0 || c <= 0 = Left (NullMatrix "mRowMajorFromList")
  | otherwise = do
      if enoughElts (r*c) as
        then do
          let bds = ((1,1), (fromIntegral r, fromIntegral c))
          return $ Matrix $ array bds $ zip (range bds) as
        else Left undefined
  where
    enoughElts _ [] = True
    enoughElts n (_:xs)
      | n <= 0 = True
      | otherwise = enoughElts (n-1) xs

mFromRowList :: [[a]] -> Either AlgErr (Matrix a)
mFromRowList [] = return Null
mFromRowList xss = do
  let r = genericLength xss
  let c = genericLength (head xss)
  mRowMajorFromList r c (concat xss)

mRowFromList :: [a] -> Either AlgErr (Matrix a)
mRowFromList as = mRowMajorFromList 1 (genericLength as) as

mColFromList :: [a] -> Either AlgErr (Matrix a)
mColFromList as = mRowMajorFromList (genericLength as) 1 as

mSingleton :: a -> Either AlgErr (Matrix a)
mSingleton a = mRowMajorFromList 1 1 [a]


-- From Map

mFromMap :: Dim -> (Index -> a) -> Either AlgErr (Matrix a)
mFromMap (r,c) f = mRowMajorFromList r c $ map f $ range ((1,1),(r,c))

mConst :: Dim -> a -> Either AlgErr (Matrix a)
mConst d a = mFromMap d (const a)

mFromMapM :: Dim -> (Index -> Either AlgErr a) -> Either AlgErr (Matrix a)
mFromMapM (r,c) f = do
  as <- sequence $ map f $ range ((1,1),(r,c))
  mRowMajorFromList r c as


{----------}
{- :Query -}
{----------}

mIsNull :: Matrix a -> Either AlgErr Bool
mIsNull (Matrix _) = return False
mIsNull Null       = return True


-- Dimension

mDim :: Matrix a -> Either AlgErr Dim
mDim (Matrix m) = Right $ snd $ bounds m
mDim Null       = Left (NullMatrix "mDim")

mNumRows :: Matrix a -> Either AlgErr Integer
mNumRows Null = Left (NullMatrix "mNumRows")
mNumRows m = do
  (r,_) <- mDim m
  return r

mIsRow :: Matrix a -> Either AlgErr Bool
mIsRow m = do
  r <- mNumRows m
  return (r == 1)

mNumCols :: Matrix a -> Either AlgErr Integer
mNumCols Null = Left (NullMatrix "mNumCols")
mNumCols m = do
  (_,c) <- mDim m
  return c

mIsCol :: Matrix a -> Either AlgErr Bool
mIsCol m = do
  c <- mNumCols m
  return (c == 1)

mIsSquare :: Matrix a -> Either AlgErr Bool
mIsSquare Null = return False
mIsSquare m = do
  (r,c) <- mDim m
  return (r == c)


-- Indices

mIsRowIndexOf :: Integer -> Matrix a -> Either AlgErr Bool
_ `mIsRowIndexOf` Null = return False
i `mIsRowIndexOf` m = do
  (r,_) <- mDim m
  return $ 0 < i && i <= r

mIsColIndexOf :: Integer -> Matrix a -> Either AlgErr Bool
_ `mIsColIndexOf` Null = return False
j `mIsColIndexOf` m = do
  (_,c) <- mDim m
  return $ 0 < j && j <= c

mIsIndexOf :: Index -> Matrix a -> Either AlgErr Bool
(i,j) `mIsIndexOf` m = do
  p <- i `mIsRowIndexOf` m
  q <- j `mIsColIndexOf` m
  return (p && q)


-- Entries

mEntryOf :: Index -> Matrix a -> Either AlgErr a
_     `mEntryOf` Null = Left (NullMatrix "mEntryOf")
(i,j) `mEntryOf` m@(Matrix a) = do
  p <- (i,j) `mIsIndexOf` m
  if p
    then Right $ a ! (i,j)
    else do
      d <- mDim m
      Left (InvalidIndex $ show (i,j) ++ show d)

mToRowList :: Matrix a -> [[a]]
mToRowList Null = []
mToRowList (Matrix a) = map (map snd) $ groupBy foo $ assocs a
  where foo ((i1,_),_) ((i2,_),_) = i1 == i2

toListM :: Matrix a -> [a]
toListM Null = []
toListM (Matrix m) = map snd $ assocs m       

mListRowOf :: Integer -> Matrix a -> Either AlgErr [a]
k `mListRowOf` m = do
  p <- k `mIsRowIndexOf` m
  (r,c) <- mDim m
  if not p
    then Left (InvalidRowIndex $ show k ++ show (r,c))
    else sequence $ map (\j -> (k,j) `mEntryOf` m) [1..c]

mListColOf :: Integer -> Matrix a -> Either AlgErr [a]
k `mListColOf` m = do
  p <- k `mIsColIndexOf` m
  (r,c) <- mDim m
  if not p
    then Left (InvalidColIndex $ show k ++ show (r,c))
    else sequence $ map (\i -> (i,k) `mEntryOf` m) [1..r]


-- Entrywise bool

mAll :: (a -> Bool) -> Matrix a -> Bool
mAll p m = all p $ toListM m

mAny :: (a -> Bool) -> Matrix a -> Bool
mAny p m = any p $ toListM m



{--------------}
{- :Structure -}
{--------------}

mRowOf :: Integer -> Matrix a -> Either AlgErr (Matrix a)
i `mRowOf` m = do
  p <- i `mIsRowIndexOf` m
  (r,c) <- mDim m
  if not p
    then Left (InvalidRowIndex $ show i ++ show (r,c))
    else do
      let foo (h,k) = (h+i-1,k) `mEntryOf` m
      mFromMapM (1,c) foo

mRowsOf :: Matrix a -> Either AlgErr [Matrix a]
mRowsOf Null = Right []
mRowsOf m = do
  r <- mNumRows m
  sequence $ map (`mRowOf` m) [1..r]

mColOf :: Integer -> Matrix a -> Either AlgErr (Matrix a)
j `mColOf` m = do
  p <- j `mIsColIndexOf` m
  (r,c) <- mDim m
  if not p
    then Left (InvalidColIndex $ show j ++ show (r,c))
    else do
      let foo (h,k) = (h,k+j-1) `mEntryOf` m
      mFromMapM (r,1) foo

mColsOf :: Matrix a -> Either AlgErr [Matrix a]
mColsOf Null = Right []
mColsOf m = do
  c <- mNumCols m
  sequence $ map (`mColOf` m) [1..c]

mTranspose :: Matrix a -> Either AlgErr (Matrix a)
mTranspose Null = Right Null
mTranspose m = do
  (r,c) <- mDim m
  let foo (i,j) = (j,i) `mEntryOf` m
  (c,r) `mFromMapM` foo

-- Horizontal catenate
mHCat :: Matrix a -> Matrix a -> Either AlgErr (Matrix a)
mHCat a Null = Right a
mHCat Null b = Right b
mHCat a b = do
  (ra,ca) <- mDim a
  (rb,cb) <- mDim b
  if ra /= rb
    then Left (DimMismatch $ show (ra,ca) ++ show (rb,cb))
    else do
      let foo (i,j) = if j <= ca then (i,j) `mEntryOf` a else (i,j-ca) `mEntryOf` b
      mFromMapM (ra, ca + cb) foo

-- Vertical catenate
mVCat :: Matrix a -> Matrix a -> Either AlgErr (Matrix a)
mVCat a Null = Right a
mVCat Null b = Right b
mVCat a b = do
  (ra,ca) <- mDim a
  (rb,cb) <- mDim b
  if ca /= cb
    then Left (DimMismatch $ show (ra,ca) ++ show (rb,cb))
    else do
      let foo (i,j) = if i <= ra then (i,j) `mEntryOf` a else (i-ra,j) `mEntryOf` b
      mFromMapM (ra + rb, ca) foo

mHCats, mVCats :: [Matrix a] -> Either AlgErr (Matrix a)
mHCats = foldM mHCat Null
mVCats = foldM mVCat Null

-- Quadrant catenate
mQCat :: ((Matrix a, Matrix a), (Matrix a, Matrix a)) -> Either AlgErr (Matrix a)
mQCat ((a,b),(c,d)) = do
  h <- mHCat a b
  k <- mHCat c d
  mVCat h k

mHSplit :: Matrix a -> Integer -> Either AlgErr (Matrix a, Matrix a)
mHSplit Null _ = Right (Null, Null)
mHSplit m k = do
  (r,c) <- mDim m
  if k <= 1
    then return (Null, m)
    else if k >= c+1
      then return (m, Null)
      else do
        a <- mFromMapM (r, k-1)   (\(i,j) -> (i,j)      `mEntryOf` m)
        b <- mFromMapM (r, c-k+1) (\(i,j) -> (i, j+k-1) `mEntryOf` m)
        return (a,b)

mVSplit :: Matrix a -> Integer -> Either AlgErr (Matrix a, Matrix a)
mVSplit Null _ = Right (Null, Null)
mVSplit m k = do
  (r,c) <- mDim m
  if k <= 1
    then return (Null, m)
    else if k >= r+1
      then return (m, Null)
      else do
        a <- mFromMapM (k-1,   c) (\(i,j) -> (i,j)      `mEntryOf` m)
        b <- mFromMapM (r-k+1, c) (\(i,j) -> (i+k-1, j) `mEntryOf` m)
        return (a,b)

mQSplit :: Matrix a -> Index -> Either AlgErr ((Matrix a, Matrix a), (Matrix a, Matrix a))
mQSplit m (i,j) = do
  (h,k) <- m `mVSplit` i
  (a,b) <- h `mHSplit` j
  (c,d) <- k `mHSplit` j
  return ((a,b),(c,d))



{----------}
{- :Monad -}
{----------}

instance Functor Matrix where
  fmap _ Null       = Null
  fmap f (Matrix m) = Matrix $ fmap f m

mSeq :: (Monad m) => Matrix (m a) -> m (Matrix a)
mSeq m = do
  case mToRowList m of
    [] -> return Null
    as -> do
      rs <- sequence $ map sequence as
      case mDim m of
        Left _ -> return Null
        Right (r,c) -> do
          let bds = ((1,1),(r,c))
          return $ Matrix $ array bds $ zip (range bds) (concat rs)



{-----------}
{- :Mutate -}
{-----------}

mSwapRows :: Integer -> Integer -> Matrix a -> Either AlgErr (Matrix a)
mSwapRows r1 r2 m = do
  (r,c) <- mDim m
  let foo (i,j)
        | i == r1   = (r2,j) `mEntryOf` m
        | i == r2   = (r1,j) `mEntryOf` m
        | otherwise = (i,j)  `mEntryOf` m
  (r,c) `mFromMapM` foo

mSwapCols :: Integer -> Integer -> Matrix a -> Either AlgErr (Matrix a)
mSwapCols c1 c2 m = do
  (r,c) <- mDim m
  let foo (i,j)
        | j == c1   = (i,c2) `mEntryOf` m
        | j == c2   = (i,c1) `mEntryOf` m
        | otherwise = (i,j)  `mEntryOf` m
  (r,c) `mFromMapM` foo

mDelRow :: Matrix a -> Integer -> Either AlgErr (Matrix a)
m `mDelRow` t = do
  p <- t `mIsRowIndexOf` m
  if not p
    then return m
    else do
      (r,c) <- mDim m
      let foo (i,j) = if i < t then (i,j) `mEntryOf` m else (i+1,j) `mEntryOf` m
      mFromMapM (r-1, c) foo

mDelCol :: Matrix a -> Integer -> Either AlgErr (Matrix a)
m `mDelCol` t = do
  p <- t `mIsColIndexOf` m
  if not p
    then return m
    else do
      (r,c) <- mDim m
      let foo (i,j) = if j < t then (i,j) `mEntryOf` m else (i,j+1) `mEntryOf` m
      mFromMapM (r, c-1) foo



{------------}
{- :Display -}
{------------}

mShowStr :: Matrix String -> Either AlgErr String
mShowStr m = do
  let
    bar [] = "[]"
    bar xs = "[" ++ (intercalate ";" xs) ++ "]"
  return $ bar $ map bar $ mToRowList m

instance (Show a) => Show (Matrix a) where
  show m = case mShowStr $ fmap show m of
    Left _  -> "error in matrix"
    Right s -> s

tabulateWith :: (a -> String) -> Matrix a -> Either AlgErr String
tabulateWith _ Null = return "[]"
tabulateWith f m = do
  let rows = map (map f) $ mToRowList m
  let k = maximum $ map length $ concat rows
  let g x = reverse $ take k $ (reverse x) ++ (repeat ' ')
  let foo xs = "[ " ++ xs ++ " ]"
  return $ init $ unlines $ map (foo . unwords . map g) $ rows

tabulateWithS :: (a -> String) -> Matrix a -> String
tabulateWithS f m = case tabulateWith f m of
  Left _  -> "error in tabulateWithS"
  Right s -> s

tabulateWithM :: (Functor m, Monad m) => (a -> m String) -> Matrix a -> m String
tabulateWithM f m = fmap (tabulateWithS id) $ mSeq $ fmap f m




{---------------}
{- :Arithmetic -}
{---------------}

mCombine :: (a -> b -> c) -> Matrix a -> Matrix b -> Either AlgErr (Matrix c)
mCombine _ Null Null = return Null
mCombine f m n = do
  dm <- mDim m
  dn <- mDim n
  if dm /= dn
    then Left (DimMismatch $ show dm ++ show dn)
    else do
      let foo (i,j) = do
            x <- (i,j) `mEntryOf` m
            y <- (i,j) `mEntryOf` n
            return (f x y)
      mFromMapM dm foo

mCombineE :: (a -> b -> Either AlgErr c) -> Matrix a -> Matrix b -> Either AlgErr (Matrix c)
mCombineE _ Null Null = return Null
mCombineE f m n = do
  dm <- mDim m
  dn <- mDim n
  if dm /= dn
    then Left (DimMismatch $ show dm ++ show dn)
    else do
      let foo (i,j) = do
            x <- (i,j) `mEntryOf` m
            y <- (i,j) `mEntryOf` n
            case (f x y) of
              Left err -> Left (MatCoefErr $ show err)
              Right z  -> return z
      mFromMapM dm foo

mConvolve
  :: (a -> b -> c) -> (c -> c -> c) -> Matrix a -> Matrix b -> Either AlgErr (Matrix c)
mConvolve _ _ Null _ = Right Null
mConvolve _ _ _ Null = Right Null
mConvolve f g m n = do
  (rm,cm) <- mDim m
  (rn,cn) <- mDim n
  if cm /= rn
    then Left (DimMismatch $ show (rm,cm) ++ show (rn,cn))
    else do
      let foo (i,j) = do
            as <- i `mListRowOf` m
            bs <- j `mListColOf` n
            return $ foldl1 g $ zipWith f as bs
      mFromMapM (rm,cn) foo

mConvolveE
  :: (a -> b -> Either AlgErr c) -> (c -> c -> Either AlgErr c) -> Matrix a -> Matrix b -> Either AlgErr (Matrix c)
mConvolveE _ _ Null _ = Right Null
mConvolveE _ _ _ Null = Right Null
mConvolveE f g m n = do
  (rm,cm) <- mDim m
  (rn,cn) <- mDim n
  if cm /= rn
    then Left (DimMismatch $ show (rm,cm) ++ show (rn,cn))
    else do
      let foo (i,j) = do
            as <- i `mListRowOf` m
            bs <- j `mListColOf` n
            let d = zipWithM f as bs >>= foldM1 g
            case d of
              Left err -> Left (MatCoefErr $ show err)
              Right x -> return x
      mFromMapM (rm,cn) foo



{------------}
{- :Ringoid -}
{------------}

instance (Ringoid t) => Ringoid (Matrix t) where
  rAdd m1 m2 = case mCombineE rAdd m1 m2 of
    Left _ -> undefined
    Right x -> Right x

  rMul m1 m2 = case mConvolveE rMul rAdd m1 m2 of
    Left _ -> undefined
    Right x -> Right x

  rNeg m = fmap rNeg m

  rZero = Null

  rIsZero m = mAll rIsZero m

  rNeutOf Null = return Null
  rNeutOf m = case foo of
    Left _ -> undefined
    Right z -> Right z
    where
      foo = do
        d <- mDim m
        mConst d rZero

  rLAnnOf Null = return Null
  rLAnnOf m = case foo of
    Left _ -> undefined
    Right z -> Right z
    where
      foo = do
        r <- mNumRows m
        mConst (r,r) rZero

  rRAnnOf Null = return Null
  rRAnnOf m = case foo of
    Left _ -> undefined
    Right z -> Right z
    where
      foo = do
        c <- mNumCols m
        mConst (c,c) rZero


mZerosAboveIndex :: (Ringoid a) => Matrix a -> Index -> Either AlgErr Bool
mZerosAboveIndex m (h,k) = do
  ts <- sequence $ map (\i -> (i,k) `mEntryOf` m) [1..h-1]
  return (and $ map rIsZero ts)

mScaleRow :: (Ringoid a, CRingoid a)
  => a -> Integer -> Matrix a -> Either AlgErr (Matrix a)
mScaleRow _ _ Null = Right Null
mScaleRow c h m = do
  dm <- mDim m
  let foo (i,j)
        | i==h = do
            a <- (i,j) `mEntryOf` m
            case c `rMul` a of
              Left err -> Left (MatCoefErr $ show err)
              Right x -> return x
        | otherwise = (i,j) `mEntryOf` m
  mFromMapM dm foo

mScaleCol :: (Ringoid a, CRingoid a)
  => a -> Integer -> Matrix a -> Either AlgErr (Matrix a)
mScaleCol _ _ Null = Right Null
mScaleCol c h m = do
  dm <- mDim m
  let foo (i,j)
        | j==h = do
            a <- (i,j) `mEntryOf` m
            case c `rMul` a of
              Left err -> Left (MatCoefErr $ show err)
              Right x -> return x
        | otherwise = (i,j) `mEntryOf` m
  mFromMapM dm foo

mAddRow :: (Ringoid a, CRingoid a)
  => a -> Integer -> Integer -> Matrix a -> Either AlgErr (Matrix a)
mAddRow _ _ _ Null = Right Null
mAddRow c h1 h2 m = do
  dm <- mDim m
  let foo (i,j)
        | i==h2 = do
            a1 <- (h1,j) `mEntryOf` m
            a2 <- (h2,j) `mEntryOf` m
            case opM2 rAdd (return a2) (rMul c a1) of
              Left err -> Left (MatCoefErr $ show err)
              Right x -> return x
        | otherwise = (i,j) `mEntryOf` m
  mFromMapM dm foo

mAddCol :: (Ringoid a, CRingoid a)
  => a -> Integer -> Integer -> Matrix a -> Either AlgErr (Matrix a)
mAddCol _ _ _ Null = Right Null
mAddCol c h1 h2 m = do
  dm <- mDim m
  let foo (i,j)
        | j==h2 = do
            a1 <- (i,h1) `mEntryOf` m
            a2 <- (i,h2) `mEntryOf` m
            case opM2 rAdd (return a2) (rMul c a1) of
              Left err -> Left (MatCoefErr $ show err)
              Right x -> return x
        | otherwise = (i,j) `mEntryOf` m
  mFromMapM dm foo

-- Find minimal k such that i,k entry is not zero for some i.
-- I.e. index of the leftmost nonzero column.
mFirstNonZeroColIndex :: (Ringoid a) => Matrix a -> Either AlgErr Integer
mFirstNonZeroColIndex Null = Left (NullMatrix "mFirstNonZeroColIndex")
mFirstNonZeroColIndex m = do
  cs <- mColsOf m >>= return . map (rIsZero)
  case findIndex (== False) cs of
    Nothing -> Left ZeroMatrix
    Just k  -> Right $ fromIntegral (k+1)

-- Find minimal k such that k,j entry is not zero for some j.
-- I.e. index of the topmost nonzero row
mFirstNonZeroRowIndex :: (Ringoid a) => Matrix a -> Either AlgErr Integer
mFirstNonZeroRowIndex Null = Left (NullMatrix "mFirstNonZeroRowIndex")
mFirstNonZeroRowIndex m = do
  cs <- mRowsOf m >>= return . map (rIsZero)
  case findIndex (== False) cs of
    Nothing -> Left ZeroMatrix
    Just k  -> Right $ fromIntegral (k+1)

instance (Ringoid t) => BipRingoid (Matrix t) where
  rBipIn x y = case mVCat x y of
    Left _ -> undefined
    Right m -> Right m

  rBipOut x y = case mHCat x y of
    Left _ -> undefined
    Right m -> Right m



{-------------}
{- :URingoid -}
{-------------}

mEId :: (Ringoid a, URingoid a) => Integer -> Either AlgErr (Matrix a)
mEId n = do
  let foo (i,j) = if i==j then rOne else rZero
  (n,n) `mFromMap` foo

mEIdT :: (Ringoid a, URingoid a) => a -> Integer -> Either AlgErr (Matrix a)
mEIdT _ = mEId

-- Swap rows/cols
mESwap :: (Ringoid a, URingoid a)
  => Integer -> Integer -> Integer -> Either AlgErr (Matrix a)
mESwap n h k = do
  let foo (i,j)
        | (i,j) == (h,k) || (i,j) == (k,h) = rOne
        | i == j && i /= h && i /= k = rOne
        | otherwise = rZero
  (n,n) `mFromMap` foo

mESwapT :: (Ringoid a, URingoid a)
  => a -> Integer -> Integer -> Integer -> Either AlgErr (Matrix a)
mESwapT _ = mESwap

-- Multiply row/col by a scalar
mEScale :: (Ringoid a, URingoid a)
  => Integer -> Integer -> a -> Either AlgErr (Matrix a)
mEScale n k a = do
  let foo (i,j)
        | i == k && k == j = a
        | i == j && i /= k = rOne
        | otherwise = rZero
  (n,n) `mFromMap` foo

mEScaleT :: (Ringoid a, URingoid a)
  => a -> Integer -> Integer -> a -> Either AlgErr (Matrix a)
mEScaleT _ = mEScale

-- Add scalar multiple of one row/col to another
mEAdd :: (Ringoid a, URingoid a)
  => Integer -> Index -> a -> Either AlgErr (Matrix a)
mEAdd n (h,k) a = do
  let foo (i,j)
        | (i,j) == (h,k) = a
        | i == j = rOne
        | otherwise = rZero
  (n,n) `mFromMap` foo

mEAddT :: (Ringoid a, URingoid a)
  => a -> Integer -> Index -> a -> Either AlgErr (Matrix a)
mEAddT _ = mEAdd



{----------------}
{- :GaussJordan -}
{----------------}

-- Detecting GJ Form

gjLeftmostNonzeroIndices :: (Ringoid a)
  => Matrix a -> Either AlgErr [(Integer, Integer)]
gjLeftmostNonzeroIndices Null = return []
gjLeftmostNonzeroIndices m = do
  rs <- mRowsOf m
  sequence $ filter isRight $ map sndSeq $ zip [1..] (map mFirstNonZeroColIndex rs)

gjLeftmostNonzeroIndicesAscending :: (Ringoid a)
  => Matrix a -> Either AlgErr Bool
gjLeftmostNonzeroIndicesAscending m = do
  ks <- gjLeftmostNonzeroIndices m
  let is = map fst ks
  let js = map snd ks
  let
    isAscending [] = True
    isAscending [_] = True
    isAscending (x:y:xs) = if x >= y then False else isAscending (y:xs)
  return $ (is `isPrefixOf` [1..]) && (isAscending js)

gjLeftmostNonzeroEntriesAreOne :: (Ringoid a, URingoid a)
  => Matrix a -> Either AlgErr Bool
gjLeftmostNonzeroEntriesAreOne m = do
  ks <- gjLeftmostNonzeroIndices m
  ts <- sequence $ map (`mEntryOf` m) ks
  return (and $ map rIsOne ts)

gjEntriesAboveLeftmostNonzerosAreZero :: (Ringoid a)
  => Matrix a -> Either AlgErr Bool
gjEntriesAboveLeftmostNonzerosAreZero m = do
  ks <- gjLeftmostNonzeroIndices m
  ps <- sequence $ map (mZerosAboveIndex m) ks
  return (and ps)

mIsGaussJordanForm :: (Ringoid a, URingoid a)
  => Matrix a -> Either AlgErr Bool
mIsGaussJordanForm m = do
  p <- gjLeftmostNonzeroIndicesAscending m
  q <- gjLeftmostNonzeroEntriesAreOne m
  r <- gjEntriesAboveLeftmostNonzerosAreZero m
  return (p && q && r)


-- Elementary Row/Column Operations

data ElemOp a
  = RowSwap  Integer Integer
  | RowScale Integer a
  | RowAdd   Integer Integer a
  | ColSwap  Integer Integer
  | ColScale Integer a
  | ColAdd   Integer Integer a
  | NoOp
  deriving (Eq, Show)

ePrependRow :: ElemOp a -> ElemOp a
ePrependRow (RowSwap  h k)   = RowSwap  (h+1) (k+1)
ePrependRow (RowScale h a)   = RowScale (h+1) a
ePrependRow (RowAdd   h k a) = RowAdd   (h+1) (k+1) a
ePrependRow x                = x

ePrependRows :: [ElemOp a] -> [ElemOp a]
ePrependRows = map ePrependRow

eOpMatrix :: (Ringoid a, URingoid a)
  => Integer -> ElemOp a -> Either AlgErr (Matrix a)
eOpMatrix n (RowSwap  h k  ) = mESwap n h k
eOpMatrix n (RowScale k a  ) = mEScale n k a
eOpMatrix n (RowAdd   h k a) = mEAdd n (k,h) a
eOpMatrix n (ColSwap  h k  ) = mESwap n h k
eOpMatrix n (ColScale k a  ) = mEScale n k a
eOpMatrix n (ColAdd   h k a) = mEAdd n (h,k) a
eOpMatrix n NoOp             = mEId n

ePerformOp :: (Ringoid a, CRingoid a)
  => Matrix a -> ElemOp a -> Either AlgErr (Matrix a)
ePerformOp m (RowSwap  h k)   = mSwapRows h k m
ePerformOp m (RowScale h c)   = mScaleRow c h m
ePerformOp m (RowAdd   h k c) = mAddRow c h k m
ePerformOp m (ColSwap  h k)   = mSwapCols h k m
ePerformOp m (ColScale h c)   = mScaleCol c h m
ePerformOp m (ColAdd   h k c) = mAddCol c h k m
ePerformOp m NoOp             = return m

ePerformOps :: (Ringoid a, CRingoid a)
  => Matrix a -> [ElemOp a] -> Either AlgErr (Matrix a)
ePerformOps = foldM ePerformOp


-- GJ Factorization

-- m = [z|n] where first column of n is not zero.
gjSplitLeftZero :: (Ringoid a)
  => Matrix a -> Either AlgErr (Matrix a, Matrix a)
gjSplitLeftZero m = do
  j <- mFirstNonZeroColIndex m
  m `mHSplit` j

-- If m -> (n, ops), then after performing ops on m, we get n, and the (1,1) entry of n is 1.
gjMoveFirstPivot :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> Either AlgErr (Matrix a, [ElemOp a])
gjMoveFirstPivot m = do
  i <- (1 `mColOf` m) >>= mFirstNonZeroRowIndex
  b <- ((i,1) `mEntryOf` m) >>= rInv
  let ops = [RowScale i b, RowSwap i 1]
  n <- ePerformOps m ops
  return (n, ops)

gjClearEntryWithPivot :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> Index -> Integer -> Either AlgErr (ElemOp a)
gjClearEntryWithPivot m (i,j) k = do
  t <- (k,j) `mEntryOf` m
  if rIsZero t
    then return NoOp
    else do
      v <- (i,j) `mEntryOf` m >>= rInv
      w <- rMul (rNeg t) v
      return (RowAdd i k w)

gjClearFirstColumn :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> Either AlgErr (Matrix a, [ElemOp a])
gjClearFirstColumn m = do
  cs <- 1 `mListColOf` m
  case cs of
    []    -> Left (NullMatrix "gjClearFirstColumn")
    [_]   -> return (m, [])
    (_:_) -> do
      r <- mNumRows m
      ops <- sequence $ map (gjClearEntryWithPivot m (1,1)) [2..r]
      n <- ePerformOps m ops
      return (n, ops)

-- If rowEchelon m = Right (ops, n, is)
-- then n is (a) row echelon form of m, with pivots is,
-- and is row-equivalent to m via the row operations ops.
gjRowEchelon :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> Either AlgErr (Matrix a, [ElemOp a], [Index])
gjRowEchelon Null = Left (NullMatrix "gjRowEchelon")
gjRowEchelon m = do
  if rIsZero m
    then return (m, [NoOp], [])
    else do
      q <- mIsRow m
      if q
        then do
          j <- mFirstNonZeroColIndex m
          b <- (1,j) `mEntryOf` m >>= rInv
          n <- mScaleRow b 1 m
          return (n, [RowScale 1 b], [(1,j)])
        else do
          (z,w1) <- gjSplitLeftZero m
          j <- case z of
                 Null -> return 0
                 _    -> mNumCols z
          (w2, movePivot)   <- gjMoveFirstPivot w1
          (w3, clearColumn) <- gjClearFirstColumn w2
          r <- mIsCol w3
          if r
            then do
              n <- z `mHCat` w3
              return (n, movePivot ++ clearColumn, [(1,j+1)])
            else do
              ((t1,t2),(t3,t4)) <- mQSplit w3 (2,2)
              (u4, recurse, piv) <- gjRowEchelon t4
              w4 <- mQCat ((t1,t2),(t3,u4))
              n <- z `mHCat` w4
              let shiftPivots = map (\(h,k) -> (h+1,k+j+1)) piv
              return (n, movePivot ++ clearColumn ++ ePrependRows recurse, (1,j+1):shiftPivots)

gjREForm :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> Either AlgErr (Matrix a)
gjREForm m = do
  (n,_,_) <- gjRowEchelon m
  return n

gjREFactor :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> Either AlgErr (Matrix a)
gjREFactor m = do
  r <- mNumRows m
  (_,ops,_) <- gjRowEchelon m
  ps <- sequence $ map (eOpMatrix r) ops
  rProd ps

gjPivots :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> Either AlgErr [Index]
gjPivots m = do
  (_,_,ps) <- gjRowEchelon m
  return ps

mClearAbovePivot :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> Index -> Either AlgErr (Matrix a, [ElemOp a])
mClearAbovePivot m (i,j) = do
  ops <- sequence $ map (gjClearEntryWithPivot m (i,j)) [1..i-1]
  n <- ePerformOps m ops
  return (n, ops)

mClearAbovePivots :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> [Index] -> Either AlgErr (Matrix a, [ElemOp a])
mClearAbovePivots m [] = return (m,[])
mClearAbovePivots m (p:ps) = do
  (n,xs) <- mClearAbovePivot m p
  (q,ys) <- mClearAbovePivots n ps
  return (q, xs ++ ys)

mGaussJordan :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> Either AlgErr (Matrix a, [ElemOp a], [Index])
mGaussJordan m = do
  (n, ops, ps) <- gjRowEchelon m
  (q, ups) <- mClearAbovePivots n ps
  return (q, ops ++ ups, ps)

mGJForm :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> Either AlgErr (Matrix a)
mGJForm m = do
  (n,_,_) <- mGaussJordan m
  return n

mGJFactor :: (Ringoid a, CRingoid a, URingoid a)
  => Matrix a -> Either AlgErr (Matrix a)
mGJFactor m = do
  r <- mNumRows m
  (_,ops,_) <- mGaussJordan m
  ps <- sequence $ map (eOpMatrix r) ops
  rProd $ reverse ps
