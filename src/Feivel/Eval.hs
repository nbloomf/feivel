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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverlappingInstances  #-}

module Feivel.Eval (
 eval, runEvalM, evalToText
) where

{-----------------------------------------------------------}
{- Contents                                                -}
{-  :Eval              :Eval:Expr        :Eval:Utilities   -}
{-    :Eval:IntExpr    :Eval:StrExpr     :Eval:BoolExpr    -}
{-    :Eval:RatExpr    :Eval:ListExpr    :Eval:MacExpr     -}
{-    :Eval:MatExpr    :Eval:Doc         :Eval:PolyExpr    -}
{-    :Eval:PermExpr   :Eval:ZZModExpr                     -}
{-                                                         -}
{-  :Inject                                                -}
{-  :Lift                                                  -}
{-  :Utilities                                             -}
{-----------------------------------------------------------}

import Feivel.Expr
import Feivel.Key
import Feivel.Lib
import Feivel.Error
import Feivel.Locus
import Feivel.EvalM
import Feivel.Store
import Feivel.Type
import Feivel.Typed
import Feivel.LaTeX
import Feivel.Get
import Feivel.Parse (pInteger, pRat)
import Feivel.Glyph

import Data.List (intersperse, (\\), sort, nub, permutations)
import Control.Monad (filterM)


zeroZZ :: Integer
zeroZZ = 0

zeroQQ :: Rat
zeroQQ = 0 :/: 1

zeroBB :: Bool
zeroBB = False

zeroMod :: Integer -> ZZModulo
zeroMod n = 0 `zzmod` n

suchThat :: (Monad m) => a -> m a
suchThat = return

hasSameTypeAs :: a -> a -> ()
hasSameTypeAs _ _ = ()


{---------}
{- :Eval -}
{---------}

class Eval t where
  eval :: t -> EvalM t

  evalWith :: t -> Store Expr -> EvalM t
  evalWith t st = do
    old <- getState
    putState st
    u <- eval t
    putState old
    return u

evalSeq :: (Eval t) => [t] -> EvalM [t]
evalSeq = sequence . map eval

evalWithSeq :: (Eval t) => [t] -> Store Expr -> EvalM [t]
evalWithSeq xs st = sequence $ map (`evalWith` st) xs

evalPar :: (Eval t) => [t] -> EvalM [t]
evalPar xs = do
  let evalLoc x = do
        st <- getState
        y  <- eval x
        putState st
        return y
  sequence $ map evalLoc xs

evalWithPar :: (Eval t) => [t] -> Store Expr -> EvalM [t]
evalWithPar xs store = do
  let evalLoc x = do
        st <- getState
        y  <- evalWith x store
        putState st
        return y
  sequence $ map evalLoc xs

instance Eval Integer where eval = return
instance Eval String  where eval = return
instance Eval Text    where eval = return
instance Eval Rat     where eval = return
instance Eval Bool    where eval = return



{--------------}
{- :Eval:Expr -}
{--------------}

instance Eval Expr where
  eval (DocE   x) = fmap toExpr $ eval x
  eval (StrE   x) = fmap toExpr $ eval x
  eval (IntE   x) = fmap toExpr $ eval x
  eval (BoolE  x) = fmap toExpr $ eval x
  eval (RatE   x) = fmap toExpr $ eval x
  eval (ListE  x) = fmap toExpr $ eval x
  eval (MacE   x) = fmap toExpr $ eval x
  eval (MatE   x) = fmap toExpr $ eval x
  eval (PolyE  x) = fmap toExpr $ eval x
  eval (PermE  x) = fmap toExpr $ eval x
  eval (ZZModE x) = fmap toExpr $ eval x



{-------------------}
{- :Eval:Utilities -}
{-------------------}

eIfThenElse :: (ToExpr a, Get a, Eval a) => BoolExpr -> a -> a -> EvalM a
eIfThenElse b t f = do
  test  <- eval b >>= getVal
  true  <- eval t >>= getVal
  false <- eval f >>= getVal
  if test then (eval true) else (eval false)

eMacro :: (Get b, Eval b) => [(Type, Key, Expr)] -> MacExpr -> Locus -> EvalM b
eMacro vals mac loc = do
  old <- getState
  ctx <- toStateT loc vals
  (defaultVals, e) <- evalWith mac (mergeStores [ctx, old]) >>= getVal :: EvalM (Store Expr, Expr)
  let newSt = mergeStores [ctx, defaultVals, old]
  evalWith e newSt >>= getVal >>= (`evalWith` newSt)

eAtIdx :: (ToExpr a, ToExpr b, ToExpr c, Get (Matrix d), Eval a, Eval b, Eval c)
  => c -> a -> b -> Locus -> EvalM d
eAtIdx m h k loc = do
  i <- eval h >>= getVal
  j <- eval k >>= getVal
  p <- eval m >>= getVal
  tryEvalM loc $ mEntryOf (i,j) p

macToGlyph :: MacExpr -> EvalM String
macToGlyph expr = do
  m <- getVal expr :: EvalM MacExpr
  case m of
    MacConst _ st ex (amb,_) :@ loc -> do
      old <- getState
      ctx <- toStateT loc st
      let newSt = mergeStores [ctx, old, amb]
      f <- evalWith ex newSt
      case f of
        MacE x -> macToGlyph x
        _ -> eval f >>= toGlyph
    _ -> reportErr (locusOf m) UnevaluatedExpression




{-----------------}
{- :Eval:IntExpr -}
{-----------------}

instance Eval IntExpr where
  eval (IntConst n :@ loc) = return (IntConst n :@ loc)

  eval (IntVar key :@ loc) = lookupKey loc key >>= get >>= eval

  eval (IntAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [IntExpr] -> Integer -> Either ListErr IntExpr

  eval (IntAtIdx m h k :@ loc) = eAtIdx m h k loc

  eval (IntIfThenElse b t f :@ _) = eIfThenElse b t f

  eval (IntMacro vals mac :@ loc) = eMacro vals mac loc

  eval (IntNeg        a :@ loc) = lift1 loc (rNegT        zeroZZ) a
  eval (IntAbs        a :@ loc) = lift1 loc (rAbsT        zeroZZ) a
  eval (IntSqPart     a :@ loc) = lift1 loc (rSqPartT     zeroZZ) a
  eval (IntSqFreePart a :@ loc) = lift1 loc (rSqFreePartT zeroZZ) a
  eval (IntRad        a :@ loc) = lift1 loc (rRadT        zeroZZ) a

  eval (IntAdd  a b :@ loc) = lift2 loc (rAddT zeroZZ) a b
  eval (IntSub  a b :@ loc) = lift2 loc (rSubT zeroZZ) a b
  eval (IntMult a b :@ loc) = lift2 loc (rMulT zeroZZ) a b
  eval (IntMod  a b :@ loc) = lift2 loc (rRemT zeroZZ) a b
  eval (IntMin  a b :@ loc) = lift2 loc (rMinT zeroZZ) a b
  eval (IntMax  a b :@ loc) = lift2 loc (rMaxT zeroZZ) a b
  eval (IntGCD  a b :@ loc) = lift2 loc (rGCDT zeroZZ) a b
  eval (IntLCM  a b :@ loc) = lift2 loc (rLCMT zeroZZ) a b
  eval (IntQuo  a b :@ loc) = lift2 loc (rQuoT zeroZZ) a b
  eval (IntPow  a b :@ loc) = lift2 loc (rPowT zeroZZ) a b

  eval (IntChoose a b :@ loc) = lift2 loc (rChooseT zeroZZ) a b

  eval (RatNumer  p :@ loc) = lift1 loc (ratNum) p
  eval (RatDenom  p :@ loc) = lift1 loc (ratDen) p
  eval (RatFloor  p :@ loc) = lift1 loc (ratFlr) p
  eval (StrLength s :@ loc) = lift1 loc (strLen) s

  eval (MatNumRows m :@ loc) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    k <- tryEvalM loc $ mNumRows n
    return $ IntConst k :@ loc

  eval (MatNumCols m :@ loc) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    k <- tryEvalM loc $ mNumCols n
    return $ IntConst k :@ loc

  eval (IntRand ls :@ loc) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs
    return $ IntConst r :@ loc

  eval (ListLen ls :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    return $ IntConst (fromIntegral $ length xs) :@ loc

  eval (IntSum   ls :@ loc) = lift1 loc (rSumT   zeroZZ) ls
  eval (IntProd  ls :@ loc) = lift1 loc (rUProdT zeroZZ) ls
  eval (IntMaxim ls :@ loc) = lift1 loc (rMaximT zeroZZ) ls
  eval (IntMinim ls :@ loc) = lift1 loc (rMinimT zeroZZ) ls
  eval (IntGCDiv ls :@ loc) = lift1 loc (rGCDsT  zeroZZ) ls
  eval (IntLCMul ls :@ loc) = lift1 loc (rLCMsT  zeroZZ) ls

  eval (IntObserveUniform a b :@ loc) = do
    x  <- eval a >>= getVal
    y  <- eval b >>= getVal
    t <- observeIntegerUniform loc (x,y)
    return $ IntConst t :@ loc

  eval (IntObserveBinomial n p :@ loc) = do
    m <- eval n >>= getVal
    q <- eval p >>= getVal
    t <- observeBinomial loc m (toDouble q)
    return $ IntConst t :@ loc

  eval (IntObservePoisson lambda :@ loc) = do
    q <- eval lambda >>= getVal
    t <- observeIntegerPoisson loc (toDouble q)
    return $ IntConst t :@ loc

  eval (IntCastStr str :@ loc) = do
    Text x <- eval str >>= getVal
    n <- parseAsAt pInteger loc x
    return $ IntConst n :@ loc



{-----------------}
{- :Eval:StrExpr -}
{-----------------}

instance Eval StrExpr where
  eval (StrConst s :@ loc) = return (StrConst s :@ loc)

  eval (StrVar key :@ loc) = lookupKey loc key >>= get >>= eval

  eval (StrAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [StrExpr] -> Integer -> Either ListErr StrExpr

  eval (StrAtIdx m h k :@ loc) = eAtIdx m h k loc

  eval (StrIfThenElse b t f :@ _) = eIfThenElse b t f

  eval (StrMacro vals mac :@ loc) = eMacro vals mac loc

  eval (Concat   a b :@ loc) = lift2 loc (strCat)   a b
  eval (StrStrip a b :@ loc) = lift2 loc (strStrip) a b

  eval (ToUpper   a :@ loc) = lift1 loc (strUpper)  a
  eval (ToLower   a :@ loc) = lift1 loc (strLower)  a
  eval (Reverse   a :@ loc) = lift1 loc (strRev)    a
  eval (Rot13     a :@ loc) = lift1 loc (strRot13)  a
  eval (StrHex    n :@ loc) = lift1 loc (strHex)    n
  eval (StrRoman  n :@ loc) = lift1 loc (strRoman)  n
  eval (StrBase36 n :@ loc) = lift1 loc (strBase36) n

  eval (StrDecimal p k :@ loc) = do
    x <- eval p >>= getVal
    d <- eval k >>= getVal
    return $ StrConst (Text $ digits d x) :@ loc

  eval (StrRand ls :@ loc) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs
    return $ StrConst r :@ loc

  eval (StrTab m :@ loc) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    tab <- tabulateWithM toGlyph n
    return $ StrConst (Text tab) :@ loc

  eval (StrTypeOf e :@ loc) = do
    t <- typeOf e
    return $ StrConst (Text $ show t) :@ loc

  eval (StrFormat LaTeX e :@ loc) = do
    t <- typeOf e
    case t of
      ZZ -> do
        x <- eval e >>= getVal :: EvalM Integer
        return $ StrConst (Text $ latex x) :@ loc
      QQ -> do
        x <- eval e >>= getVal :: EvalM Rat
        return $ StrConst (Text $ latex x) :@ loc
      MatOf ZZ -> do
        x <- eval e >>= getVal :: EvalM (Matrix Integer)
        return $ StrConst (Text $ latex x) :@ loc
      MatOf QQ -> do
        x <- eval e >>= getVal :: EvalM (Matrix Rat)
        return $ StrConst (Text $ latex x) :@ loc
      PolyOver ZZ -> do
        x <- eval e >>= getVal :: EvalM (Poly Integer)
        return $ StrConst (Text $ latex x) :@ loc
      PolyOver QQ -> do
        x <- eval e >>= getVal :: EvalM (Poly Rat)
        return $ StrConst (Text $ latex x) :@ loc
      _ -> error "StrFormat LaTeX"

  eval (StrIntCast n :@ loc) = do
    a <- eval n >>= getVal :: EvalM IntExpr
    s <- toGlyph a
    return $ StrConst (Text s) :@ loc



{------------------}
{- :Eval:BoolExpr -}
{------------------}

instance Eval BoolExpr where
  eval (BoolConst b :@ loc) = return (BoolConst b :@ loc)

  eval (BoolVar key :@ loc) = lookupKey loc key >>= get >>= eval

  eval (BoolAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [BoolExpr] -> Integer -> Either ListErr BoolExpr

  eval (BoolAtIdx m h k :@ loc) = do
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    p <- eval m >>= getVal :: EvalM (Matrix Bool)
    x <- tryEvalM loc $ mEntryOf (i,j) p
    return $ BoolConst x :@ loc

  eval (BoolIfThenElse b t f :@ _) = eIfThenElse b t f

  eval (BoolMacro vals mac :@ loc) = eMacro vals mac loc

  eval (IsDefined key :@ loc) = do
    p <- isKeyDefined key
    return $ BoolConst p :@ loc

  eval (BoolEq a b :@ loc) = do
    x <- eval a >>= getVal :: EvalM Expr
    y <- eval b >>= getVal :: EvalM Expr
    return $ BoolConst (x == y) :@ loc

  eval (BoolNEq a b :@ loc) = do
    x <- eval a >>= getVal :: EvalM Expr
    y <- eval b >>= getVal :: EvalM Expr
    return $ BoolConst (x /= y) :@ loc

  eval (BoolLT a b :@ loc) = do
    ta <- typeOf a
    tb <- typeOf b
    case unify ta tb of
      Right ZZ -> do
        x <- eval a >>= getVal :: EvalM Integer
        y <- eval b >>= getVal :: EvalM Integer
        return $ BoolConst (x < y) :@ loc
      Right SS -> do
        x <- eval a >>= getVal :: EvalM Text
        y <- eval b >>= getVal :: EvalM Text
        return $ BoolConst (x < y) :@ loc
      Right QQ -> do
        x <- eval a >>= getVal :: EvalM Rat
        y <- eval b >>= getVal :: EvalM Rat
        return $ BoolConst (x < y) :@ loc
      Right u -> reportErr loc $ SortableExpected u
      Left err -> reportErr loc err

  eval (BoolLEq a b :@ loc) = do
    ta <- typeOf a
    tb <- typeOf b
    case unify ta tb of
      Right ZZ -> do
        x <- eval a >>= getVal :: EvalM Integer
        y <- eval b >>= getVal :: EvalM Integer
        return $ BoolConst (x <= y) :@ loc
      Right SS -> do
        x <- eval a >>= getVal :: EvalM Text
        y <- eval b >>= getVal :: EvalM Text
        return $ BoolConst (x <= y) :@ loc
      Right QQ -> do
        x <- eval a >>= getVal :: EvalM Rat
        y <- eval b >>= getVal :: EvalM Rat
        return $ BoolConst (x <= y) :@ loc
      Right u -> reportErr loc $ SortableExpected u
      Left err -> reportErr loc err

  eval (BoolGT a b :@ loc) = do
    ta <- typeOf a
    tb <- typeOf b
    case unify ta tb of
      Right ZZ -> do
        x <- eval a >>= getVal :: EvalM Integer
        y <- eval b >>= getVal :: EvalM Integer
        return $ BoolConst (x > y) :@ loc
      Right SS -> do
        x <- eval a >>= getVal :: EvalM Text
        y <- eval b >>= getVal :: EvalM Text
        return $ BoolConst (x > y) :@ loc
      Right QQ -> do
        x <- eval a >>= getVal :: EvalM Rat
        y <- eval b >>= getVal :: EvalM Rat
        return $ BoolConst (x > y) :@ loc
      Right u -> reportErr loc $ SortableExpected u
      Left err -> reportErr loc err

  eval (BoolGEq a b :@ loc) = do
    ta <- typeOf a
    tb <- typeOf b
    case unify ta tb of
      Right ZZ -> do
        x <- eval a >>= getVal :: EvalM Integer
        y <- eval b >>= getVal :: EvalM Integer
        return $ BoolConst (x >= y) :@ loc
      Right SS -> do
        x <- eval a >>= getVal :: EvalM Text
        y <- eval b >>= getVal :: EvalM Text
        return $ BoolConst (x >= y) :@ loc
      Right QQ -> do
        x <- eval a >>= getVal :: EvalM Rat
        y <- eval b >>= getVal :: EvalM Rat
        return $ BoolConst (x >= y) :@ loc
      Right u -> reportErr loc $ SortableExpected u
      Left err -> reportErr loc err

  eval (BoolRand ls :@ loc) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs
    return $ BoolConst r :@ loc

  eval (ListElem x xs :@ loc) = do
    a <- eval x >>= getVal :: EvalM Expr
    as <- eval xs >>= getVal :: EvalM [Expr]
    return $ BoolConst (elem a as) :@ loc

  eval (ListIsEmpty xs :@ loc) = do
    as <- eval xs >>= getVal :: EvalM [Expr]
    return $ BoolConst (null as) :@ loc

  eval (MatIsRow m :@ loc) = do
    p <- eval m >>= getVal :: EvalM (Matrix Expr)
    q <- tryEvalM loc $ mIsRow p
    return $ BoolConst q :@ loc

  eval (MatIsCol m :@ loc) = do
    p <- eval m >>= getVal :: EvalM (Matrix Expr)
    q <- tryEvalM loc $ mIsCol p
    return $ BoolConst q :@ loc

  eval (MatIsGJForm m :@ loc) = do
    t <- typeOf m
    case t of
      MatOf QQ -> do
        p <- eval m >>= getVal :: EvalM (Matrix Rat)
        q <- tryEvalM loc $ mIsGaussJordanForm p
        return $ BoolConst q :@ loc
      MatOf BB -> do
        p <- eval m >>= getVal :: EvalM (Matrix Bool)
        q <- tryEvalM loc $ mIsGaussJordanForm p
        return $ BoolConst q :@ loc
      _ -> reportErr loc $ NumericMatrixExpected t

  -- Bool
  eval (Neg    a   :@ loc) = lift1 loc (boolNot) a
  eval (Conj   a b :@ loc) = lift2 loc (boolAnd) a b
  eval (Disj   a b :@ loc) = lift2 loc (boolOr)  a b
  eval (Imp    a b :@ loc) = lift2 loc (boolImp) a b

  -- Int
  eval (IntSqFree a :@ loc) = lift1 loc (rIsSqFreeT zeroZZ) a
  eval (IntDiv a b :@ loc)  = lift2 loc (rDividesT  zeroZZ) a b

  -- Str
  eval (Matches a b :@ loc) = lift2 loc (strMatch) a b



{-----------------}
{- :Eval:RatExpr -}
{-----------------}

instance Eval RatExpr where
  eval (RatConst p :@ loc) = return $ RatConst p :@ loc

  eval (RatVar key :@ loc) = lookupKey loc key >>= get >>= eval

  eval (RatAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [RatExpr] -> Integer -> Either ListErr RatExpr

  eval (RatAtIdx m h k :@ loc) = do
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    p <- eval m >>= getVal :: EvalM (Matrix Rat)
    x <- tryEvalM loc $ mEntryOf (i,j) p
    return $ RatConst x :@ loc

  eval (RatCast expr :@ loc) = do
    n <- eval expr >>= getVal :: EvalM Integer
    return $ RatConst (n:/:1) :@ loc

  eval (RatIfThenElse b t f :@ _) = eIfThenElse b t f

  eval (RatMacro vals mac :@ loc) = eMacro vals mac loc

  eval (RatNeg  a :@ loc)   = lift1 loc (rNegT (0:/:1)) a
  eval (RatAbs  a :@ loc)   = lift1 loc (rAbsT (0:/:1)) a

  eval (RatAdd  a b :@ loc) = lift2 loc (rAddT (0:/:1)) a b
  eval (RatSub  a b :@ loc) = lift2 loc (rSubT (0:/:1)) a b
  eval (RatMult a b :@ loc) = lift2 loc (rMulT (0:/:1)) a b
  eval (RatMin  a b :@ loc) = lift2 loc (rMinT (0:/:1)) a b
  eval (RatMax  a b :@ loc) = lift2 loc (rMaxT (0:/:1)) a b
  eval (RatPow  a b :@ loc) = lift2 loc (rPowT (0:/:1)) a b
  eval (RatQuot a b :@ loc) = lift2 loc (rDivT (0:/:1)) a b

  eval (RatSqrt p k :@ loc) = lift2 loc (ratSqt) p k

  eval (RatRand ls :@ loc) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs
    return $ RatConst r :@ loc

  eval (RatSum   ls :@ loc) = lift1 loc (rSumT   (0:/:1)) ls
  eval (RatProd  ls :@ loc) = lift1 loc (rUProdT (0:/:1)) ls
  eval (RatMaxim ls :@ loc) = lift1 loc (rMaximT (0:/:1)) ls
  eval (RatMinim ls :@ loc) = lift1 loc (rMinimT (0:/:1)) ls

  {- Mean -}
  eval (RatMean ls :@ loc) = do
    t <- typeOf ls
    case t of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ rIntMeanT (0:/:1) xs
        return $ RatConst m :@ loc
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ rMeanT (0:/:1) xs
        return $ RatConst m :@ loc
      u -> reportErr loc $ NumericListExpected u

  {- Mean Deviation -}
  eval (RatMeanDev ls :@ loc) = do
    t <- typeOf ls
    case t of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ rIntMeanDevT (0:/:1) xs
        return $ RatConst m :@ loc
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ rMeanDevT (0:/:1) xs
        return $ RatConst m :@ loc
      u -> reportErr loc $ NumericListExpected u

  {- Standard Deviation -}
  eval (RatStdDev ls d :@ loc) = do
    t <- typeOf ls
    k <- eval d >>= getVal
    case t of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ ratIntStdDev xs k
        return $ RatConst m :@ loc
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ ratStdDev xs k
        return $ RatConst m :@ loc
      u -> reportErr loc $ NumericListExpected u

  {- Z-Score -}
  eval (RatZScore x ls d :@ loc) = do
    t <- typeOf ls
    k <- eval d >>= getVal
    y <- eval x >>= getVal
    case t of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        m  <- tryEvalM loc $ ratIntZScore y xs k
        return $ RatConst m :@ loc
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        m  <- tryEvalM loc $ ratZScore y xs k
        return $ RatConst m :@ loc
      u -> reportErr loc $ NumericListExpected u

  eval (RatCastStr str :@ loc) = do
    Text x <- eval str >>= getVal
    n <- parseAsAt pRat loc x
    return $ RatConst n :@ loc



{------------------}
{- :Eval:ListExpr -}
{------------------}

instance Eval ListExpr where
  eval (ListConst t xs :@ loc) = do
    ys <- sequence $ map eval xs
    return $ ListConst t ys :@ loc

  eval (ListVar key :@ loc) = lookupKey loc key >>= getVal >>= eval

  eval (ListAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [ListExpr] -> Integer -> Either ListErr ListExpr

  eval (ListAtIdx m h k :@ loc) = eAtIdx m h k loc

  eval (ListMacro vals mac :@ loc) = eMacro vals mac loc

  eval (ListIfThenElse b t f :@ _) = eIfThenElse b t f

  eval (ListRange a b :@ loc) = do
    x <- eval a >>= getVal :: EvalM Integer
    y <- eval b >>= getVal :: EvalM Integer
    return $ ListConst ZZ [IntE $ IntConst k :@ loc | k <- [x..y]] :@ loc

  eval (ListCat a b :@ loc) = do
    t <- unifyTypesOf loc a b
    case t of
      ListOf u -> do
        xs <- eval a >>= getVal :: EvalM [Expr]
        ys <- eval b >>= getVal :: EvalM [Expr]
        return $ ListConst u (xs ++ ys) :@ loc
      _ -> reportErr loc $ ListExpected t

  eval (ListToss a b :@ loc) = do
    t <- unifyTypesOf loc a b
    case t of
      ListOf u -> do
        xs <- eval a >>= getVal :: EvalM [Expr]
        ys <- eval b >>= getVal :: EvalM [Expr]
        return $ ListConst u (xs \\ ys) :@ loc
      _ -> reportErr loc $ ListExpected t

  eval (ListRev a :@ loc) = do
    t <- typeOf a
    xs <- eval a >>= getVal :: EvalM [Expr]
    case t of
      ListOf u -> return $ ListConst u (reverse xs) :@ loc
      _ -> reportErr loc $ ListExpected t

  eval (ListSort a :@ loc) = do
    t <- typeOf a
    case t of
      ListOf SS -> do
        xs <- eval a >>= getVal :: EvalM [Text]
        return $ ListConst SS (map (\k -> StrE $ StrConst k :@ loc) (sort xs)) :@ loc
      ListOf ZZ -> do
        xs <- eval a >>= getVal :: EvalM [Integer]
        return $ ListConst ZZ (map (\k -> IntE $ IntConst k :@ loc) (sort xs)) :@ loc
      ListOf QQ -> do
        xs <- eval a >>= getVal :: EvalM [Rat]
        return $ ListConst QQ (map (\k -> RatE $ RatConst k :@ loc) (sort xs)) :@ loc
      ListOf BB -> do
        xs <- eval a >>= getVal :: EvalM [Bool]
        return $ ListConst BB (map (\k -> BoolE $ BoolConst k :@ loc) (sort xs)) :@ loc
      _ -> reportErr loc $ SortableListExpected t

  eval (ListRand ls :@ loc) = do
    t  <- typeOf ls
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    s  <- eval r >>= getVal :: EvalM ListExpr
    case t of
      ListOf (ListOf _) -> return s
      _ -> reportErr loc $ ListExpected t

  eval (ListUniq a :@ loc) = do
    t <- typeOf a
    xs <- eval a >>= getVal :: EvalM [Expr]
    case t of
      ListOf u -> return $ ListConst u (nub xs) :@ loc
      _ -> reportErr loc $ ListExpected t 

  eval (ListShuffle ls :@ loc) = do
    t <- typeOf ls
    case t of
      ListOf u -> do
        xs <- eval ls >>= getVal :: EvalM [Expr]
        ys <- shuffleEvalM xs
        return $ ListConst u ys :@ loc
      u -> reportErr loc $ ListExpected u

  eval (ListShuffles ls :@ loc) = do
    t <- typeOf ls
    case t of
      ListOf u -> do
        xs <- eval ls >>= getVal :: EvalM [Expr]
        let us = [ListConst (ListOf u) ys :@ loc | ys <- permutations xs]
        return (ListConst (ListOf u) (map toExpr us) :@ loc)
      u -> reportErr loc $ ListExpected u

  eval (ListChoose n ls :@ loc) = do
    k <- eval n >>= getVal :: EvalM Integer
    t <- typeOf ls
    case t of
      ListOf u -> do
        xs <- eval ls >>= getVal :: EvalM [Expr]
        ys <- sampleEvalM (fromIntegral k) xs
        return $ ListConst u ys :@ loc
      u -> reportErr loc $ ListExpected u

  eval (ListChoices n ls :@ loc) = do
    k <- eval n >>= getVal :: EvalM Integer
    ListOf t <- typeOf ls
    xs <- eval ls >>= getVal :: EvalM [Expr]
    let foos = [toExpr $ ListConst t x :@ loc | x <- combinations (fromIntegral k) xs]
    return $ ListConst (ListOf t) foos :@ loc 

  eval (ListBuilder e gs :@ loc) = do
    st <- getState
    xs <- bar st gs
    ys <- sequence [evalWith e x >>= getVal | x <- xs]
    t <- case ys of
           [] -> return XX
           (z:_) -> typeOf z
    eval $ ListConst t ys :@ loc
      where
        bar :: Store Expr -> [ListGuard] -> EvalM [Store Expr]
        bar st []     = return [st]
        bar st (h:hs) = do
          xs <- foo st h
          fmap concat $ sequence $ [bar x hs | x <- xs]
        
        foo :: Store Expr -> ListGuard -> EvalM [Store Expr]
        foo st (Bind key ls) = do
          xs <- eval ls >>= getVal :: EvalM [Expr]
          sequence [addKeyToStore key x (locusOf x) st | x <- xs]
        
        foo st (Guard p) = do
          x <- evalWith p st >>= getVal :: EvalM Bool
          if x == True
            then return [st]
            else return []

  eval (ListFilter k g xs :@ loc) = do
    ListOf t <- typeOf xs
    ys <- eval xs >>= getVal :: EvalM [Expr]
    let foo e = do
          defineKey k e loc
          x <- eval g >>= getVal :: EvalM Bool
          undefineKey k
          return x
    zs <- filterM foo ys
    return (ListConst t zs :@ loc)

  eval (ListMatRow k m :@ loc) = do
    u  <- expectMatrix loc m
    i  <- eval k >>= getVal :: EvalM Integer
    n  <- eval m >>= getVal :: EvalM (Matrix Expr)
    as <- tryEvalM loc $ mListRowOf i n
    return (ListConst u as :@ loc)

  eval (ListMatCol k m :@ loc) = do
    u  <- expectMatrix loc m
    i  <- eval k >>= getVal :: EvalM Integer
    n  <- eval m >>= getVal :: EvalM (Matrix Expr)
    as <- tryEvalM loc $ mListColOf i n
    return (ListConst u as :@ loc)

  eval (ListPermsOf t xs :@ loc) = do
    case t of
      PermOf ZZ -> do
        as <- eval xs >>= getVal :: EvalM [Integer]
        qs <- tryEvalM loc $ permsOf as
        let us = map (inject loc) qs :: [PermExpr]
        return (ListConst (PermOf ZZ) (map toExpr us) :@ loc)
      PermOf SS -> do
        as <- eval xs >>= getVal :: EvalM [Text]
        qs <- tryEvalM loc $ permsOf as
        let us = map (inject loc) qs :: [PermExpr]
        return (ListConst (PermOf SS) (map toExpr us) :@ loc)
      PermOf QQ -> do
        as <- eval xs >>= getVal :: EvalM [Rat]
        qs <- tryEvalM loc $ permsOf as
        let us = map (inject loc) qs :: [PermExpr]
        return (ListConst (PermOf SS) (map toExpr us) :@ loc)
      u -> reportErr loc $ PermutationExpected u



{-----------------}
{- :Eval:MacExpr -}
{-----------------}

instance Eval MacExpr where
  eval (MacConst typ vals expr (amb,p) :@ loc) = do
    if p == True
      then return $ MacConst typ vals expr (amb,True) :@ loc
      else do
        st <- getState
        return $ MacConst typ vals expr (st,True) :@ loc


  eval (MacVar key :@ loc) = lookupKey loc key >>= getVal >>= eval

  eval (MacIfThenElse b t f :@ _) = eIfThenElse b t f

  eval (MacAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [MacExpr] -> Integer -> Either ListErr MacExpr

  eval (MacAtIdx m h k :@ loc) = do
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    p <- eval m >>= getVal :: EvalM (Matrix MacExpr)
    tryEvalM loc $ mEntryOf (i,j) p

  eval (MacMacro vals mac :@ loc) = eMacro vals mac loc

  eval (MacRand ls :@ _) = do
    xs <- eval ls >>= getVal
    randomElementEvalM xs



{-----------------}
{- :Eval:MatExpr -}
{-----------------}

instance Eval MatExpr where
  eval (MatConst t m :@ loc) = do
    n <- mSeq $ fmap eval m
    return $ MatConst t n :@ loc

  eval (MatVar key :@ loc) = lookupKey loc key >>= getVal >>= eval

  eval (MatAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [MatExpr] -> Integer -> Either ListErr MatExpr

  eval (MatAtIdx m h k :@ loc) = eAtIdx m h k loc

  eval (MatMacro vals mac :@ loc) = eMacro vals mac loc

  eval (MatIfThenElse b t f :@ _) = eIfThenElse b t f

  eval (MatRand ls :@ loc) = do
    t  <- typeOf ls
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    s  <- eval r >>= getVal :: EvalM MatExpr
    case t of
      ListOf (MatOf _) -> return s
      _ -> reportErr loc $ ListExpected t

  eval (MatRowFromList t xs :@ loc) = do
    as <- eval xs >>= getVal :: EvalM [Expr]
    m  <- tryEvalM loc $ mRowFromList as
    return $ MatConst t m :@ loc

  eval (MatColFromList t xs :@ loc) = do
    as <- eval xs >>= getVal :: EvalM [Expr]
    m  <- tryEvalM loc $ mColFromList as
    return $ MatConst t m :@ loc

  eval (MatId t n :@ loc) = do
    k <- eval n >>= getVal :: EvalM Integer
    let makeIdMat a = do
          x <- tryEvalM loc $ mEIdT a k
          return $ inject loc x
    case t of
      ZZ -> makeIdMat (0::Integer)
      QQ -> makeIdMat (0:/:1)
      BB -> makeIdMat False
      _  -> reportErr loc $ NumericMatrixExpected t

  eval (MatSwapE t n h k :@ loc) = do
    m <- eval n >>= getVal
    i <- eval h >>= getVal
    j <- eval k >>= getVal
    let makeSwapMat a = do
          x <- tryEvalM loc $ mESwapT a m i j
          return $ inject loc x
    case t of
      ZZ -> makeSwapMat zeroZZ
      QQ -> makeSwapMat zeroQQ
      BB -> makeSwapMat zeroBB
      ZZMod d -> makeSwapMat (zeroMod d)
      PolyOver ZZ -> makeSwapMat (constP zeroZZ)
      _  -> reportErr loc $ NumericMatrixExpected t

  eval (MatScaleE t n h x :@ loc) = do
    m <- eval n >>= getVal
    k <- eval h >>= getVal
    let makeScaleMat a = do
          w <- eval x >>= getVal
          y <- tryEvalM loc $ mEScaleT a m k w
          return $ inject loc y
    case t of
      ZZ -> makeScaleMat zeroZZ
      QQ -> makeScaleMat zeroQQ
      BB -> makeScaleMat zeroBB
      ZZMod d -> makeScaleMat (zeroMod d)
      PolyOver ZZ -> makeScaleMat (constP zeroZZ)
      _  -> reportErr loc $ NumericMatrixExpected t

  eval (MatAddE t n h k x :@ loc) = do
    m <- eval n >>= getVal
    i <- eval h >>= getVal
    j <- eval k >>= getVal
    let makeAddMat a = do
          w <- eval x >>= getVal
          y <- tryEvalM loc $ mEAddT a m (i,j) w
          return $ inject loc y
    case t of
      ZZ -> makeAddMat zeroZZ
      QQ -> makeAddMat zeroQQ
      BB -> makeAddMat zeroBB
      ZZMod d -> makeAddMat (0`zzmod`d)
      PolyOver ZZ -> makeAddMat (constP zeroZZ)
      PolyOver QQ -> makeAddMat (constP zeroQQ)
      PolyOver BB -> makeAddMat (constP zeroBB)
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatShuffleRows m :@ loc) = do
    u  <- expectMatrix loc m
    x  <- eval m >>= getVal :: EvalM (Matrix Expr)
    rs <- tryEvalM loc $ mRowsOf x
    ts <- shuffleEvalM rs
    n  <- tryEvalM loc $ mVCats ts
    return $ MatConst u n :@ loc

  eval (MatShuffleCols m :@ loc) = do
    u  <- expectMatrix loc m
    x  <- eval m >>= getVal :: EvalM (Matrix Expr)
    rs <- tryEvalM loc $ mColsOf x
    ts <- shuffleEvalM rs
    n  <- tryEvalM loc $ mHCats ts
    return $ MatConst u n :@ loc

  eval (MatHCat a b :@ loc) = do
    t <- unifyTypesOf loc a b
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    n <- eval b >>= getVal :: EvalM (Matrix Expr)
    x <- tryEvalM loc $ mHCat m n
    case t of
      MatOf u -> return $ MatConst u x :@ loc
      _ -> reportErr loc $ MatrixExpected t

  eval (MatVCat a b :@ loc) = do
    t <- unifyTypesOf loc a b
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    n <- eval b >>= getVal :: EvalM (Matrix Expr)
    x <- tryEvalM loc $ mVCat m n
    case t of
      MatOf u -> return $ MatConst u x :@ loc
      _ -> reportErr loc $ MatrixExpected t

  eval (MatAdd a b :@ loc) = do
    t <- unifyTypesOf loc a b
    case t of
      MatOf ZZ -> lift2 loc (rAddT (mCell zeroZZ)) a b
      MatOf QQ -> lift2 loc (rAddT (mCell zeroQQ)) a b
      MatOf BB -> lift2 loc (rAddT (mCell zeroBB)) a b
      MatOf (ZZMod n) -> lift2 loc (rAddT (mCell (zeroMod n))) a b
      MatOf (PolyOver ZZ) -> lift2 loc (rAddT (mCell $ constP zeroZZ)) a b
      MatOf (PolyOver QQ) -> lift2 loc (rAddT (mCell $ constP zeroQQ)) a b
      MatOf (PolyOver BB) -> lift2 loc (rAddT (mCell $ constP zeroBB)) a b
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatNeg a :@ loc) = do
    t <- typeOf a
    case t of
      MatOf ZZ -> lift1 loc (rNegT (mCell zeroZZ)) a
      MatOf QQ -> lift1 loc (rNegT (mCell zeroQQ)) a
      MatOf BB -> lift1 loc (rNegT (mCell zeroBB)) a
      MatOf (ZZMod n) -> lift1 loc (rNegT (mCell (0`zzmod`n))) a
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatTrans a :@ loc) = do
    u <- expectMatrix loc a
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    p <- tryEvalM loc $ mTranspose m
    return $ MatConst u p :@ loc

  eval (MatMul a b :@ loc) = do
    t <- unifyTypesOf loc a b
    case t of
      MatOf ZZ -> lift2 loc (rMulT (mCell (0::Integer))) a b
      MatOf QQ -> lift2 loc (rMulT (mCell (0:/:1))) a b
      MatOf BB -> lift2 loc (rMulT (mCell False)) a b
      MatOf (ZZMod n) -> lift2 loc (rMulT (mCell (0`zzmod`n))) a b
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatPow m n :@ loc) = do
    t <- typeOf m
    k <- eval n >>= getVal :: EvalM Integer
    let makeMatPow a = do
          q <- eval m >>= getVal
          p <- tryEvalM loc $ rPosPowT a q k
          return $ inject loc p
    case t of
      MatOf ZZ -> makeMatPow (mCell (0::Integer))
      MatOf QQ -> makeMatPow (mCell (0:/:1))
      MatOf BB -> makeMatPow (mCell (False))
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatSwapRows m a b :@ loc) = do
    u <- expectMatrix loc m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    j <- eval b >>= getVal
    p <- tryEvalM loc $ mSwapRows i j n
    return $ MatConst u p :@ loc

  eval (MatSwapCols m a b :@ loc) = do
    u <- expectMatrix loc m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    j <- eval b >>= getVal
    p <- tryEvalM loc $ mSwapCols i j n
    return $ MatConst u p :@ loc

  eval (MatScaleRow m a h :@ loc) = do
    t <- typeOf m
    u <- typeOf a
    i <- eval h >>= getVal :: EvalM Integer
    case unify t (MatOf u) of
      Right (MatOf ZZ) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Integer)
        r <- eval a >>= getVal :: EvalM Integer
        p <- tryEvalM loc $ mScaleRow r i n
        return $ inject loc p
      Right (MatOf QQ) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        r <- eval a >>= getVal :: EvalM Rat
        p <- tryEvalM loc $ mScaleRow r i n
        return $ inject loc p
      Right (MatOf BB) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        r <- eval a >>= getVal :: EvalM Bool
        p <- tryEvalM loc $ mScaleRow r i n
        return $ inject loc p
      Right w -> reportErr loc $ NumericMatrixExpected w
      Left err -> reportErr loc err

  eval (MatScaleCol m a h :@ loc) = do
    t <- typeOf m
    u <- typeOf a
    i <- eval h >>= getVal :: EvalM Integer
    case unify t (MatOf u) of
      Right (MatOf ZZ) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Integer)
        r <- eval a >>= getVal :: EvalM Integer
        p <- tryEvalM loc $ mScaleCol r i n
        return $ inject loc p
      Right (MatOf QQ) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        r <- eval a >>= getVal :: EvalM Rat
        p <- tryEvalM loc $ mScaleCol r i n
        return $ inject loc p
      Right (MatOf BB) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        r <- eval a >>= getVal :: EvalM Bool
        p <- tryEvalM loc $ mScaleCol r i n
        return $ inject loc p
      Right w -> reportErr loc $ NumericMatrixExpected w
      Left err -> reportErr loc err

  eval (MatAddRow m a h k :@ loc) = do
    t <- typeOf m
    u <- typeOf a
    i <- eval h >>= getVal
    j <- eval k >>= getVal
    let makeAddRow zer = do
          n <- eval m >>= getVal
          r <- eval a >>= getVal
          suchThat $ r `hasSameTypeAs` zer
          p <- tryEvalM loc $ mAddRow r i j n
          return $ inject loc p
    case unify t (MatOf u) of
      Right (MatOf ZZ) -> makeAddRow zeroZZ
      Right (MatOf QQ) -> makeAddRow zeroQQ
      Right (MatOf BB) -> makeAddRow zeroBB
      Right (MatOf (ZZMod n)) -> makeAddRow (zeroMod n)
      Right w  -> reportErr loc $ NumericMatrixExpected w
      Left err -> reportErr loc err

  eval (MatAddCol m a h k :@ loc) = do
    t <- typeOf m
    u <- typeOf a
    i <- eval h >>= getVal
    j <- eval k >>= getVal
    let makeAddCol zer = do
          n <- eval m >>= getVal
          r <- eval a >>= getVal
          suchThat $ r `hasSameTypeAs` zer
          p <- tryEvalM loc $ mAddCol r i j n
          return $ inject loc p
    case unify t (MatOf u) of
      Right (MatOf ZZ) -> makeAddCol zeroZZ
      Right (MatOf QQ) -> makeAddCol zeroQQ
      Right (MatOf BB) -> makeAddCol zeroBB
      Right (MatOf (ZZMod n)) -> makeAddCol (zeroMod n)
      Right w -> reportErr loc $ NumericMatrixExpected w
      Left err -> reportErr loc err

  eval (MatDelRow m a :@ loc) = do
    u <- expectMatrix loc m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    p <- tryEvalM loc $ mDelRow n i
    return $ MatConst u p :@ loc

  eval (MatDelCol m a :@ loc) = do
    u <- expectMatrix loc m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    p <- tryEvalM loc $ mDelCol n i
    return $ MatConst u p :@ loc

  eval (MatGJForm m :@ loc) = do
    t <- typeOf m
    case t of
      MatOf QQ -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        p <- tryEvalM loc $ mGJForm n
        return $ inject loc p
      MatOf BB -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        p <- tryEvalM loc $ mGJForm n
        return $ inject loc p
      _ -> reportErr loc $ FieldMatrixExpected t

  eval (MatGJFactor m :@ loc) = do
    t <- typeOf m
    case t of
      MatOf QQ -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        p <- tryEvalM loc $ mGJFactor n
        return $ inject loc p
      MatOf BB -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        p <- tryEvalM loc $ mGJFactor n
        return $ inject loc p
      _ -> reportErr loc $ FieldMatrixExpected t

  eval (MatGetRow k m :@ loc) = do
    u <- expectMatrix loc m
    i <- eval k >>= getVal :: EvalM Integer
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    r <- tryEvalM loc $ mRowOf i n
    return (MatConst u r :@ loc)

  eval (MatGetCol k m :@ loc) = do
    u <- expectMatrix loc m
    i <- eval k >>= getVal :: EvalM Integer
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    c <- tryEvalM loc $ mColOf i n
    return (MatConst u c :@ loc)

  eval (MatBuilder typ e kr lr kc lc :@ loc) = do
    st <- getState
    rs <- eval lr >>= getVal :: EvalM [Expr]
    cs <- eval lc >>= getVal :: EvalM [Expr]
    es <- sequence [sequence [foo st r c | c <- cs] | r <- rs]
    m  <- tryEvalM loc $ mFromRowList es
    return (MatConst typ m :@ loc)
      where
        foo st r c = do
          st' <- addKeyToStore kr r loc st >>= addKeyToStore kc c loc
          evalWith e st' >>= getVal



{-------------}
{- :Eval:Doc -}
{-------------}

instance Eval Doc where
  eval (Empty :@ loc)     = return (Empty :@ loc)
  eval (DocText s :@ loc) = return (DocText s :@ loc)
  eval (Escaped c :@ loc) = return (DocText (Text [c]) :@ loc)

  eval (ShowState :@ loc) = do
    st <- getState
    return $ DocText (Text $ show st) :@ loc

  eval (NakedKey k :@ loc) = do
    expr <- lookupKey loc k
    let foo s = return (DocText (Text s) :@ loc)
    case expr of
      DocE   x -> eval x
      IntE   x -> eval x >>= toGlyph >>= foo
      StrE   x -> eval x >>= toGlyph >>= foo
      BoolE  x -> eval x >>= toGlyph >>= foo
      RatE   x -> eval x >>= toGlyph >>= foo
      ListE  x -> eval x >>= toGlyph >>= foo
      MacE   x -> eval x >>= macToGlyph >>= foo
      MatE   x -> eval x >>= toGlyph >>= foo
      PolyE  x -> eval x >>= toGlyph >>= foo
      PermE  x -> eval x >>= toGlyph >>= foo
      ZZModE x -> eval x >>= toGlyph >>= foo

  eval (DocMacro vals mac :@ loc) = eMacro vals mac loc

  eval (Scope body :@ _) = do
    --pushTrace "scope" loc
    current <- getState
    result <- evalWith body current
    --popTrace
    return result

  eval (IfThenElse b true false :@ _) = do
    --pushTrace "if-then-else" loc
    x <- eval b >>= getVal
    result <- case x of
                True  -> eval true
                False -> eval false
    --popTrace
    return result

  eval (NakedExpr expr :@ loc) = case expr of
    IntE   e -> eval e >>= toGlyph >>= \x -> return (DocText (Text x) :@ loc)
    StrE   e -> eval e >>= toGlyph >>= \x -> return (DocText (Text x) :@ loc)
    RatE   e -> eval e >>= toGlyph >>= \x -> return (DocText (Text x) :@ loc)
    BoolE  e -> eval e >>= toGlyph >>= \x -> return (DocText (Text x) :@ loc)
    ListE  e -> eval e >>= toGlyph >>= \x -> return (DocText (Text x) :@ loc)
    MatE   e -> eval e >>= toGlyph >>= \x -> return (DocText (Text x) :@ loc)
    MacE   e -> eval e >>= macToGlyph >>= \x -> return (DocText (Text x) :@ loc)
    DocE   e -> eval e >>= toGlyph >>= \x -> return (DocText (Text x) :@ loc)
    PolyE  e -> eval e >>= toGlyph >>= \x -> return (DocText (Text x) :@ loc)
    PermE  e -> eval e >>= toGlyph >>= \x -> return (DocText (Text x) :@ loc)
    ZZModE e -> eval e >>= toGlyph >>= \x -> return (DocText (Text x) :@ loc)

  eval (Import file Nothing rest :@ _) = do
    oldSt <- getState
    clearState
    _ <- readAndParseDocFromLib file >>= eval
    newSt <- getState
    putState oldSt
    mergeState' newSt
    eval rest

  eval (Import file (Just prefix) rest :@ _) = do
    oldSt <- getState
    clearState
    _ <- readAndParseDocFromLib file >>= eval
    newSt <- getState
    putState oldSt
    mergeState' (qualify prefix newSt)
    eval rest

  eval (Cat [] :@ loc) = return $ Empty :@ loc
  eval (Cat ts :@ loc) = do
    exprs <- sequence [eval t >>= getVal | t <- ts]
    return $ DocText (concatText exprs) :@ loc

  eval (CatPar [] :@ loc) = return $ Empty :@ loc
  eval (CatPar ts :@ loc) = do
    let foo x = do
          st <- getState
          y <- eval x >>= getVal
          putState st
          return y
    exprs <- sequence $ map foo ts
    return $ DocText (concatText exprs) :@ loc


  eval (Cond [] defa :@ _) = do
    --pushTrace "cond" loc
    result <- eval defa
    --popTrace
    return result

  eval (Cond ((c,t):ds) defa :@ loc) = do
    x <- eval c >>= getVal
    if x
      then eval t
      else eval $ Cond ds defa :@ loc


  eval (Define t k v rest :@ loc) = do
    w <- eval v
    tw <- typeOf w
    if t == tw
      then do
        defineKey k w loc
        eval rest
      else reportErr loc $ TypeMismatch t tw


  eval (LetIn key val expr :@ loc) = do
    defineKey key val loc
    t <- eval expr
    undefineKey key
    return t

  eval (Bail s :@ loc) = (eval s) >>= getVal >>= (reportErr loc . BailMessage . unText)

  eval (Alt [] :@ loc) = return $ Empty :@ loc
  eval (Alt ts :@ _)   = randomElementEvalM ts >>= eval

  eval (Shuffle xs :@ loc) = do
    x <- shuffleEvalM xs
    eval $ Cat x :@ loc


  eval (ForSay k ls body Nothing :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    eval $ CatPar [LetIn k x body :@ loc | x <- xs] :@ loc

  eval (ForSay k ls body (Just sep) :@ loc) = do
    xs <- eval ls  >>= getVal
    b  <- eval sep >>= getVal
    eval $ CatPar (intersperse b [LetIn k x body :@ loc | x <- xs]) :@ loc


  eval (Select k ls body :@ loc) = do
    xs <- eval ls >>= getVal
    x  <- randomElementEvalM xs
    eval $ LetIn k x body :@ loc



{------------------}
{- :Eval:PolyExpr -}
{------------------}

instance Eval PolyExpr where
  eval (PolyConst t p :@ loc) = do
    q <- polySeq $ fmap eval p
    return $ PolyConst t q :@ loc

  eval (PolyVar key :@ loc) = lookupKey loc key >>= getVal >>= eval

  eval (PolyAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [PolyExpr] -> Integer -> Either ListErr PolyExpr

  eval (PolyAtIdx m h k :@ loc) = eAtIdx m h k loc

  eval (PolyMacro vals mac :@ loc) = eMacro vals mac loc

  eval (PolyIfThenElse b t f :@ _) = eIfThenElse b t f

  eval (PolyRand ls :@ loc) = do
    t  <- typeOf ls
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    s  <- eval r >>= getVal :: EvalM PolyExpr
    case t of
      ListOf (PolyOver _) -> return s
      _ -> reportErr loc $ ListExpected t

  eval (PolyAdd a b :@ loc) = do
    t <- unifyTypesOf loc a b
    case t of
      PolyOver ZZ ->
        lift2 loc (rAddT (constP (0::Integer))) a b
      PolyOver QQ ->
        lift2 loc (rAddT (constP (0:/:1))) a b
      PolyOver BB ->
        lift2 loc (rAddT (constP False)) a b
      PolyOver (ZZMod n) -> do
        x <- eval a >>= getVal :: EvalM (Poly ZZModulo)
        y <- eval b >>= getVal :: EvalM (Poly ZZModulo)
        z <- tryEvalM loc $ rAdd x y
        return $ PolyConst (ZZMod n) (fmap toExpr z) :@ loc
      _ -> reportErr loc $ NumericPolynomialExpected t

  eval (PolySub a b :@ loc) = do
    t <- unifyTypesOf loc a b
    case t of
      PolyOver ZZ ->
        lift2 loc (rSubT (constP (0::Integer))) a b
      PolyOver QQ ->
        lift2 loc (rSubT (constP (0:/:1))) a b
      PolyOver BB ->
        lift2 loc (rSubT (constP False)) a b
      PolyOver (ZZMod n) -> do
        x <- eval a >>= getVal :: EvalM (Poly ZZModulo)
        y <- eval b >>= getVal :: EvalM (Poly ZZModulo)
        z <- tryEvalM loc $ rSub x y
        return $ PolyConst (ZZMod n) (fmap toExpr z) :@ loc
      _ -> reportErr loc $ NumericPolynomialExpected t

  eval (PolyMul a b :@ loc) = do
    t <- unifyTypesOf loc a b
    case t of
      PolyOver ZZ ->
        lift2 loc (rMulT (constP (0::Integer))) a b
      PolyOver QQ ->
        lift2 loc (rMulT (constP (0:/:1))) a b
      PolyOver BB ->
        lift2 loc (rMulT (constP False)) a b
      PolyOver (ZZMod n) -> do
        x <- eval a >>= getVal :: EvalM (Poly ZZModulo)
        y <- eval b >>= getVal :: EvalM (Poly ZZModulo)
        z <- tryEvalM loc $ rMul x y
        return $ PolyConst (ZZMod n) (fmap toExpr z) :@ loc
      _ -> reportErr loc $ NumericPolynomialExpected t

  eval (PolyNeg a :@ loc) = do
    t <- typeOf a
    case t of
      PolyOver ZZ ->
        lift1 loc (rNegT (constP (0::Integer))) a
      PolyOver QQ ->
        lift1 loc (rNegT (constP (0:/:1))) a
      PolyOver BB ->
        lift1 loc (rNegT (constP False)) a
      PolyOver (ZZMod n) -> do
        x <- eval a >>= getVal :: EvalM (Poly ZZModulo)
        let y = rNeg x
        return $ PolyConst (ZZMod n) (fmap toExpr y) :@ loc
      _ -> reportErr loc $ NumericPolynomialExpected t

  eval (PolyPow a b :@ loc) = do
    t <- typeOf a
    case t of
      PolyOver ZZ ->
        lift2 loc (rPowT (constP (0::Integer))) a b
      PolyOver QQ ->
        lift2 loc (rPowT (constP (0:/:1))) a b
      PolyOver BB ->
        lift2 loc (rPowT (constP False)) a b
      PolyOver (ZZMod n) -> do
        x <- eval a >>= getVal :: EvalM (Poly ZZModulo)
        y <- eval b >>= getVal :: EvalM Integer
        z <- tryEvalM loc $ rPow x y
        return $ PolyConst (ZZMod n) (fmap toExpr z) :@ loc
      _ -> reportErr loc $ NumericPolynomialExpected t

  eval (PolyFromRoots x cs :@ loc) = do
    t <- typeOf cs
    case t of
      ListOf ZZ -> do
        as <- eval cs >>= getVal :: EvalM [Integer]
        p  <- tryEvalM loc $ fromRootsP x as
        let q = fmap toExpr p
        return (PolyConst ZZ q :@ loc)
      ListOf QQ -> do
        as <- eval cs >>= getVal :: EvalM [Rat]
        p  <- tryEvalM loc $ fromRootsP x as
        let q = fmap toExpr p
        return (PolyConst QQ q :@ loc)
      ListOf (ZZMod n) -> do
        as <- eval cs >>= getVal :: EvalM [ZZModulo]
        p  <- tryEvalM loc $ fromRootsP x as
        let q = fmap toExpr p
        return (PolyConst (ZZMod n) q :@ loc)
      _ -> reportErr loc $ NumericListExpected t

  eval (PolyEvalPoly p qs :@ loc) = do
    t <- typeOf p
    case t of
      PolyOver ZZ -> do
        a <- eval p >>= getVal :: EvalM (Poly Integer)
        let foo (x,h) = do
              b <- eval h >>= getVal :: EvalM (Poly Integer)
              return (x,b)
        ks <- sequence $ map foo qs
        c  <- tryEvalM loc $ evalPolyAtPolysP ks a
        return (PolyConst ZZ (fmap toExpr c) :@ loc)
      PolyOver QQ -> do
        a <- eval p >>= getVal :: EvalM (Poly Rat)
        let foo (x,h) = do
              b <- eval h >>= getVal :: EvalM (Poly Rat)
              return (x,b)
        ks <- sequence $ map foo qs
        c  <- tryEvalM loc $ evalPolyAtPolysP ks a
        return (PolyConst QQ (fmap toExpr c) :@ loc)
      PolyOver (ZZMod n) -> do
        a <- eval p >>= getVal :: EvalM (Poly ZZModulo)
        let foo (x,h) = do
              b <- eval h >>= getVal :: EvalM (Poly ZZModulo)
              return (x,b)
        ks <- sequence $ map foo qs
        c  <- tryEvalM loc $ evalPolyAtPolysP ks a
        return (PolyConst (ZZMod n) (fmap toExpr c) :@ loc)
      _ -> reportErr loc $ NumericPolynomialExpected t



{------------------}
{- :Eval:PermExpr -}
{------------------}

instance Eval PermExpr where
  eval (PermConst t p :@ loc) = do
    q <- seqPerm $ mapPerm eval p
    return $ PermConst t q :@ loc

  eval (PermVar key :@ loc) = lookupKey loc key >>= getVal >>= eval

  eval (PermAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [PermExpr] -> Integer -> Either ListErr PermExpr

  eval (PermAtIdx m h k :@ loc) = eAtIdx m h k loc

  eval (PermMacro vals mac :@ loc) = eMacro vals mac loc

  eval (PermIfThenElse b t f :@ _) = eIfThenElse b t f

  eval (PermRand ls :@ loc) = do
    t  <- typeOf ls
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    s  <- eval r >>= getVal :: EvalM PermExpr
    case t of
      ListOf (PermOf _) -> return s
      _ -> reportErr loc $ ListExpected t

  eval (PermCompose p q :@ loc) = do
    t <- unifyTypesOf loc p q
    case t of
      PermOf ZZ -> do
        a <- eval p >>= getVal :: EvalM (Perm Integer)
        b <- eval q >>= getVal :: EvalM (Perm Integer)
        let c = compose a b
        let s = mapPerm toExpr c
        return (PermConst ZZ s :@ loc)
      _ -> reportErr loc $ PolynomialExpected t

  eval (PermInvert p :@ loc) = do
    t <- typeOf p
    case t of
      PermOf ZZ -> do
        a <- eval p >>= getVal :: EvalM (Perm Integer)
        let c = inverse a
        let s = mapPerm toExpr c
        return (PermConst ZZ s :@ loc)
      _ -> reportErr loc $ PolynomialExpected t



{-------------------}
{- :Eval:ZZModExpr -}
{-------------------}

instance Eval ZZModExpr where
  eval (ZZModConst a :@ loc) = return $ ZZModConst a :@ loc

  eval (ZZModVar key :@ loc) = lookupKey loc key >>= getVal >>= eval

  eval (ZZModAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [ZZModExpr] -> Integer -> Either ListErr ZZModExpr

  eval (ZZModAtIdx m h k :@ loc) = eAtIdx m h k loc

  eval (ZZModMacro vals mac :@ loc) = eMacro vals mac loc

  eval (ZZModIfThenElse b t f :@ _) = eIfThenElse b t f

  eval (ZZModCast n a :@ loc) = do
    res <- eval a >>= getVal :: EvalM Integer
    return (ZZModConst (res `zzmod` n) :@ loc)

  eval (ZZModNeg  a   :@ loc) = lift1 loc (rNegT (0 `zzmod` 0)) a
  eval (ZZModInv  a   :@ loc) = lift1 loc (rInvT (0 `zzmod` 0)) a
  eval (ZZModAdd  a b :@ loc) = lift2 loc (rAddT (0 `zzmod` 0)) a b
  eval (ZZModSub  a b :@ loc) = lift2 loc (rSubT (0 `zzmod` 0)) a b
  eval (ZZModMult a b :@ loc) = lift2 loc (rMulT (0 `zzmod` 0)) a b
  eval (ZZModPow  a b :@ loc) = lift2 loc (rPowT (0 `zzmod` 0)) a b

  eval (ZZModSum   ls :@ loc) = lift1 loc (rSumT   (0 `zzmod` 0)) ls
  eval (ZZModProd  ls :@ loc) = lift1 loc (rUProdT (0 `zzmod` 0)) ls


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
  inject loc x = ZZModConst x :@ loc


{- :ListExpr -}

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


{- :MatExpr -}

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


{- :PolyExpr -}

instance Inject (Poly Integer) PolyExpr where
  inject loc x = (PolyConst ZZ $ fmap toExpr x) :@ loc

instance Inject (Poly Rat) PolyExpr where
  inject loc x = (PolyConst QQ $ fmap toExpr x) :@ loc

instance Inject (Poly Bool) PolyExpr where
  inject loc x = (PolyConst BB $ fmap toExpr x) :@ loc


{- :PermExpr -}

instance Inject (Perm Integer) PermExpr where
  inject loc x = (PermConst ZZ $ mapPerm toExpr x) :@ loc

instance Inject (Perm String) PermExpr where
  inject loc x = (PermConst SS $ mapPerm toExpr x) :@ loc

instance Inject (Perm Text) PermExpr where
  inject loc x = (PermConst SS $ mapPerm toExpr x) :@ loc

instance Inject (Perm Rat) PermExpr where
  inject loc x = (PermConst QQ $ mapPerm toExpr x) :@ loc


instance Inject String Doc where
  inject loc = \x -> DocText (Text x) :@ loc


{---------}
{- :Lift -}
{---------}

lift1
  :: ( Eval x, ToExpr x, Get a
     , Inject b y, HasLocus y
     , PromoteError err
  ) => Locus -> (a -> Either err b) -> x -> EvalM y
lift1 loc f x = do
  a <- eval x >>= getVal
  b <- tryEvalM loc $ f a
  return $ inject loc b


lift2
  :: ( Eval x, ToExpr x, Get a
     , Eval y, ToExpr y, Get b
     , Inject c z, HasLocus z
     , PromoteError err
  ) => Locus -> (a -> b -> Either err c) -> x -> y -> EvalM z
lift2 loc f x y = do
  a <- eval x >>= getVal
  b <- eval y >>= getVal
  c <- tryEvalM loc $ f a b
  return $ inject loc c


{- Saved for future use
lift3
  :: ( Eval x, ToExpr x, Get a
     , Eval y, ToExpr y, Get b
     , Eval z, ToExpr z, Get c
     , Inject d w, HasLocus w
     , PromoteError err
  ) => Locus -> (a -> b -> c -> Either err d) -> x -> y -> z -> EvalM w
lift3 loc f x y z = do
  a <- eval x >>= getVal
  b <- eval y >>= getVal
  c <- eval z >>= getVal
  case f a b c of
    Left err -> reportErr loc err
    Right d  -> return $ inject loc d
-}


{- Saved for future use
lift4
  :: ( Eval x, ToExpr x, Get a
     , Eval y, ToExpr y, Get b
     , Eval z, ToExpr z, Get c
     , Eval w, ToExpr w, Get d
     , Inject e u, HasLocus u
     , PromoteError err
  ) => Locus -> (a -> b -> c -> d -> Either err e) -> x -> y -> z -> w -> EvalM u
lift4 loc f x y z w = do
  a <- eval x >>= getVal
  b <- eval y >>= getVal
  c <- eval z >>= getVal
  d <- eval w >>= getVal
  case f a b c d of
    Left err -> reportErr loc err
    Right e  -> return $ inject loc e
-}



{--------------}
{- :Utilities -}
{--------------}

evalToText :: (Eval t, ToExpr t, Glyph t) => t -> EvalM String
evalToText t = eval t >>= toGlyph




