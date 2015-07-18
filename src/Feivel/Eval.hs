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
 Eval, eval, evalToGlyph
) where

{-----------------------------------------------------------}
{- Contents                                                -}
{-  :Eval              :Eval:Expr        :Eval:Utilities   -}
{-    :Eval:IntExpr    :Eval:StrExpr     :Eval:BoolExpr    -}
{-    :Eval:RatExpr    :Eval:ListExpr    :Eval:MacExpr     -}
{-    :Eval:MatExpr    :Eval:Doc         :Eval:PolyExpr    -}
{-    :Eval:PermExpr   :Eval:ZZModExpr                     -}
{-                                                         -}
{-  :Glyph                                                 -}
{-  :Lift                                                  -}
{-  :Constants                                             -}
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
import Feivel.Parse (pInteger, pRat)
import Feivel.Get
import Feivel.Put

import Data.List (intersperse, (\\), sort, nub, permutations)
import Control.Monad (filterM)


{---------}
{- :Eval -}
{---------}

class Eval t where
  eval :: t -> EvalM t

-- eval with a specified store
evalWith :: (Eval t) => t -> Store Expr -> EvalM t
evalWith t st = do
  old <- getState
  putState st
  u <- eval t
  putState old
  return u

{-
-- eval each item of a list in sequence
evalSeq :: (Eval t) => [t] -> EvalM [t]
evalSeq = sequence . map eval

-- eval each item of a list in parallel
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
-}

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

eKey :: (Eval a, Get a) => Key -> Locus -> EvalM a
eKey key loc = lookupKey loc key >>= getVal >>= eval

eIfThenElse :: (ToExpr a, Get a, Eval a) => Expr -> a -> a -> EvalM a
eIfThenElse b t f = do
  test  <- eval b >>= getVal
  true  <- eval t >>= getVal
  false <- eval f >>= getVal
  if test then (eval true) else (eval false)

eMacro :: (Get b, Eval b) => [(Type, Key, Expr)] -> Expr -> Locus -> EvalM b
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

evalToGlyph :: (ToExpr a) => a -> EvalM String
evalToGlyph x = eval (toExpr x) >>= toGlyph



{-----------------}
{- :Eval:IntExpr -}
{-----------------}

instance Eval IntExpr where
  eval (IntConst n :@ loc) = return (IntConst n :@ loc)

  {- :Common -}
  eval (IntVar key :@ loc)        = eKey key loc
  eval (IntAtIdx m h k :@ loc)    = eAtIdx m h k loc
  eval (IntIfThenElse b t f :@ _) = eIfThenElse b t f
  eval (IntMacro vals mac :@ loc) = eMacro vals mac loc

  eval (IntAtPos a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [IntExpr] -> Integer -> Either ListErr IntExpr

  eval (IntNeg        a :@ loc) = lift1 loc a (rNegT        zeroZZ)
  eval (IntAbs        a :@ loc) = lift1 loc a (rAbsT        zeroZZ)
  eval (IntSqPart     a :@ loc) = lift1 loc a (rSqPartT     zeroZZ)
  eval (IntSqFreePart a :@ loc) = lift1 loc a (rSqFreePartT zeroZZ)
  eval (IntRad        a :@ loc) = lift1 loc a (rRadT        zeroZZ)

  eval (IntAdd  a b :@ loc) = lift2 loc a b (rAddT zeroZZ)
  eval (IntSub  a b :@ loc) = lift2 loc a b (rSubT zeroZZ)
  eval (IntMult a b :@ loc) = lift2 loc a b (rMulT zeroZZ)
  eval (IntMod  a b :@ loc) = lift2 loc a b (rRemT zeroZZ)
  eval (IntMin  a b :@ loc) = lift2 loc a b (rMinT zeroZZ)
  eval (IntMax  a b :@ loc) = lift2 loc a b (rMaxT zeroZZ)
  eval (IntGCD  a b :@ loc) = lift2 loc a b (rGCDT zeroZZ)
  eval (IntLCM  a b :@ loc) = lift2 loc a b (rLCMT zeroZZ)
  eval (IntQuo  a b :@ loc) = lift2 loc a b (rQuoT zeroZZ)
  eval (IntPow  a b :@ loc) = lift2 loc a b (rPowT zeroZZ)

  eval (IntChoose a b :@ loc) = lift2 loc a b (rChooseT zeroZZ)

  eval (RatNumer  p :@ loc) = lift1 loc p (ratNum)
  eval (RatDenom  p :@ loc) = lift1 loc p (ratDen)
  eval (RatFloor  p :@ loc) = lift1 loc p (ratFlr)
  eval (StrLength s :@ loc) = lift1 loc s (strLen)

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

  eval (IntSum   ls :@ loc) = lift1 loc ls (rSumT   zeroZZ)
  eval (IntProd  ls :@ loc) = lift1 loc ls (rUProdT zeroZZ)
  eval (IntMaxim ls :@ loc) = lift1 loc ls (rMaximT zeroZZ)
  eval (IntMinim ls :@ loc) = lift1 loc ls (rMinimT zeroZZ)
  eval (IntGCDiv ls :@ loc) = lift1 loc ls (rGCDsT  zeroZZ)
  eval (IntLCMul ls :@ loc) = lift1 loc ls (rLCMsT  zeroZZ)

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

  eval (MatRank m :@ loc) = do
    case typeOf m of
      MatOf QQ -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        r <- tryEvalM loc $ mRank n
        return $ IntConst r :@ loc
      MatOf BB -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        r <- tryEvalM loc $ mRank n
        return $ IntConst r :@ loc
      MatOf u -> reportErr loc $ FieldMatrixExpected u
      u -> reportErr loc $ MatrixExpected u

  eval (IntContent p :@ loc) = do
    q <- eval p >>= getVal :: EvalM (Poly Integer)
    c <- tryEvalM loc $ contentP q
    return $ IntConst c :@ loc



{-----------------}
{- :Eval:StrExpr -}
{-----------------}

instance Eval StrExpr where
  eval (StrConst s :@ loc) = return (StrConst s :@ loc)

  {- :Common -}
  eval (StrVar key :@ loc)        = eKey key loc
  eval (StrAtIdx m h k :@ loc)    = eAtIdx m h k loc
  eval (StrIfThenElse b t f :@ _) = eIfThenElse b t f
  eval (StrMacro vals mac :@ loc) = eMacro vals mac loc

  eval (StrAtPos a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [StrExpr] -> Integer -> Either ListErr StrExpr

  eval (Concat   a b :@ loc) = lift2 loc a b (strCat)
  eval (StrStrip a b :@ loc) = lift2 loc a b (strStrip)

  eval (ToUpper   a :@ loc) = lift1 loc a (strUpper)
  eval (ToLower   a :@ loc) = lift1 loc a (strLower)
  eval (Reverse   a :@ loc) = lift1 loc a (strRev)
  eval (Rot13     a :@ loc) = lift1 loc a (strRot13)
  eval (StrHex    n :@ loc) = lift1 loc n (strHex)
  eval (StrRoman  n :@ loc) = lift1 loc n (strRoman)
  eval (StrBase36 n :@ loc) = lift1 loc n (strBase36)

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
    return $ StrConst (Text $ show $ typeOf e) :@ loc

  eval (StrFormat LaTeX e :@ loc) = do
    case typeOf e of
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
    s <- eval a >>= toGlyph
    return $ StrConst (Text s) :@ loc



{------------------}
{- :Eval:BoolExpr -}
{------------------}

instance Eval BoolExpr where
  eval (BoolConst b :@ loc) = return (BoolConst b :@ loc)

  {- :Common -}
  eval (BoolVar key :@ loc)        = eKey key loc
  eval (BoolAtIdx m h k :@ loc)    = eAtIdx m h k loc
  eval (BoolIfThenElse b t f :@ _) = eIfThenElse b t f
  eval (BoolMacro vals mac :@ loc) = eMacro vals mac loc

  eval (BoolAtPos a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [BoolExpr] -> Integer -> Either ListErr BoolExpr

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
    case unify (typeOf a) (typeOf b) of
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
    case unify (typeOf a) (typeOf b) of
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
    case unify (typeOf a) (typeOf b) of
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
    case unify (typeOf a) (typeOf b) of
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
    case typeOf m of
      MatOf QQ -> do
        p <- eval m >>= getVal :: EvalM (Matrix Rat)
        q <- tryEvalM loc $ mIsGaussJordanForm p
        return $ BoolConst q :@ loc
      MatOf BB -> do
        p <- eval m >>= getVal :: EvalM (Matrix Bool)
        q <- tryEvalM loc $ mIsGaussJordanForm p
        return $ BoolConst q :@ loc
      t -> reportErr loc $ NumericMatrixExpected t

  -- Bool
  eval (Neg    a   :@ loc) = lift1 loc a   (boolNot)
  eval (Conj   a b :@ loc) = lift2 loc a b (boolAnd)
  eval (Disj   a b :@ loc) = lift2 loc a b (boolOr)
  eval (Imp    a b :@ loc) = lift2 loc a b (boolImp)

  -- Int
  eval (IntSqFree a :@ loc) = lift1 loc a   (rIsSqFreeT zeroZZ)
  eval (IntDiv a b :@ loc)  = lift2 loc a b (rDividesT  zeroZZ)

  -- Str
  eval (Matches a b :@ loc) = lift2 loc a b (strMatch)



{-----------------}
{- :Eval:RatExpr -}
{-----------------}

instance Eval RatExpr where
  eval (RatConst p :@ loc) = return $ RatConst p :@ loc

  {- :Common -}
  eval (RatVar key :@ loc)        = eKey key loc
  eval (RatAtIdx m h k :@ loc)    = eAtIdx m h k loc
  eval (RatIfThenElse b t f :@ _) = eIfThenElse b t f
  eval (RatMacro vals mac :@ loc) = eMacro vals mac loc

  eval (RatAtPos a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [RatExpr] -> Integer -> Either ListErr RatExpr

  eval (RatCast expr :@ loc) = do
    n <- eval expr >>= getVal :: EvalM Integer
    return $ RatConst (n:/:1) :@ loc

  eval (RatNeg  a :@ loc)   = lift1 loc a (rNegT zeroQQ)
  eval (RatAbs  a :@ loc)   = lift1 loc a (rAbsT zeroQQ)

  eval (RatAdd  a b :@ loc) = lift2 loc a b (rAddT zeroQQ)
  eval (RatSub  a b :@ loc) = lift2 loc a b (rSubT zeroQQ)
  eval (RatMult a b :@ loc) = lift2 loc a b (rMulT zeroQQ)
  eval (RatMin  a b :@ loc) = lift2 loc a b (rMinT zeroQQ)
  eval (RatMax  a b :@ loc) = lift2 loc a b (rMaxT zeroQQ)
  eval (RatPow  a b :@ loc) = lift2 loc a b (rPowT zeroQQ)
  eval (RatQuot a b :@ loc) = lift2 loc a b (rDivT zeroQQ)

  eval (RatSqrt p k :@ loc) = lift2 loc p k (ratSqt)

  eval (RatRand ls :@ loc) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs
    return $ RatConst r :@ loc

  eval (RatSum   ls :@ loc) = lift1 loc ls (rSumT   (0:/:1))
  eval (RatProd  ls :@ loc) = lift1 loc ls (rUProdT (0:/:1))
  eval (RatMaxim ls :@ loc) = lift1 loc ls (rMaximT (0:/:1))
  eval (RatMinim ls :@ loc) = lift1 loc ls (rMinimT (0:/:1))

  {- Mean -}
  eval (RatMean ls :@ loc) = do
    case typeOf ls of
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
    case typeOf ls of
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
    k <- eval d >>= getVal
    case typeOf ls of
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
    k <- eval d >>= getVal
    y <- eval x >>= getVal
    case typeOf ls of
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

  {- :Common -}
  eval (ListVar _ key :@ loc)        = eKey key loc
  eval (ListAtIdx _ m h k :@ loc)    = eAtIdx m h k loc
  eval (ListMacro _ vals mac :@ loc) = eMacro vals mac loc
  eval (ListIfThenElse _ b t f :@ _) = eIfThenElse b t f

  eval (ListAtPos _ a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [ListExpr] -> Integer -> Either ListErr ListExpr

  eval (ListRange _ a b :@ loc) = do
    x <- eval a >>= getVal :: EvalM Integer
    y <- eval b >>= getVal :: EvalM Integer
    return (ListConst ZZ [IntE $ IntConst k :@ loc | k <- [x..y]] :@ loc)

  eval (ListCat u a b :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Expr]
    ys <- eval b >>= getVal :: EvalM [Expr]
    return (ListConst u (xs ++ ys) :@ loc)

  eval (ListToss _ a b :@ loc) = do
    t <- unifyTypesOf loc a b
    case t of
      ListOf u -> do
        xs <- eval a >>= getVal :: EvalM [Expr]
        ys <- eval b >>= getVal :: EvalM [Expr]
        return $ ListConst u (xs \\ ys) :@ loc
      _ -> reportErr loc $ ListExpected t

  eval (ListRev u a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Expr]
    return $ ListConst u (reverse xs) :@ loc

  eval (ListSort SS a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Text]
    return $ ListConst SS (map (\k -> StrE $ StrConst k :@ loc) (sort xs)) :@ loc
  eval (ListSort ZZ a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Integer]
    return $ ListConst ZZ (map (\k -> IntE $ IntConst k :@ loc) (sort xs)) :@ loc
  eval (ListSort QQ a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Rat]
    return $ ListConst QQ (map (\k -> RatE $ RatConst k :@ loc) (sort xs)) :@ loc
  eval (ListSort BB a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Bool]
    return $ ListConst BB (map (\k -> BoolE $ BoolConst k :@ loc) (sort xs)) :@ loc
  eval (ListSort typ _ :@ loc) = reportErr loc $ SortableListExpected typ

  eval (ListRand _ ls :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    randomElementEvalM xs >>= getVal :: EvalM ListExpr

  eval (ListUniq u a :@ loc) = do
    xs <- eval a >>= getVal :: EvalM [Expr]
    return $ ListConst u (nub xs) :@ loc

  eval (ListShuffle u ls :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    ys <- shuffleEvalM xs
    return $ ListConst u ys :@ loc

  eval (ListShuffles (ListOf u) ls :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    let us = [ListE $ ListConst (ListOf u) ys :@ loc | ys <- permutations xs]
    return (ListConst (ListOf u) us :@ loc)
  eval (ListShuffles u _ :@ loc) = reportErr loc $ ListExpected u

  eval (ListChoose u n ls :@ loc) = do
    k  <- eval n >>= getVal :: EvalM Integer
    xs <- eval ls >>= getVal :: EvalM [Expr]
    ys <- sampleEvalM (fromIntegral k) xs
    return (ListConst u ys :@ loc)

  eval (ListChoices (ListOf u) n ls :@ loc) = do
    k  <- eval n  >>= getVal :: EvalM Integer
    xs <- eval ls >>= getVal :: EvalM [Expr]
    let foos = [ListE $ ListConst u x :@ loc | x <- combinations (fromIntegral k) xs]
    return $ ListConst (ListOf u) foos :@ loc
  eval (ListChoices u _ _ :@ loc) = reportErr loc $ ListExpected u

  eval (ListBuilder _ e gs :@ loc) = do
    st <- getState
    xs <- bar st gs
    ys <- sequence [evalWith e x >>= getVal | x <- xs]
    t <- case ys of
           [] -> return XX
           (z:_) -> return (typeOf z)
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

  eval (ListFilter u k g xs :@ loc) = do
    ys <- eval xs >>= getVal :: EvalM [Expr]
    let foo e = do
          defineKey k e loc
          x <- eval g >>= getVal :: EvalM Bool
          undefineKey k
          return x
    zs <- filterM foo ys
    return (ListConst u zs :@ loc)

  eval (ListMatRow u k m :@ loc) = do
    i  <- eval k >>= getVal :: EvalM Integer
    n  <- eval m >>= getVal :: EvalM (Matrix Expr)
    as <- tryEvalM loc $ mListRowOf i n
    return (ListConst u as :@ loc)

  eval (ListMatCol u k m :@ loc) = do
    i  <- eval k >>= getVal :: EvalM Integer
    n  <- eval m >>= getVal :: EvalM (Matrix Expr)
    as <- tryEvalM loc $ mListColOf i n
    return (ListConst u as :@ loc)

  eval (ListPermsOf typ xs :@ loc) = do
    case typ of
      PermOf ZZ -> do
        as <- eval xs >>= getVal :: EvalM [Integer]
        qs <- tryEvalM loc $ permsOf as
        let us = map (put loc) qs :: [Expr]
        return (ListConst (PermOf ZZ) us :@ loc)
      PermOf SS -> do
        as <- eval xs >>= getVal :: EvalM [Text]
        qs <- tryEvalM loc $ permsOf as
        let us = map (put loc) qs :: [Expr]
        return (ListConst (PermOf SS) us :@ loc)
      PermOf QQ -> do
        as <- eval xs >>= getVal :: EvalM [Rat]
        qs <- tryEvalM loc $ permsOf as
        let us = map (put loc) qs :: [Expr]
        return (ListConst (PermOf SS) us :@ loc)
      u -> reportErr loc $ PermutationExpected u

  eval (ListPivotColIndices _ m :@ loc) = do
    let pivotIndices x = do
          n  <- eval m >>= getVal
          suchThat $ n `hasSameTypeAs` x
          is <- tryEvalM loc $ mPivotCols n
          return (ListConst ZZ (map toExpr is) :@ loc)
    case typeOf m of
      MatOf QQ -> pivotIndices (mCell zeroQQ)
      MatOf BB -> pivotIndices (mCell zeroBB)
      MatOf u  -> reportErr loc $ FieldMatrixExpected u
      t        -> reportErr loc $ MatrixExpected t



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

  {- :Common -}
  eval (MacVar _ key :@ loc)        = eKey key loc
  eval (MacIfThenElse _ b t f :@ _) = eIfThenElse b t f
  eval (MacAtIdx _ m h k :@ loc)    = eAtIdx m h k loc
  eval (MacMacro _ vals mac :@ loc) = eMacro vals mac loc

  eval (MacAtPos _ a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [MacExpr] -> Integer -> Either ListErr MacExpr

  eval (MacRand _ ls :@ _) = do
    xs <- eval ls >>= getVal
    randomElementEvalM xs



{-----------------}
{- :Eval:MatExpr -}
{-----------------}

instance Eval MatExpr where
  eval (MatConst t m :@ loc) = do
    n <- mSeq $ fmap eval m
    return $ MatConst t n :@ loc

  {- :Common -}
  eval (MatVar _ key :@ loc)        = eKey key loc
  eval (MatAtIdx _ m h k :@ loc)    = eAtIdx m h k loc
  eval (MatMacro _ vals mac :@ loc) = eMacro vals mac loc
  eval (MatIfThenElse _ b t f :@ _) = eIfThenElse b t f

  eval (MatAtPos _ a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [MatExpr] -> Integer -> Either ListErr MatExpr

  eval (MatRand _ ls :@ _) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    eval r >>= getVal

  eval (MatRowFromList t xs :@ loc) = do
    as <- eval xs >>= getVal :: EvalM [Expr]
    m  <- tryEvalM loc $ mRowFromList as
    return (MatConst t m :@ loc)

  eval (MatColFromList t xs :@ loc) = do
    as <- eval xs >>= getVal :: EvalM [Expr]
    m  <- tryEvalM loc $ mColFromList as
    return (MatConst t m :@ loc)

  eval (MatId u n :@ loc) = do
    let makeIdMat x = lift1 loc n (mEIdT x)
    case u of
      ZZ          -> makeIdMat zeroZZ
      QQ          -> makeIdMat zeroQQ
      BB          -> makeIdMat zeroBB
      ZZMod k     -> makeIdMat (zeroMod k)
      PolyOver ZZ -> makeIdMat (constP zeroZZ)
      PolyOver QQ -> makeIdMat (constP zeroQQ)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatSwapE u n h k :@ loc) = do
    let makeSwapMat x = lift3 loc n h k (mESwapT x)
    case u of
      ZZ          -> makeSwapMat zeroZZ
      QQ          -> makeSwapMat zeroQQ
      BB          -> makeSwapMat zeroBB
      ZZMod n     -> makeSwapMat (zeroMod n)
      PolyOver ZZ -> makeSwapMat (constP zeroZZ)
      PolyOver QQ -> makeSwapMat (constP zeroQQ)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatScaleE u n h r :@ loc) = do
    let makeScaleMat x = lift3 loc n h r (mEScaleT x)
    case u of
      ZZ          -> makeScaleMat zeroZZ
      QQ          -> makeScaleMat zeroQQ
      BB          -> makeScaleMat zeroBB
      ZZMod n     -> makeScaleMat (zeroMod n)
      PolyOver ZZ -> makeScaleMat (constP zeroZZ)
      PolyOver QQ -> makeScaleMat (constP zeroQQ)
      PolyOver BB -> makeScaleMat (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatAddE u n h k r :@ loc) = do
    let makeAddMat x = lift4 loc n h k r (mEAddT x)
    case u of
      ZZ          -> makeAddMat zeroZZ
      QQ          -> makeAddMat zeroQQ
      BB          -> makeAddMat zeroBB
      ZZMod d     -> makeAddMat (zeroMod d)
      PolyOver ZZ -> makeAddMat (constP zeroZZ)
      PolyOver QQ -> makeAddMat (constP zeroQQ)
      PolyOver BB -> makeAddMat (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatShuffleRows u m :@ loc) = do
    x  <- eval m >>= getVal :: EvalM (Matrix Expr)
    rs <- tryEvalM loc $ mRowsOf x
    ts <- shuffleEvalM rs
    n  <- tryEvalM loc $ mVCats ts
    return (MatConst u n :@ loc)

  eval (MatShuffleCols u m :@ loc) = do
    x  <- eval m >>= getVal :: EvalM (Matrix Expr)
    rs <- tryEvalM loc $ mColsOf x
    ts <- shuffleEvalM rs
    n  <- tryEvalM loc $ mHCats ts
    return (MatConst u n :@ loc)

  eval (MatHCat u a b :@ loc) = do
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    n <- eval b >>= getVal :: EvalM (Matrix Expr)
    x <- tryEvalM loc $ mHCat m n
    return (MatConst u x :@ loc)

  eval (MatVCat u a b :@ loc) = do
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    n <- eval b >>= getVal :: EvalM (Matrix Expr)
    x <- tryEvalM loc $ mVCat m n
    return $ MatConst u x :@ loc

  eval (MatAdd u a b :@ loc) = do
    let addMat x = lift2 loc a b (rAddT (mCell x))
    case u of
      ZZ          -> addMat zeroZZ
      QQ          -> addMat zeroQQ
      BB          -> addMat zeroBB
      ZZMod n     -> addMat (zeroMod n)
      PolyOver ZZ -> addMat (constP zeroZZ)
      PolyOver QQ -> addMat (constP zeroQQ)
      PolyOver BB -> addMat (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatNeg u a :@ loc) = do
    let negMat x = lift1 loc a (rNegT (mCell x))
    case u of
      ZZ          -> negMat zeroZZ
      QQ          -> negMat zeroQQ
      BB          -> negMat zeroBB
      ZZMod n     -> negMat (zeroMod n)
      PolyOver ZZ -> negMat (constP zeroZZ)
      PolyOver QQ -> negMat (constP zeroQQ)
      PolyOver BB -> negMat (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatTrans _ a :@ loc) = do
    u <- expectMatrix loc a
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    p <- tryEvalM loc $ mTranspose m
    return (MatConst u p :@ loc)

  eval (MatMul u a b :@ loc) = do
    let mulMat x = lift2 loc a b (rMulT (mCell x))
    case u of
      ZZ          -> mulMat zeroZZ
      QQ          -> mulMat zeroQQ
      BB          -> mulMat zeroBB
      (ZZMod n)   -> mulMat (zeroMod n)
      PolyOver ZZ -> mulMat (constP zeroZZ)
      PolyOver QQ -> mulMat (constP zeroQQ)
      PolyOver BB -> mulMat (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatPow u m n :@ loc) = do
    let powMat x = lift2 loc m n (rPosPowT (mCell x))
    case u of
      ZZ          -> powMat zeroZZ
      QQ          -> powMat zeroQQ
      BB          -> powMat zeroBB
      (ZZMod k)   -> powMat (zeroMod k)
      PolyOver ZZ -> powMat (constP zeroZZ)
      PolyOver QQ -> powMat (constP zeroQQ)
      PolyOver BB -> powMat (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatSwapRows _ m a b :@ loc) = do
    u <- expectMatrix loc m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    j <- eval b >>= getVal
    p <- tryEvalM loc $ mSwapRows i j n
    return $ MatConst u p :@ loc

  eval (MatSwapCols _ m a b :@ loc) = do
    u <- expectMatrix loc m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    j <- eval b >>= getVal
    p <- tryEvalM loc $ mSwapCols i j n
    return $ MatConst u p :@ loc

  eval (MatScaleRow u m a h :@ loc) = do
    let scaleRowMat x = lift3 loc a h m (mScaleRowT x)
    case u of
      ZZ          -> scaleRowMat zeroZZ
      QQ          -> scaleRowMat zeroQQ
      BB          -> scaleRowMat zeroBB
      (ZZMod n)   -> scaleRowMat (zeroMod n)
      PolyOver ZZ -> scaleRowMat (constP zeroZZ)
      PolyOver QQ -> scaleRowMat (constP zeroQQ)
      PolyOver BB -> scaleRowMat (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatScaleCol u m a h :@ loc) = do
    let scaleColMat x = lift3 loc a h m (mScaleColT x)
    case u of
      ZZ          -> scaleColMat zeroZZ
      QQ          -> scaleColMat zeroQQ
      BB          -> scaleColMat zeroBB
      (ZZMod n)   -> scaleColMat (zeroMod n)
      PolyOver ZZ -> scaleColMat (constP zeroZZ)
      PolyOver QQ -> scaleColMat (constP zeroQQ)
      PolyOver BB -> scaleColMat (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (MatAddRow _ m a h k :@ loc) = do
    let t = typeOf m
    let u = typeOf a
    i <- eval h >>= getVal
    j <- eval k >>= getVal
    let makeAddRow zer = do
          n <- eval m >>= getVal
          r <- eval a >>= getVal
          suchThat $ r `hasSameTypeAs` zer
          p <- tryEvalM loc $ mAddRow r i j n
          getVal (put loc p)
    case unify t (MatOf u) of
      Right (MatOf ZZ) -> makeAddRow zeroZZ
      Right (MatOf QQ) -> makeAddRow zeroQQ
      Right (MatOf BB) -> makeAddRow zeroBB
      Right (MatOf (ZZMod n)) -> makeAddRow (zeroMod n)
      Right w  -> reportErr loc $ NumericMatrixExpected w
      Left err -> reportErr loc err

  eval (MatAddCol _ m a h k :@ loc) = do
    let t = typeOf m
    let u = typeOf a
    i <- eval h >>= getVal
    j <- eval k >>= getVal
    let makeAddCol zer = do
          n <- eval m >>= getVal
          r <- eval a >>= getVal
          suchThat $ r `hasSameTypeAs` zer
          p <- tryEvalM loc $ mAddCol r i j n
          getVal (put loc p)
    case unify t (MatOf u) of
      Right (MatOf ZZ) -> makeAddCol zeroZZ
      Right (MatOf QQ) -> makeAddCol zeroQQ
      Right (MatOf BB) -> makeAddCol zeroBB
      Right (MatOf (ZZMod n)) -> makeAddCol (zeroMod n)
      Right w -> reportErr loc $ NumericMatrixExpected w
      Left err -> reportErr loc err

  eval (MatDelRow _ m a :@ loc) = do
    u <- expectMatrix loc m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    p <- tryEvalM loc $ mDelRow n i
    return $ MatConst u p :@ loc

  eval (MatDelCol _ m a :@ loc) = do
    u <- expectMatrix loc m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal
    p <- tryEvalM loc $ mDelCol n i
    return $ MatConst u p :@ loc

  eval (MatGJForm u m :@ loc) = do
    let gjFormMat x = lift1 loc m (mGJFormT x)
    case u of
      QQ -> gjFormMat zeroQQ
      BB -> gjFormMat zeroBB
      _  -> reportErr loc $ FieldTypeExpected u

  eval (MatGJFactor u m :@ loc) = do
    let gjFactorMat x = lift1 loc m (mGJFactorT x)
    case u of
      QQ -> gjFactorMat zeroQQ
      BB -> gjFactorMat zeroBB
      _  -> reportErr loc $ FieldTypeExpected u

  eval (MatGetRow _ k m :@ loc) = do
    u <- expectMatrix loc m
    i <- eval k >>= getVal :: EvalM Integer
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    r <- tryEvalM loc $ mRowOf i n
    return (MatConst u r :@ loc)

  eval (MatGetCol _ k m :@ loc) = do
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
    s <- evalToGlyph expr
    return $ DocText (Text s) :@ loc

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

  eval (NakedExpr expr :@ loc) = do
    x <- evalToGlyph expr
    return $ DocText (Text x) :@ loc

  eval (Import file Nothing rest :@ _) = do
    oldSt <- getState
    clearState
    _ <- readAndParseDocFromLib file >>= eval
    newSt <- getState
    putState oldSt
    mergeStateEvalM newSt
    eval rest

  eval (Import file (Just prefix) rest :@ _) = do
    oldSt <- getState
    clearState
    _ <- readAndParseDocFromLib file >>= eval
    newSt <- getState
    putState oldSt
    mergeStateEvalM (qualify prefix newSt)
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
    let tw = typeOf w
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

  {- :Common -}
  eval (PolyVar _ key :@ loc)        = eKey key loc
  eval (PolyAtIdx _ m h k :@ loc)    = eAtIdx m h k loc
  eval (PolyMacro _ vals mac :@ loc) = eMacro vals mac loc
  eval (PolyIfThenElse _ b t f :@ _) = eIfThenElse b t f

  eval (PolyAtPos _ a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [PolyExpr] -> Integer -> Either ListErr PolyExpr

  eval (PolyRand _ ls :@ loc) = do
    let t = typeOf ls
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    s  <- eval r >>= getVal :: EvalM PolyExpr
    case t of
      ListOf (PolyOver _) -> return s
      _ -> reportErr loc $ ListExpected t

  eval (PolyAdd u a b :@ loc) = do
    let addPoly x = lift2 loc a b (rAddT (constP x))
    case u of
      ZZ          -> addPoly zeroZZ
      QQ          -> addPoly zeroQQ
      BB          -> addPoly zeroBB
      (ZZMod n)   -> addPoly (zeroMod n)
      PolyOver ZZ -> addPoly (constP zeroZZ)
      PolyOver QQ -> addPoly (constP zeroQQ)
      PolyOver BB -> addPoly (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (PolySub u a b :@ loc) = do
    let subPoly x = lift2 loc a b (rSubT (constP x))
    case u of
      ZZ          -> subPoly zeroZZ
      QQ          -> subPoly zeroQQ
      BB          -> subPoly zeroBB
      (ZZMod n)   -> subPoly (zeroMod n)
      PolyOver ZZ -> subPoly (constP zeroZZ)
      PolyOver QQ -> subPoly (constP zeroQQ)
      PolyOver BB -> subPoly (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (PolyMul u a b :@ loc) = do
    let mulPoly x = lift2 loc a b (rMulT (constP x))
    case u of
      ZZ          -> mulPoly zeroZZ
      QQ          -> mulPoly zeroQQ
      BB          -> mulPoly zeroBB
      (ZZMod n)   -> mulPoly (zeroMod n)
      PolyOver ZZ -> mulPoly (constP zeroZZ)
      PolyOver QQ -> mulPoly (constP zeroQQ)
      PolyOver BB -> mulPoly (constP zeroBB)
      _           -> reportErr loc $ NumericTypeExpected u

  eval (PolyNeg _ a :@ loc) = do
    let t = typeOf a
    case t of
      PolyOver ZZ ->
        lift1 loc a (rNegT (constP (0::Integer)))
      PolyOver QQ ->
        lift1 loc a (rNegT (constP (0:/:1)))
      PolyOver BB ->
        lift1 loc a (rNegT (constP False))
      PolyOver (ZZMod n) -> do
        x <- eval a >>= getVal :: EvalM (Poly ZZModulo)
        let y = rNeg x
        return $ PolyConst (ZZMod n) (fmap toExpr y) :@ loc
      _ -> reportErr loc $ NumericPolynomialExpected t

  eval (PolyPow _ a b :@ loc) = do
    let t = typeOf a
    case t of
      PolyOver ZZ ->
        lift2 loc a b (rPowT (constP (0::Integer)))
      PolyOver QQ ->
        lift2 loc a b (rPowT (constP (0:/:1)))
      PolyOver BB ->
        lift2 loc a b (rPowT (constP False))
      PolyOver (ZZMod n) -> do
        x <- eval a >>= getVal :: EvalM (Poly ZZModulo)
        y <- eval b >>= getVal :: EvalM Integer
        z <- tryEvalM loc $ rPow x y
        return $ PolyConst (ZZMod n) (fmap toExpr z) :@ loc
      _ -> reportErr loc $ NumericPolynomialExpected t

  eval (PolyFromRoots _ x cs :@ loc) = do
    let t = typeOf cs
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

  eval (PolyEvalPoly _ p qs :@ loc) = do
    let t = typeOf p
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

  eval (PermAtPos _ a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [PermExpr] -> Integer -> Either ListErr PermExpr

  {- Common -}
  eval (PermVar _ key :@ loc)        = eKey key loc
  eval (PermAtIdx _ m h k :@ loc)    = eAtIdx m h k loc
  eval (PermMacro _ vals mac :@ loc) = eMacro vals mac loc
  eval (PermIfThenElse _ b t f :@ _) = eIfThenElse b t f

  eval (PermRand _ ls :@ loc) = do
    let t = typeOf ls
    xs <- eval ls >>= getVal :: EvalM [Expr]
    r  <- randomElementEvalM xs
    s  <- eval r >>= getVal :: EvalM PermExpr
    case t of
      ListOf (PermOf _) -> return s
      _ -> reportErr loc $ ListExpected t

  eval (PermCompose _ p q :@ loc) = do
    t <- unifyTypesOf loc p q
    case t of
      PermOf ZZ -> do
        a <- eval p >>= getVal :: EvalM (Perm Integer)
        b <- eval q >>= getVal :: EvalM (Perm Integer)
        let c = compose a b
        let s = mapPerm toExpr c
        return (PermConst ZZ s :@ loc)
      _ -> reportErr loc $ PolynomialExpected t

  eval (PermInvert _ p :@ loc) = do
    let t = typeOf p
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

instance Eval (ZZModExpr Expr) where
  eval (ZZModConst n a :@ loc) = return $ ZZModConst n a :@ loc

  {- :Common -}
  eval (ZZModVar _ key :@ loc)        = eKey key loc
  eval (ZZModAtIdx _ m h k :@ loc)    = eAtIdx m h k loc
  eval (ZZModMacro _ vals mac :@ loc) = eMacro vals mac loc
  eval (ZZModIfThenElse _ b t f :@ _) = eIfThenElse b t f

  eval (ZZModAtPos _ a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [ZZModExpr Expr] -> Integer -> Either ListErr (ZZModExpr Expr)

  eval (ZZModCast (ZZMod n) a :@ loc) = do
    res <- eval a >>= getVal :: EvalM Integer
    return (ZZModConst (ZZMod n) (res `zzmod` n) :@ loc)

  eval (ZZModNeg  _ a   :@ loc) = lift1 loc a   (rNegT (0 `zzmod` 0))
  eval (ZZModInv  _ a   :@ loc) = lift1 loc a   (rInvT (0 `zzmod` 0))
  eval (ZZModAdd  _ a b :@ loc) = lift2 loc a b (rAddT (0 `zzmod` 0))
  eval (ZZModSub  _ a b :@ loc) = lift2 loc a b (rSubT (0 `zzmod` 0))
  eval (ZZModMult _ a b :@ loc) = lift2 loc a b (rMulT (0 `zzmod` 0))
  eval (ZZModPow  _ a b :@ loc) = lift2 loc a b (rPowT (0 `zzmod` 0))

  eval (ZZModSum   _ ls :@ loc) = lift1 loc ls (rSumT   (0 `zzmod` 0))
  eval (ZZModProd  _ ls :@ loc) = lift1 loc ls (rUProdT (0 `zzmod` 0))



{----------}
{- :Glyph -}
{----------}

class Glyph t where
  toGlyph :: t -> EvalM String


instance Glyph Expr where
  toGlyph expr = case expr of
    IntE   x -> toGlyph x
    StrE   x -> toGlyph x
    BoolE  x -> toGlyph x
    RatE   x -> toGlyph x
    ListE  x -> toGlyph x
    MatE   x -> toGlyph x
    DocE   x -> toGlyph x
    PolyE  x -> toGlyph x
    PermE  x -> toGlyph x
    ZZModE x -> toGlyph x
    MacE   x -> toGlyph x


instance Glyph IntExpr where
  toGlyph (IntConst n :@ _) = return $ show n
  toGlyph x = error $ "toGlyph: IntExpr: " ++ show x


instance Glyph StrExpr where
  toGlyph (StrConst (Text s) :@ _) = return s
  toGlyph x = error $ "toGlyph: StrExpr: " ++ show x


instance Glyph BoolExpr where
  toGlyph (BoolConst True  :@ _) = return "#t"
  toGlyph (BoolConst False :@ _) = return "#f"
  toGlyph x = error $ "toGlyph: BoolExpr: " ++ show x


instance Glyph RatExpr where
  toGlyph (RatConst x :@ _) = return $ show x
  toGlyph x = error $ "toGlyph: RatExpr: " ++ show x


instance Glyph (ZZModExpr Expr) where
  toGlyph (ZZModConst _ a :@ _) = return $ showZZMod a
  toGlyph x = error $ "toGlyph: ZZModExpr: " ++ show x


instance Glyph ListExpr where
  toGlyph (ListConst _ xs :@ _) = do
    ys <- sequence $ map toGlyph xs
    return $ "{" ++ concat (intersperse ";" ys) ++ "}"
  toGlyph x = error $ "toGlyph: ListExpr: " ++ show x


instance Glyph MatExpr where
  toGlyph (MatConst _ m :@ _) = do
    n <- mSeq $ fmap toGlyph m
    case mShowStr n of
      Left err -> reportErr (error "Glyph instance of MatExpr") err
      Right x  -> return x
  toGlyph x = error $ "toGlyph: MatExpr: " ++ show x


instance Glyph PolyExpr where
  toGlyph (PolyConst _ px :@ _) = do
    qx <- polySeq $ mapCoef toGlyph px
    return $ showStrP qx
  toGlyph x = error $ "toGlyph: PolyExpr: " ++ show x


instance Glyph PermExpr where
  toGlyph (PermConst _ px :@ _) = do
    qx <- seqPerm $ mapPerm toGlyph px
    return $ showPerm qx
  toGlyph x = error $ "toGlyph: PermExpr: " ++ show x


instance Glyph MacExpr where
  toGlyph(MacConst _ st ex (amb,_) :@ loc) = do
    old <- getState
    ctx <- toStateT loc st
    f   <- evalWith ex (mergeStores [ctx, old, amb])
    eval f >>= toGlyph
  toGlyph _ = error "toGlyph: MacExpr"


instance Glyph Doc where
  toGlyph (Empty            :@ _) = return ""
  toGlyph (DocText (Text s) :@ _) = return s
  toGlyph x = error $ "toGlyph: Doc: " ++ show x



{---------}
{- :Lift -}
{---------}

lift1
  :: ( Eval x, ToExpr x, Get a
     , Put b, Get y
     , PromoteError err
  ) => Locus -> x -> (a -> Either err b) -> EvalM y
lift1 loc x f = do
  a <- eval x >>= getVal
  b <- tryEvalM loc $ f a
  getVal (put loc b)


lift2
  :: ( Eval x, ToExpr x, Get a
     , Eval y, ToExpr y, Get b
     , Put c, Get z
     , PromoteError err
  ) => Locus -> x -> y -> (a -> b -> Either err c) -> EvalM z
lift2 loc x y f = do
  a <- eval x >>= getVal
  b <- eval y >>= getVal
  c <- tryEvalM loc $ f a b
  getVal (put loc c)


lift3
  :: ( Eval x, ToExpr x, Get a
     , Eval y, ToExpr y, Get b
     , Eval z, ToExpr z, Get c
     , Put d, Get w
     , PromoteError err
  ) => Locus -> x -> y -> z -> (a -> b -> c -> Either err d) -> EvalM w
lift3 loc x y z f = do
  a <- eval x >>= getVal
  b <- eval y >>= getVal
  c <- eval z >>= getVal
  d <- tryEvalM loc $ f a b c
  getVal (put loc d)


lift4
  :: ( Eval x, ToExpr x, Get a
     , Eval y, ToExpr y, Get b
     , Eval z, ToExpr z, Get c
     , Eval w, ToExpr w, Get d
     , Put e, Get u
     , PromoteError err
  ) => Locus -> x -> y -> z -> w -> (a -> b -> c -> d -> Either err e) -> EvalM u
lift4 loc x y z w f = do
  a <- eval x >>= getVal
  b <- eval y >>= getVal
  c <- eval z >>= getVal
  d <- eval w >>= getVal
  e <- tryEvalM loc $ f a b c d
  getVal (put loc e)



{--------------}
{- :Constants -}
{--------------}

hasSameTypeAs :: a -> a -> ()
hasSameTypeAs _ _ = ()

suchThat :: (Monad m) => a -> m a
suchThat = return



zeroZZ :: Integer
zeroZZ = 0

zeroQQ :: Rat
zeroQQ = 0 :/: 1

zeroBB :: Bool
zeroBB = False

zeroMod :: Integer -> ZZModulo
zeroMod n = 0 `zzmod` n



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
