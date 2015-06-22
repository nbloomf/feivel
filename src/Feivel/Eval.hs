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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Feivel.Eval (
 eval, runEvalM, evalToText
) where

{-----------------------------------------------------------}
{- Contents                                                -}
{-  :Eval              :Eval:Expr        :Eval:Utilities   -}
{-    :Eval:IntExpr    :Eval:StrExpr     :Eval:BoolExpr    -}
{-    :Eval:RatExpr    :Eval:ListExpr    :Eval:MacExpr     -}
{-    :Eval:MatExpr    :Eval:Doc         :Eval:PolyExpr    -}
{-                                                         -}
{-  :Typed             :Typed:Expr                         -}
{-    :Typed:IntExpr   :Typed:StrExpr    :Typed:BoolExpr   -}
{-    :Typed:RatExpr   :Typed:ListExpr   :Typed:MacExpr    -}
{-    :Typed:MatExpr   :Typed:Doc        :Typed:PolyExpr   -}
{-                                                         -}
{-  :Get                                                   -}
{-    :Get:IntExpr     :Get:StrExpr      :Get:BoolExpr     -}
{-    :Get:RatExpr     :Get:ListExpr     :Get:MacExpr      -}
{-    :Get:MatExpr     :Get:Doc          :Get:PolyExpr     -}
{-                                                         -}
{-  :Glyph                                                 -}
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
import Feivel.LaTeX

import Data.List (intersperse, (\\), sort, nub)
import Control.Monad (filterM)


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

instance Eval Integer where eval = return
instance Eval String  where eval = return
instance Eval Rat     where eval = return



{--------------}
{- :Eval:Expr -}
{--------------}

instance Eval Expr where
  eval (DocE  x) = fmap toExpr $ eval x
  eval (StrE  x) = fmap toExpr $ eval x
  eval (IntE  x) = fmap toExpr $ eval x
  eval (BoolE x) = fmap toExpr $ eval x
  eval (RatE  x) = fmap toExpr $ eval x
  eval (ListE x) = fmap toExpr $ eval x
  eval (MacE  x) = fmap toExpr $ eval x
  eval (MatE  x) = fmap toExpr $ eval x
  eval (PolyE x) = fmap toExpr $ eval x



{-------------------}
{- :Eval:Utilities -}
{-------------------}

eIfThenElse :: (ToExpr a, Get a, Eval a) => BoolExpr -> a -> a -> EvalM a
eIfThenElse b t f = do
  test  <- eval b >>= getVal
  true  <- eval t >>= getVal
  false <- eval f >>= getVal
  if test then (eval true) else (eval false)

eMacro :: (ToExpr a, Get b, Eval a) => [(Type, Key, Expr)] -> a -> Locus -> EvalM b
eMacro vals mac loc = do
  old <- getState
  ctx <- toStateT loc vals
  (def, e) <- evalWith mac (ctx `mergeState` old) >>= getVal :: EvalM (Store Expr, Expr)
  evalWith e (ctx `mergeState` (def `mergeState` old)) >>= eval >>= getVal



{-----------------}
{- :Eval:IntExpr -}
{-----------------}

instance Eval IntExpr where
  eval (IntConst n :@ loc) = return (IntConst n :@ loc)

  eval (IntVar key :@ loc) = lookupKey loc key >>= get >>= eval

  eval (IntAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [IntExpr] -> Integer -> Either ListErr IntExpr

  eval (IntAtIdx m h k :@ loc) = do
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    p <- eval m >>= getVal :: EvalM (Matrix Integer)
    case (i,j) `mEntryOf` p of
      Left err -> reportErr loc err
      Right x -> return $ IntConst x :@ loc

  eval (IntIfThenElse b t f :@ _) = eIfThenElse b t f

  eval (IntMacro vals mac :@ loc) = eMacro vals mac loc

  eval (IntNeg    a :@ loc) = lift1 loc (rNegT (0::Integer)) a
  eval (IntAbs    a :@ loc) = lift1 loc (rAbsT (0::Integer)) a
  eval (IntSqPart     a :@ loc) = lift1 loc (rSqPartT     (0::Integer)) a
  eval (IntSqFreePart a :@ loc) = lift1 loc (rSqFreePartT (0::Integer)) a
  eval (IntRad        a :@ loc) = lift1 loc (rRadT        (0::Integer)) a

  eval (IntAdd  a b :@ loc) = lift2 loc (rAddT (0::Integer)) a b
  eval (IntSub  a b :@ loc) = lift2 loc (rSubT (0::Integer)) a b
  eval (IntMult a b :@ loc) = lift2 loc (rMulT (0::Integer)) a b
  eval (IntMod  a b :@ loc) = lift2 loc (rRemT (0::Integer)) a b
  eval (IntMin  a b :@ loc) = lift2 loc (rMinT (0::Integer)) a b
  eval (IntMax  a b :@ loc) = lift2 loc (rMaxT (0::Integer)) a b
  eval (IntGCD  a b :@ loc) = lift2 loc (rGCDT (0::Integer)) a b
  eval (IntLCM  a b :@ loc) = lift2 loc (rLCMT (0::Integer)) a b
  eval (IntQuo  a b :@ loc) = lift2 loc (rQuoT (0::Integer)) a b
  eval (IntPow  a b :@ loc) = lift2 loc (rPowT (0::Integer)) a b

  eval (IntChoose a b :@ loc) = lift2 loc (rChooseT (0::Integer)) a b

  eval (RatNumer  p :@ loc) = lift1 loc (ratNum) p
  eval (RatDenom  p :@ loc) = lift1 loc (ratDen) p
  eval (RatFloor  p :@ loc) = lift1 loc (ratFlr) p
  eval (StrLength s :@ loc) = lift1 loc (strLen) s

  eval (MatNumRows m :@ loc) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    case mNumRows n of
      Left err -> reportErr loc err
      Right k -> return $ IntConst k :@ loc

  eval (MatNumCols m :@ loc) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    case mNumCols n of
      Left err -> reportErr loc err
      Right k -> return $ IntConst k :@ loc

  eval (IntRand ls :@ loc) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs
    return $ IntConst r :@ loc

  eval (ListLen ls :@ loc) = do
    xs <- eval ls >>= getVal :: EvalM [Expr]
    return $ IntConst (fromIntegral $ length xs) :@ loc

  eval (IntSum   ls :@ loc) = lift1 loc (rSumT   (0::Integer)) ls
  eval (IntProd  ls :@ loc) = lift1 loc (rUProdT (0::Integer)) ls
  eval (IntMaxim ls :@ loc) = lift1 loc (rMaximT (0::Integer)) ls
  eval (IntMinim ls :@ loc) = lift1 loc (rMinimT (0::Integer)) ls
  eval (IntGCDiv ls :@ loc) = lift1 loc (rGCDsT  (0::Integer)) ls
  eval (IntLCMul ls :@ loc) = lift1 loc (rLCMsT  (0::Integer)) ls

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



{-----------------}
{- :Eval:StrExpr -}
{-----------------}

instance Eval StrExpr where
  eval (StrConst s :@ loc) = return (StrConst s :@ loc)

  eval (StrVar key :@ loc) = lookupKey loc key >>= get >>= eval

  eval (StrAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [StrExpr] -> Integer -> Either ListErr StrExpr

  eval (StrAtIdx m h k :@ loc) = do
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    p <- eval m >>= getVal :: EvalM (Matrix String)
    case (i,j) `mEntryOf` p of
      Left err -> reportErr loc err
      Right x -> return $ StrConst x :@ loc

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
    return $ StrConst (digits d x) :@ loc

  eval (StrRand ls :@ loc) = do
    xs <- eval ls >>= getVal
    r  <- randomElementEvalM xs
    return $ StrConst r :@ loc

  eval (StrTab m :@ loc) = do
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    tab <- tabulateWithM toGlyph n
    return $ StrConst tab :@ loc

  eval (StrTypeOf e :@ loc) = do
    t <- typeOf e
    return $ StrConst (show t) :@ loc

  eval (StrFormat LaTeX e :@ loc) = do
    t <- typeOf e
    case t of
      ZZ -> do
        x <- eval e >>= getVal :: EvalM Integer
        return $ StrConst (latex x) :@ loc
      QQ -> do
        x <- eval e >>= getVal :: EvalM Rat
        return $ StrConst (latex x) :@ loc
      MatOf ZZ -> do
        x <- eval e >>= getVal :: EvalM (Matrix Integer)
        return $ StrConst (latex x) :@ loc
      MatOf QQ -> do
        x <- eval e >>= getVal :: EvalM (Matrix Rat)
        return $ StrConst (latex x) :@ loc
      PolyOver ZZ -> do
        x <- eval e >>= getVal :: EvalM (Poly Integer)
        return $ StrConst (latex x) :@ loc
      PolyOver QQ -> do
        x <- eval e >>= getVal :: EvalM (Poly Rat)
        return $ StrConst (latex x) :@ loc
      _ -> error "StrFormat LaTeX"



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
    case (i,j) `mEntryOf` p of
      Left err -> reportErr loc err
      Right x -> return $ BoolConst x :@ loc

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
        x <- eval a >>= getVal :: EvalM String
        y <- eval b >>= getVal :: EvalM String
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
        x <- eval a >>= getVal :: EvalM String
        y <- eval b >>= getVal :: EvalM String
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
        x <- eval a >>= getVal :: EvalM String
        y <- eval b >>= getVal :: EvalM String
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
        x <- eval a >>= getVal :: EvalM String
        y <- eval b >>= getVal :: EvalM String
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
    case mIsRow p of
      Left err -> reportErr loc err
      Right q  -> return $ BoolConst q :@ loc

  eval (MatIsCol m :@ loc) = do
    p <- eval m >>= getVal :: EvalM (Matrix Expr)
    case mIsCol p of
      Left err -> reportErr loc err
      Right q  -> return $ BoolConst q :@ loc

  eval (MatIsGJForm m :@ loc) = do
    t <- typeOf m
    case t of
      MatOf QQ -> do
        p <- eval m >>= getVal :: EvalM (Matrix Rat)
        case mIsGaussJordanForm p of
          Left err -> reportErr loc err
          Right q  -> return $ BoolConst q :@ loc
      MatOf BB -> do
        p <- eval m >>= getVal :: EvalM (Matrix Bool)
        case mIsGaussJordanForm p of
          Left err -> reportErr loc err
          Right q  -> return $ BoolConst q :@ loc
      _ -> reportErr loc $ NumericMatrixExpected t

  -- Bool
  eval (Neg    a   :@ loc) = lift1 loc (boolNot) a
  eval (Conj   a b :@ loc) = lift2 loc (boolAnd) a b
  eval (Disj   a b :@ loc) = lift2 loc (boolOr)  a b
  eval (Imp    a b :@ loc) = lift2 loc (boolImp) a b

  -- Int
  eval (IntSqFree a :@ loc) = lift1 loc (rIsSqFreeT (0::Integer)) a
  eval (IntDiv a b :@ loc)  = lift2 loc (rDividesT  (0::Integer)) a b

  -- Str
  eval (Matches a b :@ loc) = lift2 loc (strMatch) a b



{-----------------}
{- :Eval:RatExpr -}
{-----------------}

instance Eval RatExpr where
  eval (RatConst p :@ loc) = return $ RatConst p :@ loc -- lift1 loc (ratRed) p

  eval (RatVar key :@ loc) = lookupKey loc key >>= get >>= eval

  eval (RatAtPos a t :@ loc) = lift2 loc (foo) a t
    where foo = listAtPos :: [RatExpr] -> Integer -> Either ListErr RatExpr

  eval (RatAtIdx m h k :@ loc) = do
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    p <- eval m >>= getVal :: EvalM (Matrix Rat)
    case (i,j) `mEntryOf` p of
      Left err -> reportErr loc err
      Right x -> return $ RatConst x :@ loc

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
        case rIntMeanT (0:/:1) xs of
          Right m -> return $ RatConst m :@ loc
          Left err -> reportErr loc err
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        case rMeanT (0:/:1) xs of
          Right m -> return $ RatConst m :@ loc
          Left err -> reportErr loc err
      u -> reportErr loc $ NumericListExpected u

  {- Mean Deviation -}
  eval (RatMeanDev ls :@ loc) = do
    t <- typeOf ls
    case t of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        case rIntMeanDevT (0:/:1) xs of
          Right m -> return $ RatConst m :@ loc
          Left err -> reportErr loc err
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        case rMeanDevT (0:/:1) xs of
          Right m -> return $ RatConst m :@ loc
          Left err -> reportErr loc err
      u -> reportErr loc $ NumericListExpected u

  {- Standard Deviation -}
  eval (RatStdDev ls d :@ loc) = do
    t <- typeOf ls
    k <- eval d >>= getVal
    case t of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        case ratIntStdDev xs k of
          Right m -> return $ RatConst m :@ loc
          Left err -> reportErr loc err
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        case ratStdDev xs k of
          Right m -> return $ RatConst m :@ loc
          Left err -> reportErr loc err
      u -> reportErr loc $ NumericListExpected u

  {- Z-Score -}
  eval (RatZScore x ls d :@ loc) = do
    t <- typeOf ls
    k <- eval d >>= getVal
    y <- eval x >>= getVal
    case t of
      ListOf ZZ -> do
        xs <- eval ls >>= getVal :: EvalM [Integer]
        case ratIntZScore y xs k of
          Right m -> return $ RatConst m :@ loc
          Left err -> reportErr loc err
      ListOf QQ -> do
        xs <- eval ls >>= getVal :: EvalM [Rat]
        case ratZScore y xs k of
          Right m -> return $ RatConst m :@ loc
          Left err -> reportErr loc err
      u -> reportErr loc $ NumericListExpected u


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

  eval (ListAtIdx m h k :@ loc) = do
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    p <- eval m >>= getVal :: EvalM (Matrix ListExpr)
    case (i,j) `mEntryOf` p of
      Left err -> reportErr loc err
      Right x -> return x

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
        xs <- eval a >>= getVal :: EvalM [String]
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
    return $ ListConst t ys :@ loc
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
    case (i,j) `mEntryOf` p of
      Left err -> reportErr loc err
      Right x -> return x

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

  eval (MatAtIdx m h k :@ loc) = do
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    p <- eval m >>= getVal :: EvalM (Matrix MatExpr)
    case (i,j) `mEntryOf` p of
      Left err -> reportErr loc err
      Right x -> return x

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

  eval (MatId t n :@ loc) = do
    k <- eval n >>= getVal :: EvalM Integer
    case t of
      ZZ -> case mEIdT (0::Integer) k of
        Left err -> reportErr loc err
        Right x -> return $ inject loc x
      QQ -> case mEIdT (0:/:1) k of
        Left err -> reportErr loc err
        Right x -> return $ inject loc x
      BB -> case mEIdT False k of
        Left err -> reportErr loc err
        Right x -> return $ inject loc x
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatSwapE t n h k :@ loc) = do
    m <- eval n >>= getVal :: EvalM Integer
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    case t of
      ZZ -> case mESwapT (0::Integer) m i j of
        Left err -> reportErr loc err
        Right x -> return $ inject loc x
      QQ -> case mESwapT (0:/:1) m i j of
        Left err -> reportErr loc err
        Right x -> return $ inject loc x
      BB -> case mESwapT (False) m i j of
        Left err -> reportErr loc err
        Right x -> return $ inject loc x
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatScaleE t n h x :@ loc) = do
    m <- eval n >>= getVal :: EvalM Integer
    k <- eval h >>= getVal :: EvalM Integer
    case t of
      ZZ -> do
        w <- eval x >>= getVal :: EvalM Integer
        case mEScaleT (0::Integer) m k w of
          Left err -> reportErr loc err
          Right y -> return $ inject loc y
      QQ -> do
        w <- eval x >>= getVal :: EvalM Rat
        case mEScaleT (0:/:1) m k w of
          Left err -> reportErr loc err
          Right y -> return $ inject loc y
      BB -> do
        w <- eval x >>= getVal :: EvalM Bool
        case mEScaleT (False) m k w of
          Left err -> reportErr loc err
          Right y -> return $ inject loc y
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatAddE t n h k x :@ loc) = do
    m <- eval n >>= getVal :: EvalM Integer
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    case t of
      ZZ -> do
        w <- eval x >>= getVal :: EvalM Integer
        case mEAddT (0::Integer) m (i,j) w of
          Left err -> reportErr loc err
          Right y -> return $ inject loc y
      QQ -> do
        w <- eval x >>= getVal :: EvalM Rat
        case mEAddT (0:/:1) m (i,j) w of
          Left err -> reportErr loc err
          Right y -> return $ inject loc y
      BB -> do
        w <- eval x >>= getVal :: EvalM Bool
        case mEAddT (False) m (i,j) w of
          Left err -> reportErr loc err
          Right y -> return $ inject loc y
      PolyOver ZZ -> do
        w <- eval x >>= getVal :: EvalM (Poly Integer)
        case mEAddT (constP (0::Integer)) m (i,j) w of
          Left err -> reportErr loc err
          Right y -> return $ inject loc y
      PolyOver QQ -> do
        w <- eval x >>= getVal :: EvalM (Poly Rat)
        case mEAddT (constP (0:/:1)) m (i,j) w of
          Left err -> reportErr loc err
          Right y -> return $ inject loc y
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatShuffleRows m :@ loc) = do
    t <- typeOf m
    x <- eval m >>= getVal :: EvalM (Matrix Expr)
    case mRowsOf x of
      Left err -> reportErr loc err
      Right rs -> do
        ts <- shuffleEvalM rs
        case mVCats ts of
          Left err -> reportErr loc err
          Right n -> case t of
            MatOf u -> return $ MatConst u n :@ loc
            _ -> reportErr loc $ MatrixExpected t

  eval (MatShuffleCols m :@ loc) = do
    t <- typeOf m
    x <- eval m >>= getVal :: EvalM (Matrix Expr)
    case mColsOf x of
      Left err -> reportErr loc err
      Right rs -> do
        ts <- shuffleEvalM rs
        case mHCats ts of
          Left err -> reportErr loc err
          Right n -> case t of
            MatOf u -> return $ MatConst u n :@ loc
            _ -> reportErr loc $ MatrixExpected t

  eval (MatHCat a b :@ loc) = do
    t <- unifyTypesOf loc a b
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    n <- eval b >>= getVal :: EvalM (Matrix Expr)
    case mHCat m n of
      Left err -> reportErr loc err
      Right x -> case t of
        MatOf u -> return $ MatConst u x :@ loc
        _ -> reportErr loc $ MatrixExpected t

  eval (MatVCat a b :@ loc) = do
    t <- unifyTypesOf loc a b
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    n <- eval b >>= getVal :: EvalM (Matrix Expr)
    case mVCat m n of
      Left err -> reportErr loc err
      Right x -> case t of
        MatOf u -> return $ MatConst u x :@ loc
        _ -> reportErr loc $ MatrixExpected t

  eval (MatAdd a b :@ loc) = do
    t <- unifyTypesOf loc a b
    case t of
      MatOf ZZ ->
        lift2 loc (rAddT (mCell (0::Integer))) a b
      MatOf QQ ->
        lift2 loc (rAddT (mCell (0:/:1))) a b
      MatOf BB ->
        lift2 loc (rAddT (mCell False)) a b
      MatOf (PolyOver ZZ) ->
        lift2 loc (rAddT (mCell $ constP (0::Integer))) a b
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatNeg a :@ loc) = do
    t <- typeOf a
    case t of
      MatOf ZZ -> lift1 loc (rNegT (mCell (0::Integer))) a
      MatOf QQ -> lift1 loc (rNegT (mCell (0:/:1))) a
      MatOf BB -> lift1 loc (rNegT (mCell False)) a
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatTrans a :@ loc) = do
    t <- typeOf a
    m <- eval a >>= getVal :: EvalM (Matrix Expr)
    case mTranspose m of
      Left err -> reportErr loc err
      Right p -> case t of
          MatOf u -> return $ MatConst u p :@ loc
          _ -> reportErr loc $ MatrixExpected t 

  eval (MatMul a b :@ loc) = do
    t <- unifyTypesOf loc a b
    case t of
      MatOf ZZ ->
        lift2 loc (rMulT (mCell (0::Integer))) a b
      MatOf QQ ->
        lift2 loc (rMulT (mCell (0:/:1))) a b
      MatOf BB ->
        lift2 loc (rMulT (mCell False)) a b
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatPow m n :@ loc) = do
    t <- typeOf m
    k <- eval n >>= getVal :: EvalM Integer
    case t of
      MatOf ZZ -> do
        q <- eval m >>= getVal :: EvalM (Matrix Integer)
        case rPosPow q k of
          Left err -> reportErr loc err
          Right p -> return $ MatConst ZZ (fmap (IntE . inject loc) p) :@ loc
      MatOf QQ -> do
        q <- eval m >>= getVal :: EvalM (Matrix Rat)
        case rPosPow q k of
          Left err -> reportErr loc err
          Right p -> return $ MatConst QQ (fmap (RatE . inject loc) p) :@ loc
      MatOf BB -> do
        q <- eval m >>= getVal :: EvalM (Matrix Bool)
        case rPosPow q k of
          Left err -> reportErr loc err
          Right p -> return $ MatConst BB (fmap (BoolE . inject loc) p) :@ loc
      _ -> reportErr loc $ NumericMatrixExpected t

  eval (MatSwapRows m a b :@ loc) = do
    t <- typeOf m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal :: EvalM Integer
    j <- eval b >>= getVal :: EvalM Integer
    case mSwapRows i j n of
      Right p -> case t of
        MatOf u -> return $ MatConst u p :@ loc
        _ -> reportErr loc $ MatrixExpected t
      Left err -> reportErr loc err

  eval (MatSwapCols m a b :@ loc) = do
    t <- typeOf m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal :: EvalM Integer
    j <- eval b >>= getVal :: EvalM Integer
    case mSwapCols i j n of
      Right p -> case t of
        MatOf u -> return $ MatConst u p :@ loc
        _ -> reportErr loc $ MatrixExpected t
      Left err -> reportErr loc err

  eval (MatScaleRow m a h :@ loc) = do
    t <- typeOf m
    u <- typeOf a
    i <- eval h >>= getVal :: EvalM Integer
    case unify t (MatOf u) of
      Right (MatOf ZZ) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Integer)
        r <- eval a >>= getVal :: EvalM Integer
        case mScaleRow r i n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst ZZ (fmap (IntE . inject loc) p) :@ loc
      Right (MatOf QQ) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        r <- eval a >>= getVal :: EvalM Rat
        case mScaleRow r i n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst QQ (fmap (RatE . inject loc) p) :@ loc
      Right (MatOf BB) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        r <- eval a >>= getVal :: EvalM Bool
        case mScaleRow r i n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst BB (fmap (BoolE . inject loc) p) :@ loc
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
        case mScaleCol r i n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst ZZ (fmap (IntE . inject loc) p) :@ loc
      Right (MatOf QQ) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        r <- eval a >>= getVal :: EvalM Rat
        case mScaleCol r i n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst QQ (fmap (RatE . inject loc) p) :@ loc
      Right (MatOf BB) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        r <- eval a >>= getVal :: EvalM Bool
        case mScaleCol r i n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst BB (fmap (BoolE . inject loc) p) :@ loc
      Right w -> reportErr loc $ NumericMatrixExpected w
      Left err -> reportErr loc err

  eval (MatAddRow m a h k :@ loc) = do
    t <- typeOf m
    u <- typeOf a
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    case unify t (MatOf u) of
      Right (MatOf ZZ) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Integer)
        r <- eval a >>= getVal :: EvalM Integer
        case mAddRow r i j n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst ZZ (fmap (IntE . inject loc) p) :@ loc
      Right (MatOf QQ) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        r <- eval a >>= getVal :: EvalM Rat
        case mAddRow r i j n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst QQ (fmap (RatE . inject loc) p) :@ loc
      Right (MatOf BB) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        r <- eval a >>= getVal :: EvalM Bool
        case mAddRow r i j n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst BB (fmap (BoolE . inject loc) p) :@ loc
      Right w -> reportErr loc $ NumericMatrixExpected w
      Left err -> reportErr loc err

  eval (MatAddCol m a h k :@ loc) = do
    t <- typeOf m
    u <- typeOf a
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    case unify t (MatOf u) of
      Right (MatOf ZZ) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Integer)
        r <- eval a >>= getVal :: EvalM Integer
        case mAddCol r i j n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst ZZ (fmap (IntE . inject loc) p) :@ loc
      Right (MatOf QQ) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        r <- eval a >>= getVal :: EvalM Rat
        case mAddCol r i j n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst QQ (fmap (RatE . inject loc) p) :@ loc
      Right (MatOf BB) -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        r <- eval a >>= getVal :: EvalM Bool
        case mAddCol r i j n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst BB (fmap (BoolE . inject loc) p) :@ loc
      Right w -> reportErr loc $ NumericMatrixExpected w
      Left err -> reportErr loc err

  eval (MatDelRow m a :@ loc) = do
    t <- typeOf m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal :: EvalM Integer
    case mDelRow n i of
      Right p -> case t of
        MatOf u -> return $ MatConst u p :@ loc
        _ -> reportErr loc $ MatrixExpected t
      Left err -> reportErr loc err

  eval (MatDelCol m a :@ loc) = do
    t <- typeOf m
    n <- eval m >>= getVal :: EvalM (Matrix Expr)
    i <- eval a >>= getVal :: EvalM Integer
    case mDelCol n i of
      Right p -> case t of
        MatOf u -> return $ MatConst u p :@ loc
        _ -> reportErr loc $ MatrixExpected t
      Left err -> reportErr loc err

  eval (MatGJForm m :@ loc) = do
    t <- typeOf m
    case t of
      MatOf QQ -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        case mGJForm n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst QQ (fmap (RatE . inject loc) p) :@ loc
      MatOf BB -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        case mGJForm n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst BB (fmap (BoolE . inject loc) p) :@ loc
      _ -> reportErr loc $ FieldMatrixExpected t

  eval (MatGJFactor m :@ loc) = do
    t <- typeOf m
    case t of
      MatOf QQ -> do
        n <- eval m >>= getVal :: EvalM (Matrix Rat)
        case mGJFactor n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst QQ (fmap (RatE . inject loc) p) :@ loc
      MatOf BB -> do
        n <- eval m >>= getVal :: EvalM (Matrix Bool)
        case mGJFactor n of
          Left err -> reportErr loc err
          Right p -> return $ MatConst BB (fmap (BoolE . inject loc) p) :@ loc
      _ -> reportErr loc $ FieldMatrixExpected t



{-------------}
{- :Eval:Doc -}
{-------------}

instance Eval Doc where
  eval (Empty :@ loc)     = return (Empty :@ loc)
  eval (DocText s :@ loc) = return (DocText s :@ loc)
  eval (Escaped c :@ loc) = return (DocText [c] :@ loc)

  eval (ShowState :@ loc) = do
    st <- getState
    return $ DocText (show st) :@ loc

  eval (NakedKey k :@ loc) = do
    expr <- lookupKey loc k
    let foo s = return (DocText s :@ loc)
    case expr of
      DocE  x -> eval x
      IntE  x -> eval x >>= toGlyph >>= foo
      StrE  x -> eval x >>= toGlyph >>= foo
      BoolE x -> eval x >>= toGlyph >>= foo
      RatE  x -> eval x >>= toGlyph >>= foo
      ListE x -> eval x >>= toGlyph >>= foo
      MacE  x -> eval x >>= toGlyph >>= foo
      MatE  x -> eval x >>= toGlyph >>= foo
      PolyE x -> eval x >>= toGlyph >>= foo

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
    IntE  e -> eval e >>= toGlyph >>= \x -> return (DocText x :@ loc)
    StrE  e -> eval e >>= toGlyph >>= \x -> return (DocText x :@ loc)
    RatE  e -> eval e >>= toGlyph >>= \x -> return (DocText x :@ loc)
    BoolE e -> eval e >>= toGlyph >>= \x -> return (DocText x :@ loc)
    ListE e -> eval e >>= toGlyph >>= \x -> return (DocText x :@ loc)
    MatE  e -> eval e >>= toGlyph >>= \x -> return (DocText x :@ loc)
    MacE  e -> eval e >>= toGlyph >>= \x -> return (DocText x :@ loc)
    DocE  e -> eval e >>= toGlyph >>= \x -> return (DocText x :@ loc)
    PolyE e -> eval e >>= toGlyph >>= \x -> return (DocText x :@ loc)


  eval (Cat [] :@ loc) = return $ Empty :@ loc
  eval (Cat ts :@ loc) = do
    exprs <- sequence [eval t >>= getVal | t <- ts]
    return $ DocText (concat exprs) :@ loc

  eval (CatPar [] :@ loc) = return $ Empty :@ loc
  eval (CatPar ts :@ loc) = do
    let foo x = do
          st <- getState
          y <- eval x >>= getVal
          putState st
          return y
    exprs <- sequence $ map foo ts
    return $ DocText (concat exprs) :@ loc


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


  eval (Pull d Nothing rest :@ _) = do
    dFile <- eval d >>= getVal
    mergeDataFromLibrary dFile
    eval rest

  eval (Pull d (Just n) rest :@ _) = do
    dFile <- eval d >>= getVal
    name  <- eval n >>= getVal
    mergeQualifiedDataFromLibrary dFile name
    eval rest


  eval (LetIn key val expr :@ loc) = do
    defineKey key val loc
    t <- eval expr
    undefineKey key
    return t

  eval (Bail s :@ loc) = (eval s) >>= getVal >>= (reportErr loc . BailMessage)

  eval (Alt [] :@ loc) = return $ Empty :@ loc
  eval (Alt ts :@ _)   = randomElementEvalM ts >>= eval

  eval (Shuffle xs :@ loc) = do
    x <- shuffleEvalM xs
    eval $ Cat x :@ loc


  eval (Input t :@ loc) = do
    state  <- getState
    tFile  <- eval t >>= getVal
    expr   <- readTemplateFromLibrary tFile
    result <- eval expr >>= getVal :: EvalM String
    putState state
    return $ DocText (init result) :@ loc
    

  eval (Splice t Nothing _ :@ _) = do
    old   <- getState
    tFile <- eval t >>= getVal
    expr  <- readTemplateFromLibrary tFile
    undefineKeys
    result <- eval expr
    putState old
    return result

  eval (Splice t (Just (Left d)) Nothing :@ _) = do
    old    <- getState
    tFile  <- eval t >>= getVal
    expr   <- readTemplateFromLibrary tFile
    dFile  <- eval d >>= getVal
    readRandomDataFromLibrary dFile
    result <- eval expr
    putState old
    return result

  eval (Splice t (Just (Left d)) (Just fmt) :@ _) = do
    old    <- getState
    tFile  <- eval t >>= getVal
    expr   <- readTemplateFromLibrary tFile
    dFile  <- eval d >>= getVal :: EvalM String
    readRandomFormattedDataFromLibrary dFile fmt
    result <- eval expr
    putState old
    return result

  eval (Splice t (Just (Right d)) Nothing :@ _) = do
    old   <- getState
    tFile <- eval t >>= getVal
    expr  <- readTemplateFromLibrary tFile
    dFile <- eval d >>= getVal :: EvalM String
    fmt   <- lookupDataFormat
    readRandomFormattedDataFromLibrary dFile fmt
    result <- eval expr
    putState old
    return result

  eval (Splice t (Just (Right d)) (Just fmt) :@ _) = do
    old   <- getState
    tFile <- eval t >>= getVal
    expr  <- readTemplateFromLibrary tFile
    dFile <- eval d >>= getVal :: EvalM String
    readRandomFormattedDataFromLibrary dFile fmt
    result <- eval expr
    putState old
    return result

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

  eval (PolyAtIdx m h k :@ loc) = do
    i <- eval h >>= getVal :: EvalM Integer
    j <- eval k >>= getVal :: EvalM Integer
    p <- eval m >>= getVal :: EvalM (Matrix PolyExpr)
    case (i,j) `mEntryOf` p of
      Left err -> reportErr loc err
      Right x -> return x

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
      _ -> reportErr loc $ NumericPolynomialExpected t



{----------}
{- :Typed -}
{----------}

class Typed t where
  typeOf :: t -> EvalM Type


unifyTypesOf :: (Typed a, Typed b) => Locus -> a -> b -> EvalM Type
unifyTypesOf loc a b = do
  ta <- typeOf a
  tb <- typeOf b
  case unify ta tb of
    Left err -> reportErr loc err
    Right t -> return t

expectType :: (Typed a) => Locus -> Type -> a -> EvalM Type
expectType loc t x = do
  u <- typeOf x
  if t == u
    then return u
    else reportErr loc $ TypeMismatch t u

sameType :: (Typed a, Typed b) => Locus -> a -> b -> EvalM Type
sameType loc a b = do
  ta <- typeOf a
  tb <- typeOf b
  if ta == tb
    then return ta
    else reportErr loc $ TypeMismatch ta tb



{---------------}
{- :Typed:Expr -}
{---------------}

instance Typed Expr where
  typeOf (StrE  x) = typeOf x
  typeOf (IntE  x) = typeOf x
  typeOf (RatE  x) = typeOf x
  typeOf (BoolE x) = typeOf x
  typeOf (ListE x) = typeOf x
  typeOf (MacE  x) = typeOf x
  typeOf (DocE  x) = typeOf x
  typeOf (MatE  x) = typeOf x
  typeOf (PolyE x) = typeOf x



{------------------}
{- :Typed:IntExpr -}
{------------------}

instance Typed IntExpr where
  typeOf (IntVar key :@ loc) =
    lookupKey loc key >>= expectType loc ZZ
  typeOf _ = return ZZ



{------------------}
{- :Typed:StrExpr -}
{------------------}

instance Typed StrExpr where
  typeOf (StrVar key :@ loc) =
    lookupKey loc key >>= expectType loc SS
  typeOf _ = return SS



{-------------------}
{- :Typed:BoolExpr -}
{-------------------}

instance Typed BoolExpr where
  typeOf (BoolVar key :@ loc) =
    lookupKey loc key >>= expectType loc BB
  typeOf _ = return BB



{------------------}
{- :Typed:RatExpr -}
{------------------}

instance Typed RatExpr where
  typeOf (RatVar key :@ loc) =
    lookupKey loc key >>= expectType loc QQ
  typeOf _ = return QQ



{-------------------}
{- :Typed:ListExpr -}
{-------------------}

instance Typed ListExpr where
  typeOf (ListVar key :@ loc) = do
    expr <- lookupKey loc key
    t <- typeOf expr
    case t of
      ListOf x -> return $ ListOf x
      u -> reportErr loc $ ListExpected u

  typeOf (ListMacro _ x :@ _) = typeOf x

  typeOf (ListConst t _ :@ _) = return $ ListOf t

  typeOf (ListIfThenElse _ t f :@ loc) = do
    a <- typeOf t
    b <- typeOf f
    case unify a b of
      Right u -> return u
      Left err -> reportErr loc err

  typeOf (ListAtPos x _ :@ loc) = do
    t <- typeOf x
    case t of
      MatOf (ListOf u) -> return $ ListOf u
      _ -> reportErr loc $ ListListExpected t

  typeOf (ListAtIdx x _ _ :@ loc) = do
    t <- typeOf x
    case t of
      ListOf (ListOf u) -> return $ ListOf u
      _ -> reportErr loc $ ListMatrixExpected t

  typeOf (ListCat  a b :@ loc) = sameType loc a b
  typeOf (ListToss a b :@ loc) = sameType loc a b

  typeOf (ListShuffle a :@ _) = typeOf a

  typeOf (ListRev x :@ _) = typeOf x
  typeOf (ListSort x :@ _) = typeOf x
  typeOf (ListUniq x :@ _) = typeOf x

  typeOf (ListRange _ _ :@ _) = return $ ListOf ZZ

  typeOf (ListBuilder e _ :@ _) = do
    t <- typeOf e
    return $ ListOf t

  typeOf (ListRand xs :@ loc) = do
    t <- typeOf xs
    case t of
      ListOf (ListOf u) -> return $ ListOf u
      _ -> reportErr loc $ ListListExpected t

  typeOf (ListChoose _ xs :@ _) = typeOf xs

  typeOf (ListChoices _ xs :@ _) = do
    t <- typeOf xs
    return $ ListOf t

  typeOf (ListFilter _ _ xs :@ _) = typeOf xs



{------------------}
{- :Typed:MacExpr -}
{------------------}

instance Typed MacExpr where
  typeOf (MacConst typ _ _ _ :@ _) = return $ MacTo typ

  typeOf (MacVar key :@ loc) = do
    expr <- lookupKey loc key
    t <- typeOf expr
    case t of
      MacTo x -> return $ MacTo x
      u -> reportErr loc $ MacroExpected u

  typeOf (MacMacro _ expr :@ _) = do
    t <- typeOf expr
    return $ MacTo t

  typeOf (MacAtPos ms _ :@ loc) = do
    t <- typeOf ms
    case t of
      ListOf (MacTo u) -> return $ MacTo u
      _ -> reportErr loc $ MacroListExpected t

  typeOf (MacAtIdx ms _ _ :@ loc) = do
    t <- typeOf ms
    case t of
      MatOf (MacTo u) -> return $ MacTo u
      _ -> reportErr loc $ MacroMatrixExpected t

  typeOf (MacRand ms :@ loc) = do
    t <- typeOf ms
    case t of
      ListOf (MacTo u) -> return $ MacTo u
      _ -> reportErr loc $ MacroListExpected t

  typeOf (MacIfThenElse _ t f :@ loc) = do
    a <- typeOf t
    b <- typeOf f
    case unify a b of
      Right u -> return u
      Left err -> reportErr loc err



{------------------}
{- :Typed:MatExpr -}
{------------------}

instance Typed MatExpr where
  typeOf (MatVar key :@ loc) = do
    expr <- lookupKey loc key
    t <- typeOf expr
    case t of
      MatOf x -> return $ MatOf x
      u -> reportErr loc $ MatrixExpected u

  typeOf (MatMacro _ x :@ _) = typeOf x

  typeOf (MatAtPos m _ :@ loc) = do
    t <- typeOf m
    case t of
      MatOf (MatOf u) -> return $ MatOf u
      _ -> reportErr loc $ MatrixListExpected t

  typeOf (MatAtIdx m _ _ :@ loc) = do
    t <- typeOf m
    case t of
      ListOf (MatOf u) -> return $ MatOf u
      _ -> reportErr loc $ MatrixMatrixExpected t

  typeOf (MatIfThenElse _ t f :@ loc) = do
    a <- typeOf t
    b <- typeOf f
    case unify a b of
      Right u -> return u
      Left err -> reportErr loc err

  typeOf (MatConst t _ :@ _) = return $ MatOf t

  typeOf (MatId t _ :@ _) = return $ MatOf t
  typeOf (MatSwapE t _ _ _ :@ _) = return $ MatOf t
  typeOf (MatScaleE t _ _ _ :@ _) = return $ MatOf t
  typeOf (MatAddE t _ _ _ _ :@ _) = return $ MatOf t

  typeOf (MatHCat a b :@ loc) = sameType loc a b
  typeOf (MatVCat a b :@ loc) = sameType loc a b
  typeOf (MatAdd  a b :@ loc) = sameType loc a b
  typeOf (MatMul  a b :@ loc) = sameType loc a b
  typeOf (MatPow m _ :@ _) = typeOf m
  typeOf (MatTrans m :@ _) = typeOf m
  typeOf (MatNeg m :@ _) = typeOf m

  typeOf (MatSwapRows m _ _ :@ _) = typeOf m
  typeOf (MatSwapCols m _ _ :@ _) = typeOf m
  typeOf (MatScaleRow m _ _ :@ _) = typeOf m
  typeOf (MatScaleCol m _ _ :@ _) = typeOf m
  typeOf (MatAddRow m _ _ _ :@ _) = typeOf m
  typeOf (MatAddCol m _ _ _ :@ _) = typeOf m
  typeOf (MatDelRow m _ :@ _) = typeOf m
  typeOf (MatDelCol m _ :@ _) = typeOf m

  typeOf (MatShuffleRows m :@ _) = typeOf m
  typeOf (MatShuffleCols m :@ _) = typeOf m

  typeOf (MatGJForm   m :@ _) = typeOf m
  typeOf (MatGJFactor m :@ _) = typeOf m

  typeOf (MatRand ms :@ loc) = do
    t <- typeOf ms
    case t of
      ListOf (MatOf u) -> return $ MatOf u
      _ -> reportErr loc $ MatrixListExpected t



{--------------}
{- :Typed:Doc -}
{--------------}

instance Typed Doc where
  typeOf _ = return DD



{-------------------}
{- :Typed:PolyExpr -}
{-------------------}

instance Typed PolyExpr where
  typeOf (PolyVar key :@ loc) = do
    expr <- lookupKey loc key
    t <- typeOf expr
    case t of
      PolyOver x -> return $ PolyOver x
      u -> reportErr loc $ PolynomialExpected u

  typeOf (PolyMacro _ x :@ _) = typeOf x

  typeOf (PolyConst t _ :@ _) = return $ PolyOver t

  typeOf (PolyAdd  a b :@ loc) = sameType loc a b
  typeOf (PolySub  a b :@ loc) = sameType loc a b
  typeOf (PolyMul  a b :@ loc) = sameType loc a b
  typeOf (PolyNeg  a :@ _) = typeOf a
  typeOf (PolyPow  a _ :@ _) = typeOf a

  typeOf (PolyAtPos m _ :@ loc) = do
    t <- typeOf m
    case t of
      MatOf (PolyOver u) -> return $ PolyOver u
      _ -> reportErr loc $ PolynomialListExpected t

  typeOf (PolyAtIdx m _ _ :@ loc) = do
    t <- typeOf m
    case t of
      ListOf (PolyOver u) -> return $ PolyOver u
      _ -> reportErr loc $ PolynomialMatrixExpected t

  typeOf (PolyIfThenElse _ t f :@ loc) = do
    a <- typeOf t
    b <- typeOf f
    case unify a b of
      Right u -> return u
      Left err -> reportErr loc err

  typeOf (PolyRand xs :@ loc) = do
    t <- typeOf xs
    case t of
      ListOf (PolyOver u) -> return $ PolyOver u
      _ -> reportErr loc $ PolynomialListExpected t



{--------}
{- :Get -}
{--------}

class Get a where
  get :: Expr -> EvalM a

getVal :: (ToExpr a, Get b) => a -> EvalM b
getVal x = get (toExpr x)

instance Get Expr where
  get expr = return expr


{----------------}
{- :Get:IntExpr -}
{----------------}

instance Get IntExpr where
  get expr = do
    x <- eval expr
    case x of
      IntE y -> return y
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ TypeMismatch ZZ t

instance Get Integer where
  get expr = do
    x <- eval expr >>= get :: EvalM IntExpr
    case x of
      IntConst k :@ _ -> return k
      v -> reportErr (locusOf v) UnevaluatedExpression

instance Get [IntExpr] where
  get expr = do
    x <- eval expr
    case x of
      ListE (ListConst ZZ xs :@ _) -> sequence $ map get xs
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ TypeMismatch (ListOf ZZ) t

instance Get [Integer] where
  get expr = do
    xs <- eval expr >>= get :: EvalM [IntExpr]
    sequence $ map getVal xs


{----------------}
{- :Get:StrExpr -}
{----------------}

instance Get StrExpr where
  get expr = do
    x <- eval expr
    case x of
      StrE y -> return y
      DocE y -> do  -- this shouldn't be necessary. why is it?
        str <- toGlyph y
        return $ StrConst str :@ (locusOf y)
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ TypeMismatch SS t

instance Get String where
  get expr = do
    x <- eval expr >>= get :: EvalM StrExpr
    case x of
      StrConst s :@ _ -> return s
      v -> reportErr (locusOf v) UnevaluatedExpression

instance Get [StrExpr] where
  get expr = do
    x <- eval expr
    case x of
      ListE (ListConst SS xs :@ _) -> sequence $ map get xs
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ TypeMismatch (ListOf SS) t

instance Get [String] where
  get expr = do
    xs <- eval expr >>= get :: EvalM [StrExpr]
    sequence $ map getVal xs


{-----------------}
{- :Get:BoolExpr -}
{-----------------}

instance Get BoolExpr where
  get expr = do
    x <- eval expr
    case x of
      BoolE y -> return y
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ TypeMismatch BB t

instance Get Bool where
  get expr = do
    x <- eval expr >>= get :: EvalM BoolExpr
    case x of
      BoolConst b :@ _ -> return b
      v -> reportErr (locusOf v) UnevaluatedExpression

instance Get [BoolExpr] where
  get expr = do
    x <- eval expr
    case x of
      ListE (ListConst BB xs :@ _) -> sequence $ map get xs
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ TypeMismatch (ListOf BB) t

instance Get [Bool] where
  get expr = do
    xs <- eval expr >>= get :: EvalM [BoolExpr]
    sequence $ map getVal xs


{----------------}
{- :Get:RatExpr -}
{----------------}

instance Get RatExpr where
  get expr = do
    x <- eval expr
    case x of
      RatE y -> return y
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ TypeMismatch QQ t

instance Get Rat where
  get expr = do
    x <- eval expr >>= get :: EvalM RatExpr
    case x of
      RatConst r :@ _ -> return r
      v -> reportErr (locusOf v) UnevaluatedExpression

instance Get [RatExpr] where
  get expr = do
    x <- eval expr
    case x of
      ListE (ListConst QQ xs :@ _) -> sequence $ map get xs
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ TypeMismatch (ListOf QQ) t

instance Get [Rat] where
  get expr = do
    xs <- eval expr >>= get :: EvalM [RatExpr]
    sequence $ map getVal xs


{-----------------}
{- :Get:ListExpr -}
{-----------------}

instance Get ListExpr where
  get expr = do
    x <- eval expr
    case x of
      ListE y -> return y
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ ListExpected t

instance Get [Expr] where
  get expr = do
    x <- eval expr
    case x of
      ListE (ListConst _ xs :@ _) -> return xs
      ListE v -> reportErr (locusOf v) UnevaluatedExpression
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ ListExpected t

instance Get [ListExpr] where
  get expr = do
    x <- eval expr >>= get :: EvalM [Expr]
    sequence $ fmap get x

instance Get [MatExpr] where
  get expr = do
    x <- eval expr >>= get :: EvalM [Expr]
    sequence $ fmap get x

instance Get [MacExpr] where
  get expr = do
    x <- eval expr >>= get :: EvalM [Expr]
    sequence $ fmap get x

instance Get [PolyExpr] where
  get expr = do
    x <- eval expr >>= get :: EvalM [Expr]
    sequence $ fmap get x



{----------------}
{- :Get:MacExpr -}
{----------------}

instance Get MacExpr where
  get expr = do
    x <- eval expr
    case x of
      MacE y -> return y
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ MacroExpected t


instance Get (Store Expr, Expr) where
  get expr = do
    x <- eval expr
    case x of
      MacE (MacConst _ vals y (amb,_) :@ loc) -> do
        st <- toStateT loc vals
        return (mergeState st amb, y)
      MacE v -> reportErr (locusOf v) UnevaluatedExpression
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ MacroExpected t


{----------------}
{- :Get:MatExpr -}
{----------------}

instance Get MatExpr where
  get expr = do
    x <- eval expr
    case x of
      MatE m -> return m
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ MatrixExpected t

instance Get (Matrix Expr) where
  get expr = do
    x <- eval expr
    case x of
      MatE (MatConst _ m :@ _) -> return m
      MatE v -> reportErr (locusOf v) UnevaluatedExpression
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ MatrixExpected t

instance Get (Matrix Integer) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Matrix Expr)
    mSeq $ fmap get x

instance Get (Matrix Rat) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Matrix Expr)
    mSeq $ fmap get x

instance Get (Matrix String) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Matrix Expr)
    mSeq $ fmap get x

instance Get (Matrix Bool) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Matrix Expr)
    mSeq $ fmap get x

instance Get (Matrix ListExpr) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Matrix Expr)
    mSeq $ fmap get x

instance Get (Matrix MatExpr) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Matrix Expr)
    mSeq $ fmap get x

instance Get (Matrix MacExpr) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Matrix Expr)
    mSeq $ fmap get x

instance Get (Matrix PolyExpr) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Matrix Expr)
    mSeq $ fmap get x

instance Get (Matrix (Poly Integer)) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Matrix Expr)
    mSeq $ fmap get x

instance Get (Matrix (Poly Rat)) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Matrix Expr)
    mSeq $ fmap get x

instance Get (Matrix (Poly Bool)) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Matrix Expr)
    mSeq $ fmap get x


{------------}
{- :Get:Doc -}
{------------}

instance Get Doc where
  get expr = do
    x <- eval expr
    case x of
      DocE y -> return y
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ TypeMismatch DD t


{-----------------}
{- :Get:PolyExpr -}
{-----------------}

instance Get PolyExpr where
  get expr = do
    x <- eval expr
    case x of
      PolyE m -> return m
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ PolynomialExpected t

instance Get (Poly Expr) where
  get expr = do
    x <- eval expr
    case x of
      PolyE (PolyConst _ m :@ _) -> return m
      PolyE v -> reportErr (locusOf v) UnevaluatedExpression
      v -> do
        t <- typeOf v
        reportErr (locusOf v) $ PolynomialExpected t

instance Get (Poly Integer) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Poly Expr)
    polySeq $ fmap get x

instance Get (Poly Rat) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Poly Expr)
    polySeq $ fmap get x

instance Get (Poly Bool) where
  get expr = do
    x <- eval expr >>= get :: EvalM (Poly Expr)
    polySeq $ fmap get x



{----------}
{- :Glyph -}
{----------}

class Glyph t where
  toGlyph :: t -> EvalM String


instance Glyph Expr where
  toGlyph expr = case expr of
    IntE  x -> toGlyph x
    StrE  x -> toGlyph x
    BoolE x -> toGlyph x
    RatE  x -> toGlyph x
    ListE x -> toGlyph x
    MacE  x -> toGlyph x
    MatE  x -> toGlyph x
    DocE  x -> toGlyph x
    PolyE x -> toGlyph x

instance Glyph IntExpr where
  toGlyph expr = do
    n <- eval expr >>= getVal :: EvalM Integer
    return $ show n

instance Glyph StrExpr where
  toGlyph expr = do
    s <- eval expr >>= getVal :: EvalM String
    return s

instance Glyph BoolExpr where
  toGlyph expr = do
    b <- eval expr >>= getVal :: EvalM Bool
    return $ foo b
      where
        foo True  = "#t"
        foo False = "#f"

instance Glyph RatExpr where
  toGlyph expr = do
    r <- eval expr >>= getVal :: EvalM Rat
    return $ show r

instance Glyph ListExpr where
  toGlyph expr = do
    xs <- eval expr >>= getVal :: EvalM [Expr]
    ys <- sequence [toGlyph x | x <- xs]
    return $ "{" ++ concat (intersperse ";" ys) ++ "}"

instance Glyph MacExpr where
  toGlyph expr = do
    m <- eval expr >>= getVal :: EvalM MacExpr
    case m of
      MacConst _ st ex (amb,_) :@ loc -> do
        old <- getState
        ctx <- toStateT loc st
        f   <- evalWith ex (ctx `mergeState` old `mergeState` amb)
        eval f >>= toGlyph
      _ -> reportErr (locusOf m) UnevaluatedExpression 

instance Glyph MatExpr where
  toGlyph expr = do
    m <- eval expr >>= getVal :: EvalM (Matrix Expr)
    n <- mSeq $ fmap toGlyph m
    case mShowStr n of
      Left err -> reportErr (error "Glyph instance of MatExpr") err
      Right x  -> return x

instance Glyph PolyExpr where
  toGlyph expr = do
    p <- eval expr >>= getVal :: EvalM (Poly Expr)
    q <- polySeq $ mapCoef toGlyph p
    return $ showStrP q

instance Glyph Doc where
  toGlyph expr = do
    d <- eval expr >>= getVal :: EvalM Doc
    case d of
      Empty :@ _     -> return ""
      DocText s :@ _ -> return s
      a              -> reportErr (locusOf a) UnevaluatedExpression


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
  inject loc x = StrConst x :@ loc

instance Inject Bool BoolExpr where
  inject loc x = BoolConst x :@ loc

instance Inject Rat RatExpr where
  inject loc x = RatConst x :@ loc


{- :ListExpr -}

instance Inject [Integer] ListExpr where
  inject loc xs = (ListConst ZZ $ map foo xs) :@ loc
    where foo x = toExpr (IntConst x :@ loc)

instance Inject [String] ListExpr where
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


instance Inject String Doc where
  inject loc = \x -> DocText x :@ loc


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
  case f a of
    Left err -> reportErr loc err
    Right b  -> return $ inject loc b


lift2
  :: ( Eval x, ToExpr x, Get a
     , Eval y, ToExpr y, Get b
     , Inject c z, HasLocus z
     , PromoteError err
  ) => Locus -> (a -> b -> Either err c) -> x -> y -> EvalM z
lift2 loc f x y = do
  a <- eval x >>= getVal
  b <- eval y >>= getVal
  case f a b of
    Left err -> reportErr loc err
    Right c  -> return $ inject loc c


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

toStateT :: Locus -> [(Type, Key, Expr)] -> EvalM (Store Expr)
toStateT loc vs = do
  ws <- sequence $ map (checkType loc) vs
  toState loc ws
    where
      checkType :: Locus -> (Type, Key, Expr) -> EvalM (Key, Expr)
      checkType loc' (t,k,v) = do
        tv <- typeOf v
        if tv == t
          then return (k,v)
          else reportErr loc' $ TypeMismatch t tv

