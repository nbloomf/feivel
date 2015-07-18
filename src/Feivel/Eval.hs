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
{-                     :Eval:StrExpr                       -}
{-                     :Eval:ListExpr    :Eval:MacExpr     -}
{-                     :Eval:Doc                           -}
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

import Feivel.Eval.Eval
import Feivel.Eval.Util

import Feivel.Eval.ZZMod ()
import Feivel.Eval.Perm  ()
import Feivel.Eval.Rat   ()
import Feivel.Eval.Poly  ()
import Feivel.Eval.Mat   ()
import Feivel.Eval.Bool  ()
import Feivel.Eval.Int   ()

import Data.List (intersperse, (\\), sort, nub, permutations)
import Control.Monad (filterM)


{---------}
{- :Eval -}
{---------}



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

evalToGlyph :: (ToExpr a) => a -> EvalM String
evalToGlyph x = eval (toExpr x) >>= toGlyph




{-----------------}
{- :Eval:StrExpr -}
{-----------------}

instance Eval (StrExpr Expr) where
  eval (StrConst s :@ loc) = return (StrConst s :@ loc)

  {- :Common -}
  eval (StrVar key :@ loc)        = eKey key loc
  eval (StrAtIdx m h k :@ loc)    = eAtIdx m h k loc
  eval (StrIfThenElse b t f :@ _) = eIfThenElse b t f
  eval (StrMacro vals mac :@ loc) = eMacro vals mac loc

  eval (StrAtPos a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [StrExpr Expr] -> Integer -> Either ListErr (StrExpr Expr)

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
    a <- eval n >>= getVal :: EvalM (IntExpr Expr)
    s <- eval a >>= toGlyph
    return $ StrConst (Text s) :@ loc








{------------------}
{- :Eval:ListExpr -}
{------------------}

instance Eval (ListExpr Expr) where
  eval (ListConst t xs :@ loc) = do
    ys <- sequence $ map eval xs
    return $ ListConst t ys :@ loc

  {- :Common -}
  eval (ListVar _ key :@ loc)        = eKey key loc
  eval (ListAtIdx _ m h k :@ loc)    = eAtIdx m h k loc
  eval (ListMacro _ vals mac :@ loc) = eMacro vals mac loc
  eval (ListIfThenElse _ b t f :@ _) = eIfThenElse b t f

  eval (ListAtPos _ a t :@ loc) = lift2 loc a t (foo)
    where foo = listAtPos :: [ListExpr Expr] -> Integer -> Either ListErr (ListExpr Expr)

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
    randomElementEvalM xs >>= getVal :: EvalM (ListExpr Expr)

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
        bar :: Store Expr -> [ListGuard Expr] -> EvalM [Store Expr]
        bar st []     = return [st]
        bar st (h:hs) = do
          xs <- foo st h
          fmap concat $ sequence $ [bar x hs | x <- xs]
        
        foo :: Store Expr -> (ListGuard Expr) -> EvalM [Store Expr]
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

instance Eval (MacExpr Expr) where
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
    where foo = listAtPos :: [MacExpr Expr] -> Integer -> Either ListErr (MacExpr Expr)

  eval (MacRand _ ls :@ _) = do
    xs <- eval ls >>= getVal
    randomElementEvalM xs





{-------------}
{- :Eval:Doc -}
{-------------}

instance Eval (Doc Expr) where
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


instance Glyph (IntExpr Expr) where
  toGlyph (IntConst n :@ _) = return $ show n
  toGlyph x = error $ "toGlyph: IntExpr: " ++ show x


instance Glyph (StrExpr Expr) where
  toGlyph (StrConst (Text s) :@ _) = return s
  toGlyph x = error $ "toGlyph: StrExpr: " ++ show x


instance Glyph (BoolExpr Expr) where
  toGlyph (BoolConst True  :@ _) = return "#t"
  toGlyph (BoolConst False :@ _) = return "#f"
  toGlyph x = error $ "toGlyph: BoolExpr: " ++ show x


instance Glyph (RatExpr Expr) where
  toGlyph (RatConst x :@ _) = return $ show x
  toGlyph x = error $ "toGlyph: RatExpr: " ++ show x


instance Glyph (ZZModExpr Expr) where
  toGlyph (ZZModConst _ a :@ _) = return $ showZZMod a
  toGlyph x = error $ "toGlyph: ZZModExpr: " ++ show x


instance Glyph (ListExpr Expr) where
  toGlyph (ListConst _ xs :@ _) = do
    ys <- sequence $ map toGlyph xs
    return $ "{" ++ concat (intersperse ";" ys) ++ "}"
  toGlyph x = error $ "toGlyph: ListExpr: " ++ show x


instance Glyph (MatExpr Expr) where
  toGlyph (MatConst _ m :@ _) = do
    n <- mSeq $ fmap toGlyph m
    case mShowStr n of
      Left err -> reportErr (error "Glyph instance of MatExpr") err
      Right x  -> return x
  toGlyph x = error $ "toGlyph: MatExpr: " ++ show x


instance Glyph (PolyExpr Expr) where
  toGlyph (PolyConst _ px :@ _) = do
    qx <- polySeq $ mapCoef toGlyph px
    return $ showStrP qx
  toGlyph x = error $ "toGlyph: PolyExpr: " ++ show x


instance Glyph (PermExpr Expr) where
  toGlyph (PermConst _ px :@ _) = do
    qx <- seqPerm $ mapPerm toGlyph px
    return $ showPerm qx
  toGlyph x = error $ "toGlyph: PermExpr: " ++ show x


instance Glyph (MacExpr Expr) where
  toGlyph(MacConst _ st ex (amb,_) :@ loc) = do
    old <- getState
    ctx <- toStateT loc st
    f   <- evalWith ex (mergeStores [ctx, old, amb])
    eval f >>= toGlyph
  toGlyph _ = error "toGlyph: MacExpr"


instance Glyph (Doc Expr) where
  toGlyph (Empty            :@ _) = return ""
  toGlyph (DocText (Text s) :@ _) = return s
  toGlyph x = error $ "toGlyph: Doc: " ++ show x








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
