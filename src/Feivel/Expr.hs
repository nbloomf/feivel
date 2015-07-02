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

{-# LANGUAGE FlexibleInstances #-}

module Feivel.Expr (
  ToExpr, toExpr,

  -- Expression Types
  Doc, DocLeaf(..),

  Expr(..),
  StrExpr, StrExprLeaf(..), Format(..),
  IntExpr, IntExprLeaf(..),
  BoolExpr, BoolExprLeaf(..),
  RatExpr, RatExprLeaf(..),
  ZZModExpr, ZZModExprLeaf(..),

  ListExpr, ListExprLeaf(..), ListGuard(..),
  MatExpr, MatExprLeaf(..),
  PolyExpr, PolyExprLeaf(..),
  PermExpr, PermExprLeaf(..),
  MacExpr, MacExprLeaf(..),

  -- Errors
  ExprErr(..)
) where


import Feivel.Key
import Feivel.Type
import Feivel.Locus
import Feivel.Lib
import Feivel.Format
import Feivel.Store

{-----------------}
{- Contents      -}
{-   :Doc        -}
{-   :Expr       -}
{-     :StrExpr  -}
{-     :IntExpr  -}
{-     :BoolExpr -}
{-     :RatExpr  -}
{-     :ZZModExpr -}
{-     :ListExpr -}
{-     :MatExpr  -}
{-     :PolyExpr -}
{-     :PermExpr -}
{-     :MacExpr  -}
{-  :ExprErr     -}
{-----------------}


{--------}
{- :Doc -}
{--------}

type Doc = AtLocus DocLeaf

data DocLeaf
 -- Primitives
 = Empty
 | DocText   String
 | Escaped   Char
 | Scope     Doc
 | NakedKey  Key
 | Input     StrExpr
 | NakedExpr Expr

 | DocMacro [(Type, Key, Expr)] MacExpr

 -- Combination
 | Cat     [Doc]
 | CatPar  [Doc]
 | Alt     [Doc]
 | Shuffle [Doc]

 -- Flow Control
 | IfThenElse BoolExpr Doc Doc
 | Cond       [(BoolExpr, Doc)] Doc

 -- Binding
 | LetIn  Key Expr Doc
 | Define Type Key Expr Doc

 -- Selection and Repetition
 | ForSay Key ListExpr Doc (Maybe Doc)
 | Select Key ListExpr Doc

 -- Splice
 | Splice StrExpr (Maybe (Either StrExpr StrExpr)) (Maybe DataFormat)
 | Pull   StrExpr (Maybe StrExpr) Doc

 -- Debugging
 | Bail      StrExpr
 | ShowState
 deriving (Eq, Show)



{---------}
{- :Expr -}
{---------}

data Expr
  = DocE  Doc
  | BoolE BoolExpr
  | StrE  StrExpr
  | IntE  IntExpr
  | RatE  RatExpr
 
  | ZZModE ZZModExpr
 
  | ListE  ListExpr
  | MatE   MatExpr
  | PolyE  PolyExpr
  | PermE  PermExpr
  | MacE   MacExpr
  deriving (Eq, Show)

instance HasLocus Expr where
  locusOf (DocE   x) = locusOf x
  locusOf (StrE   x) = locusOf x
  locusOf (IntE   x) = locusOf x
  locusOf (BoolE  x) = locusOf x
  locusOf (RatE   x) = locusOf x
  locusOf (ZZModE x) = locusOf x
  locusOf (ListE  x) = locusOf x
  locusOf (MatE   x) = locusOf x
  locusOf (PolyE  x) = locusOf x
  locusOf (PermE  x) = locusOf x
  locusOf (MacE   x) = locusOf x


{- ToExpr -}

class ToExpr t where
  toExpr :: t -> Expr

instance ToExpr Expr      where toExpr = id
instance ToExpr Doc       where toExpr = DocE
instance ToExpr BoolExpr  where toExpr = BoolE
instance ToExpr StrExpr   where toExpr = StrE
instance ToExpr IntExpr   where toExpr = IntE
instance ToExpr RatExpr   where toExpr = RatE
instance ToExpr ZZModExpr where toExpr = ZZModE
instance ToExpr ListExpr  where toExpr = ListE
instance ToExpr MatExpr   where toExpr = MatE
instance ToExpr PolyExpr  where toExpr = PolyE
instance ToExpr PermExpr  where toExpr = PermE
instance ToExpr MacExpr   where toExpr = MacE

-- Not a fan of "no locus" here
instance ToExpr Integer where toExpr k = IntE  $ IntConst k :@ NullLocus
instance ToExpr Rat     where toExpr r = RatE  $ RatConst r :@ NullLocus
instance ToExpr String  where toExpr s = StrE  $ StrConst s :@ NullLocus
instance ToExpr Bool    where toExpr b = BoolE $ BoolConst b :@ NullLocus



{------------}
{- :StrExpr -}
{------------}

type StrExpr = AtLocus StrExprLeaf

data StrExprLeaf
  = StrConst String
  | StrVar   Key

  | StrMacro [(Type, Key, Expr)] MacExpr
  | StrAtPos ListExpr IntExpr
  | StrAtIdx MatExpr  IntExpr IntExpr
 
  | StrIfThenElse BoolExpr StrExpr StrExpr

  -- Combinators
  | Concat      StrExpr StrExpr
  | StrStrip    StrExpr StrExpr
 
  | Reverse     StrExpr
  | ToUpper     StrExpr
  | ToLower     StrExpr
  | Rot13       StrExpr

  -- Integer
  | StrHex      IntExpr
  | StrRoman    IntExpr
  | StrBase36   IntExpr

  -- Rational
  | StrDecimal RatExpr IntExpr

  -- List
  | StrRand ListExpr

  -- Matrix
  | StrTab MatExpr

  -- General
  | StrFormat Format Expr
  | StrTypeOf Expr

  -- Casting
  | StrIntCast IntExpr
  deriving (Eq, Show)



{------------}
{- :IntExpr -}
{------------}

type IntExpr = AtLocus IntExprLeaf

data IntExprLeaf
  = IntConst Integer
  | IntVar   Key

  | IntMacro [(Type, Key, Expr)] MacExpr
  | IntAtPos ListExpr IntExpr
  | IntAtIdx MatExpr  IntExpr IntExpr
 
  | IntIfThenElse BoolExpr IntExpr IntExpr
 
  -- Arithmetic
  | IntAdd    IntExpr IntExpr
  | IntSub    IntExpr IntExpr
  | IntMult   IntExpr IntExpr
  | IntQuo    IntExpr IntExpr
  | IntMod    IntExpr IntExpr
  | IntPow    IntExpr IntExpr
  | IntGCD    IntExpr IntExpr
  | IntLCM    IntExpr IntExpr
  | IntMin    IntExpr IntExpr
  | IntMax    IntExpr IntExpr
  | IntChoose IntExpr IntExpr
 
  | IntNeg        IntExpr
  | IntAbs        IntExpr
  | IntRad        IntExpr
  | IntSqPart     IntExpr
  | IntSqFreePart IntExpr

  -- String
  | StrLength StrExpr

  -- Rational
  | RatNumer RatExpr
  | RatDenom RatExpr
  | RatFloor RatExpr

  -- List
  | ListLen  ListExpr
  | IntRand  ListExpr
  | IntSum   ListExpr
  | IntProd  ListExpr
  | IntMaxim ListExpr
  | IntMinim ListExpr
  | IntGCDiv ListExpr
  | IntLCMul ListExpr

  -- Matrix
  | MatNumRows MatExpr
  | MatNumCols MatExpr

  -- Stats
  | IntObserveUniform  IntExpr IntExpr
  | IntObserveBinomial IntExpr RatExpr
  | IntObservePoisson  RatExpr
  deriving (Eq, Show)



{-------------}
{- :BoolExpr -}
{-------------}

type BoolExpr = AtLocus BoolExprLeaf

data BoolExprLeaf
  = BoolConst Bool
  | BoolVar   Key
  | IsDefined Key

  | BoolMacro [(Type, Key, Expr)] MacExpr
  | BoolAtPos ListExpr IntExpr
  | BoolAtIdx MatExpr  IntExpr IntExpr

  | BoolIfThenElse BoolExpr BoolExpr BoolExpr

  | BoolEq  Expr Expr
  | BoolNEq Expr Expr

  | BoolLT  Expr Expr
  | BoolLEq Expr Expr
  | BoolGT  Expr Expr
  | BoolGEq Expr Expr

  -- Arithmetic
  | Conj BoolExpr BoolExpr
  | Disj BoolExpr BoolExpr
  | Imp  BoolExpr BoolExpr
  | Neg  BoolExpr

  -- String
  | Matches StrExpr String

  -- Integer
  | IntDiv    IntExpr IntExpr
  | IntSqFree IntExpr

  | BoolRand ListExpr

  -- List
  | ListElem    Expr ListExpr
  | ListIsEmpty ListExpr

  -- Matrix
  | MatIsRow    MatExpr
  | MatIsCol    MatExpr
  | MatIsGJForm MatExpr
  deriving (Eq, Show)



{------------}
{- :RatExpr -}
{------------}

type RatExpr = AtLocus RatExprLeaf

data RatExprLeaf
  = RatConst Rat
  | RatVar   Key
  | RatCast  IntExpr

  | RatMacro [(Type, Key, Expr)] MacExpr
  | RatAtPos ListExpr IntExpr
  | RatAtIdx MatExpr  IntExpr IntExpr

  | RatIfThenElse BoolExpr RatExpr RatExpr
 
  -- Arithmetic
  | RatNeg   RatExpr
  | RatAbs   RatExpr
 
  | RatAdd   RatExpr RatExpr
  | RatSub   RatExpr RatExpr
  | RatMult  RatExpr RatExpr
  | RatQuot  RatExpr RatExpr
  | RatMin   RatExpr RatExpr
  | RatMax   RatExpr RatExpr

  | RatPow   RatExpr IntExpr

  -- List
  | RatRand  ListExpr
  | RatSum   ListExpr
  | RatProd  ListExpr
  | RatMaxim ListExpr
  | RatMinim ListExpr

  -- Stats
  | RatMean    ListExpr
  | RatMeanDev ListExpr
  | RatStdDev  ListExpr IntExpr
  | RatZScore  RatExpr  ListExpr IntExpr

  -- Approximations
  | RatSqrt  RatExpr IntExpr
  deriving (Eq, Show)



{--------------}
{- :ZZModExpr -}
{--------------}

type ZZModExpr = AtLocus ZZModExprLeaf

data ZZModExprLeaf
  = ZZModConst ZZModulo
  | ZZModVar   Key
  | ZZModCast  IntExpr

  | ZZModMacro [(Type, Key, Expr)] MacExpr
  | ZZModAtPos ListExpr IntExpr
  | ZZModAtIdx MatExpr  IntExpr IntExpr

  | ZZModIfThenElse BoolExpr ZZModExpr ZZModExpr
 
  -- Arithmetic
  | ZZModNeg   ZZModExpr
 
  | ZZModAdd   ZZModExpr ZZModExpr
  | ZZModSub   ZZModExpr ZZModExpr
  | ZZModMult  ZZModExpr ZZModExpr
  deriving (Eq, Show)



{-------------}
{- :ListExpr -}
{-------------}

type ListExpr = AtLocus ListExprLeaf

data ListExprLeaf
  = ListConst   Type [Expr]
  | ListVar     Key
  | ListBuilder Expr [ListGuard]

  | ListMacro [(Type, Key, Expr)] MacExpr
  | ListAtPos ListExpr IntExpr
  | ListAtIdx MatExpr  IntExpr IntExpr

  | ListIfThenElse BoolExpr ListExpr ListExpr

  -- Arithmetic
  | ListCat   ListExpr ListExpr
  | ListToss  ListExpr ListExpr
  | ListRev   ListExpr
  | ListSort  ListExpr
  | ListUniq  ListExpr

  | ListFilter Key BoolExpr ListExpr

  -- Integer
  | ListRange IntExpr IntExpr

  | ListRand ListExpr

  -- Matrices
  | ListMatRow IntExpr MatExpr
  | ListMatCol IntExpr MatExpr

  -- Random
  | ListShuffle ListExpr
  | ListChoose  IntExpr  ListExpr

  | ListChoices IntExpr  ListExpr
  deriving (Eq, Show)

data ListGuard
  = Bind  Key ListExpr
  | Guard BoolExpr
  deriving (Eq, Show)



{------------}
{- :MatExpr -}
{------------}

type MatExpr = AtLocus MatExprLeaf

data MatExprLeaf
  = MatConst Type (Matrix Expr)
  | MatVar   Key

  | MatMacro [(Type, Key, Expr)] MacExpr
  | MatAtPos ListExpr IntExpr
  | MatAtIdx MatExpr  IntExpr IntExpr

  | MatIfThenElse BoolExpr MatExpr MatExpr

  | MatBuilder Type Expr Key ListExpr Key ListExpr

  -- Special Values
  | MatId     Type IntExpr
  | MatSwapE  Type IntExpr IntExpr IntExpr
  | MatScaleE Type IntExpr IntExpr Expr
  | MatAddE   Type IntExpr IntExpr IntExpr Expr

  -- Arithmetic
  | MatHCat  MatExpr MatExpr
  | MatVCat  MatExpr MatExpr
  | MatAdd   MatExpr MatExpr
  | MatMul   MatExpr MatExpr
  | MatPow   MatExpr IntExpr
  | MatNeg   MatExpr
  | MatTrans MatExpr

  -- Mutation
  | MatSwapRows MatExpr IntExpr IntExpr
  | MatSwapCols MatExpr IntExpr IntExpr
  | MatScaleRow MatExpr Expr IntExpr
  | MatScaleCol MatExpr Expr IntExpr
  | MatAddRow   MatExpr Expr IntExpr IntExpr
  | MatAddCol   MatExpr Expr IntExpr IntExpr
  | MatDelRow   MatExpr IntExpr
  | MatDelCol   MatExpr IntExpr

  | MatGetRow   IntExpr MatExpr
  | MatGetCol   IntExpr MatExpr

  -- Randomness
  | MatShuffleRows MatExpr
  | MatShuffleCols MatExpr

  -- Factorizations
  | MatGJForm   MatExpr
  | MatGJFactor MatExpr

  | MatRand ListExpr
  deriving (Eq, Show)



{-------------}
{- :PolyExpr -}
{-------------}

type PolyExpr = AtLocus PolyExprLeaf

data PolyExprLeaf
  = PolyConst Type (Poly Expr)
  | PolyVar   Key

  | PolyMacro [(Type, Key, Expr)] MacExpr
  | PolyAtPos ListExpr IntExpr
  | PolyAtIdx MatExpr  IntExpr IntExpr

  | PolyRand ListExpr

  | PolyIfThenElse BoolExpr PolyExpr PolyExpr

  | PolyAdd PolyExpr PolyExpr
  | PolySub PolyExpr PolyExpr
  | PolyMul PolyExpr PolyExpr
  | PolyPow PolyExpr IntExpr
  | PolyNeg PolyExpr

  | PolyFromRoots Variable ListExpr
  | PolyEvalPoly  PolyExpr [(Variable, PolyExpr)]
  deriving (Eq, Show)



{-------------}
{- :PermExpr -}
{-------------}

type PermExpr = AtLocus PermExprLeaf

data PermExprLeaf
  = PermConst Type (Perm Expr)
  | PermVar   Key

  | PermMacro [(Type, Key, Expr)] MacExpr
  | PermAtPos ListExpr IntExpr
  | PermAtIdx MatExpr  IntExpr IntExpr

  | PermRand ListExpr

  | PermIfThenElse BoolExpr PermExpr PermExpr

  | PermCompose PermExpr PermExpr
  | PermInvert  PermExpr
  deriving (Eq, Show)



{------------}
{- :MacExpr -}
{------------}

type MacExpr = AtLocus MacExprLeaf

data MacExprLeaf
  = MacConst Type [(Type, Key, Expr)] Expr (Store Expr, Bool)
  | MacVar   Key

  | MacMacro [(Type, Key, Expr)] MacExpr
  | MacAtPos ListExpr IntExpr
  | MacAtIdx MatExpr  IntExpr IntExpr

  | MacRand ListExpr

  | MacIfThenElse BoolExpr MacExpr MacExpr
  deriving (Eq, Show)



{------------}
{- :ExprErr -}
{------------}

data ExprErr
 = UnevaluatedExpression
 | BailMessage String
 deriving (Eq, Show)
