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
import Feivel.Store

{------------------}
{- Contents       -}
{-   :Doc         -}
{-   :Expr        -}
{-     :StrExpr   -}
{-     :IntExpr   -}
{-     :BoolExpr  -}
{-     :RatExpr   -}
{-     :ZZModExpr -}
{-     :ListExpr  -}
{-     :MatExpr   -}
{-     :PolyExpr  -}
{-     :PermExpr  -}
{-     :MacExpr   -}
{-  :ExprErr      -}
{------------------}


{--------}
{- :Doc -}
{--------}

type Doc = AtLocus DocLeaf

data DocLeaf
 -- Primitives
 = Empty
 | DocText   Text
 | Escaped   Char
 | Scope     Doc
 | NakedKey  Key
 | NakedExpr Expr

 | Import    String (Maybe String) Doc

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
instance ToExpr Integer  where toExpr k = IntE   $ IntConst k :@ NullLocus
instance ToExpr Rat      where toExpr r = RatE   $ RatConst r :@ NullLocus
instance ToExpr String   where toExpr s = StrE   $ StrConst (Text s) :@ NullLocus
instance ToExpr Text     where toExpr t = StrE   $ StrConst t :@ NullLocus
instance ToExpr Bool     where toExpr b = BoolE  $ BoolConst b :@ NullLocus
instance ToExpr ZZModulo where toExpr (ZZModulo a n) = ZZModE $ ZZModConst (ZZMod n) (ZZModulo a n) :@ NullLocus



{------------}
{- :StrExpr -}
{------------}

type StrExpr = AtLocus StrExprLeaf

data StrExprLeaf
  = StrConst Text
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
  | MatRank    MatExpr

  -- Polynomial
  | IntContent PolyExpr

  -- Stats
  | IntObserveUniform  IntExpr IntExpr
  | IntObserveBinomial IntExpr RatExpr
  | IntObservePoisson  RatExpr

  -- Casts
  | IntCastStr StrExpr
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
  | Matches StrExpr Text

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

  -- Casting
  | RatCastStr StrExpr
  deriving (Eq, Show)



{--------------}
{- :ZZModExpr -}
{--------------}

type ZZModExpr = AtLocus ZZModExprLeaf

data ZZModExprLeaf
  = ZZModConst Type ZZModulo
  | ZZModVar   Type Key
  | ZZModCast  Type IntExpr

  | ZZModMacro Type [(Type, Key, Expr)] MacExpr
  | ZZModAtPos Type ListExpr IntExpr
  | ZZModAtIdx Type MatExpr  IntExpr IntExpr

  | ZZModIfThenElse Type BoolExpr ZZModExpr ZZModExpr
 
  -- Arithmetic
  | ZZModNeg   Type ZZModExpr
  | ZZModInv   Type ZZModExpr
 
  | ZZModAdd   Type ZZModExpr ZZModExpr
  | ZZModSub   Type ZZModExpr ZZModExpr
  | ZZModMult  Type ZZModExpr ZZModExpr
  | ZZModPow   Type ZZModExpr IntExpr

  | ZZModSum   Type ListExpr
  | ZZModProd  Type ListExpr
  deriving (Eq, Show)



{-------------}
{- :ListExpr -}
{-------------}

type ListExpr = AtLocus ListExprLeaf

data ListExprLeaf
  = ListConst   Type [Expr]
  | ListVar     Type Key
  | ListBuilder Type Expr [ListGuard]

  | ListMacro      Type [(Type, Key, Expr)] MacExpr
  | ListAtPos      Type ListExpr IntExpr
  | ListAtIdx      Type MatExpr  IntExpr IntExpr
  | ListRand       Type ListExpr
  | ListIfThenElse Type BoolExpr ListExpr ListExpr

  -- Arithmetic
  | ListCat   Type ListExpr ListExpr
  | ListToss  Type ListExpr ListExpr
  | ListRev   Type ListExpr
  | ListSort  Type ListExpr
  | ListUniq  Type ListExpr

  | ListFilter Type Key BoolExpr ListExpr

  -- Integer
  | ListRange Type IntExpr IntExpr

  -- Matrices
  | ListMatRow Type IntExpr MatExpr
  | ListMatCol Type IntExpr MatExpr

  -- Random
  | ListShuffle  Type ListExpr
  | ListChoose   Type IntExpr  ListExpr

  | ListShuffles Type ListExpr
  | ListChoices  Type IntExpr  ListExpr

  -- Permutations
  | ListPermsOf Type ListExpr

  | ListPivotColIndices Type MatExpr
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
  | MatVar   Type Key

  | MatMacro Type [(Type, Key, Expr)] MacExpr
  | MatAtPos Type ListExpr IntExpr
  | MatAtIdx Type MatExpr  IntExpr IntExpr

  | MatIfThenElse Type BoolExpr MatExpr MatExpr

  | MatBuilder Type Expr Key ListExpr Key ListExpr

  | MatRowFromList Type ListExpr
  | MatColFromList Type ListExpr

  -- Special Values
  | MatId     Type IntExpr
  | MatSwapE  Type IntExpr IntExpr IntExpr
  | MatScaleE Type IntExpr IntExpr Expr
  | MatAddE   Type IntExpr IntExpr IntExpr Expr

  -- Arithmetic
  | MatHCat  Type MatExpr MatExpr
  | MatVCat  Type MatExpr MatExpr
  | MatAdd   Type MatExpr MatExpr
  | MatMul   Type MatExpr MatExpr
  | MatPow   Type MatExpr IntExpr
  | MatNeg   Type MatExpr
  | MatTrans Type MatExpr

  -- Mutation
  | MatSwapRows Type MatExpr IntExpr IntExpr
  | MatSwapCols Type MatExpr IntExpr IntExpr
  | MatScaleRow Type MatExpr Expr IntExpr
  | MatScaleCol Type MatExpr Expr IntExpr
  | MatAddRow   Type MatExpr Expr IntExpr IntExpr
  | MatAddCol   Type MatExpr Expr IntExpr IntExpr
  | MatDelRow   Type MatExpr IntExpr
  | MatDelCol   Type MatExpr IntExpr

  | MatGetRow   Type IntExpr MatExpr
  | MatGetCol   Type IntExpr MatExpr

  -- Randomness
  | MatShuffleRows Type MatExpr
  | MatShuffleCols Type MatExpr

  -- Factorizations
  | MatGJForm   Type MatExpr
  | MatGJFactor Type MatExpr

  | MatRand Type ListExpr
  deriving (Eq, Show)



{-------------}
{- :PolyExpr -}
{-------------}

type PolyExpr = AtLocus PolyExprLeaf

data PolyExprLeaf
  = PolyConst Type (Poly Expr)
  | PolyVar   Type Key

  | PolyMacro Type [(Type, Key, Expr)] MacExpr
  | PolyAtPos Type ListExpr IntExpr
  | PolyAtIdx Type MatExpr  IntExpr IntExpr

  | PolyRand Type ListExpr

  | PolyIfThenElse Type BoolExpr PolyExpr PolyExpr

  | PolyAdd Type PolyExpr PolyExpr
  | PolySub Type PolyExpr PolyExpr
  | PolyMul Type PolyExpr PolyExpr
  | PolyPow Type PolyExpr IntExpr
  | PolyNeg Type PolyExpr

  | PolyFromRoots Type Variable ListExpr
  | PolyEvalPoly  Type PolyExpr [(Variable, PolyExpr)]
  deriving (Eq, Show)



{-------------}
{- :PermExpr -}
{-------------}

type PermExpr = AtLocus PermExprLeaf

data PermExprLeaf
  = PermConst Type (Perm Expr)
  | PermVar   Type Key

  | PermMacro Type [(Type, Key, Expr)] MacExpr
  | PermAtPos Type ListExpr IntExpr
  | PermAtIdx Type MatExpr  IntExpr IntExpr

  | PermRand Type ListExpr

  | PermIfThenElse Type BoolExpr PermExpr PermExpr

  | PermCompose Type PermExpr PermExpr
  | PermInvert  Type PermExpr
  deriving (Eq, Show)



{------------}
{- :MacExpr -}
{------------}

type MacExpr = AtLocus MacExprLeaf

data MacExprLeaf
  = MacConst Type [(Type, Key, Expr)] Expr (Store Expr, Bool)
  | MacVar   Type Key

  | MacMacro Type [(Type, Key, Expr)] MacExpr
  | MacAtPos Type ListExpr IntExpr
  | MacAtIdx Type MatExpr  IntExpr IntExpr

  | MacRand Type ListExpr

  | MacIfThenElse Type BoolExpr MacExpr MacExpr
  deriving (Eq, Show)



{------------}
{- :ExprErr -}
{------------}

data ExprErr
 = UnevaluatedExpression
 | BailMessage String
 deriving (Eq, Show)
