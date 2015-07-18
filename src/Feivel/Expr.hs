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

import Feivel.Expr.ZZMod
import Feivel.Expr.Perm
import Feivel.Expr.Mac
import Feivel.Expr.Poly
import Feivel.Expr.Rat
import Feivel.Expr.Mat

{------------------}
{- Contents       -}
{-   :Doc         -}
{-   :Expr        -}
{-     :StrExpr   -}
{-     :IntExpr   -}
{-     :BoolExpr  -}
{-     :ListExpr  -}
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

 | DocMacro [(Type, Key, Expr)] Expr -- MacTo DD

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
  | RatE  (RatExpr Expr)
 
  | ZZModE (ZZModExpr Expr)
 
  | ListE  ListExpr
  | MatE   (MatExpr  Expr)
  | PolyE  (PolyExpr Expr)
  | PermE  (PermExpr Expr)
  | MacE   (MacExpr  Expr)
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
instance ToExpr (RatExpr   Expr) where toExpr = RatE
instance ToExpr (ZZModExpr Expr) where toExpr = ZZModE
instance ToExpr ListExpr  where toExpr = ListE
instance ToExpr (MatExpr  Expr) where toExpr = MatE
instance ToExpr (PolyExpr Expr) where toExpr = PolyE
instance ToExpr (PermExpr Expr) where toExpr = PermE
instance ToExpr (MacExpr  Expr) where toExpr = MacE

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

  | StrMacro [(Type, Key, Expr)] Expr -- MacTo SS
  | StrAtPos Expr Expr -- ListOf SS, ZZ
  | StrAtIdx Expr Expr Expr -- MatOf SS, ZZ, ZZ
 
  | StrIfThenElse Expr StrExpr StrExpr -- BB

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
  | StrDecimal Expr Expr -- QQ, ZZ

  -- List
  | StrRand ListExpr

  -- Matrix
  | StrTab Expr -- MatOf XX

  -- General
  | StrFormat Format Expr
  | StrTypeOf Expr

  -- Casting
  | StrIntCast IntExpr
  deriving (Eq, Show)



{------------}
{- :IntExpr -}
{------------}

type IntExpr = AtLocus (IntExprLeaf Expr)

data IntExprLeaf a
  = IntConst Integer
  | IntVar   Key

  | IntMacro [(Type, Key, Expr)] a -- MacTo ZZ
  | IntAtPos a a   -- ListOf ZZ, ZZ
  | IntAtIdx a a a -- MatOf ZZ, ZZ, ZZ
 
  | IntIfThenElse a IntExpr IntExpr -- BB
 
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
  | StrLength a -- SS

  -- Rational
  | RatNumer a -- QQ
  | RatDenom a -- QQ
  | RatFloor a -- QQ

  -- List
  | ListLen  a -- ListOf XX
  | IntRand  a -- ListOf ZZ
  | IntSum   a -- ListOf ZZ
  | IntProd  a -- ListOf ZZ
  | IntMaxim a -- ListOf ZZ
  | IntMinim a -- ListOf ZZ
  | IntGCDiv a -- ListOf ZZ
  | IntLCMul a -- ListOf ZZ

  -- Matrix
  | MatNumRows a -- MatOf XX
  | MatNumCols a -- MatOf XX
  | MatRank    a -- MatOf XX

  -- Polynomial
  | IntContent a -- PolyOver ZZ

  -- Stats
  | IntObserveUniform  IntExpr IntExpr
  | IntObserveBinomial IntExpr a    -- QQ
  | IntObservePoisson  a            -- QQ

  -- Casts
  | IntCastStr a -- SS
  deriving (Eq, Show)



{-------------}
{- :BoolExpr -}
{-------------}

type BoolExpr = AtLocus (BoolExprLeaf Expr)

data BoolExprLeaf a
  = BoolConst Bool
  | BoolVar   Key
  | IsDefined Key

  | BoolMacro [(Type, Key, a)] a -- Expr, MacTo BB
  | BoolAtPos a a      -- ListOf BB, ZZ
  | BoolAtIdx a a a -- MatOf BB, ZZ, ZZ

  | BoolIfThenElse a BoolExpr BoolExpr -- BB

  | BoolEq  a a
  | BoolNEq a a

  | BoolLT  a a
  | BoolLEq a a
  | BoolGT  a a
  | BoolGEq a a

  -- Arithmetic
  | Conj BoolExpr BoolExpr
  | Disj BoolExpr BoolExpr
  | Imp  BoolExpr BoolExpr
  | Neg  BoolExpr

  -- String
  | Matches a Text -- SS

  -- Integer
  | IntDiv    a a -- ZZ, ZZ
  | IntSqFree a      -- ZZ

  | BoolRand a -- ListOf BB

  -- List
  | ListElem    a a -- XX, ListOf XX
  | ListIsEmpty a -- ListOf XX

  -- Matrix
  | MatIsRow    a -- MatOf XX
  | MatIsCol    a -- MatOf XX
  | MatIsGJForm a -- MatOf XX
  deriving (Eq, Show)



{-------------}
{- :ListExpr -}
{-------------}

type ListExpr = AtLocus ListExprLeaf

data ListExprLeaf
  = ListConst   Type [Expr]
  | ListVar     Type Key
  | ListBuilder Type Expr [ListGuard]

  | ListMacro      Type [(Type, Key, Expr)] Expr -- MacTo (ListOf typ)
  | ListAtPos      Type Expr Expr -- ListOf (ListOf typ), ZZ
  | ListAtIdx      Type Expr Expr Expr -- MatOf (ListOf typ), ZZ, ZZ
  | ListRand       Type ListExpr
  | ListIfThenElse Type Expr ListExpr ListExpr -- BB

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
  | ListMatRow Type IntExpr Expr -- MatOf typ
  | ListMatCol Type IntExpr Expr -- MatOf typ

  -- Random
  | ListShuffle  Type ListExpr
  | ListChoose   Type IntExpr  ListExpr

  | ListShuffles Type ListExpr
  | ListChoices  Type IntExpr  ListExpr

  -- Permutations
  | ListPermsOf Type ListExpr

  | ListPivotColIndices Type Expr -- MatOf XX
  deriving (Eq, Show)

data ListGuard
  = Bind  Key ListExpr
  | Guard BoolExpr
  deriving (Eq, Show)







{------------}
{- :ExprErr -}
{------------}

data ExprErr
 = UnevaluatedExpression
 | BailMessage String
 deriving (Eq, Show)
