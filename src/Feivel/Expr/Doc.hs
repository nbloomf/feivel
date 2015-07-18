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

module Feivel.Expr.Doc where

import Feivel.Expr.Util

{--------}
{- :Doc -}
{--------}

type Doc a = AtLocus (DocLeaf a)

data DocLeaf a
 -- Primitives
 = Empty
 | DocText   Text
 | Escaped   Char
 | Scope     (Doc a)
 | NakedKey  Key
 | NakedExpr a -- XX

 | Import    String (Maybe String) (Doc a)

 | DocMacro [(Type, Key, a)] a -- XX, MacTo DD

 -- Combination
 | Cat     [Doc a]
 | CatPar  [Doc a]
 | Alt     [Doc a]
 | Shuffle [Doc a]

 -- Flow Control
 | IfThenElse a (Doc a) (Doc a) -- BB
 | Cond       [(a, Doc a)] (Doc a) -- BB

 -- Binding
 | LetIn  Key a (Doc a) -- XX
 | Define Type Key a (Doc a) -- XX

 -- Selection and Repetition
 | ForSay Key a (Doc a) (Maybe (Doc a)) -- ListOf XX
 | Select Key a (Doc a)             -- ListOf XX

 -- Debugging
 | Bail      a -- SS
 | ShowState
 deriving (Eq, Show)