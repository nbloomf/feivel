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

module Feivel.Grammar.Doc where

import Feivel.Grammar.Util


data DocLeaf a doc
 -- Primitives
 = Empty
 | DocText   Text
 | Escaped   Char
 | Scope     doc
 | NakedKey  Key
 | NakedExpr a -- XX

 | Import    String (Maybe String) doc

 | DocMacro [(Type, Key, a)] a -- XX, MacTo DD

 -- Combination
 | Cat     [doc]
 | CatPar  [doc]
 | Alt     [doc]
 | Shuffle [doc]

 -- Flow Control
 | IfThenElse a doc doc -- BB
 | Cond       [(a, doc)] doc -- BB

 -- Binding
 | LetIn  Key a doc -- XX
 | Define Type Key a doc -- XX

 -- Selection and Repetition
 | ForSay Key a doc (Maybe doc) -- ListOf XX
 | Select Key a doc             -- ListOf XX

 -- Debugging
 | Bail      a -- SS
 | ShowState
 deriving (Eq, Show)
