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

module Feivel.Debug where

import Feivel.Parse
import Feivel.EvalM
import Feivel.Eval
import Feivel.Store
import Feivel.Expr
import Feivel.Type

testParseM :: ParseM (a,b) -> String -> IO a
testParseM p str = case runParseM p "" str of
  Left err  -> fail $ show err
  Right (val,_) -> return val

testEvalM :: (Eval a) => a -> IO a
testEvalM expr = do
  (result, _) <- runEvalM emptyStore (eval expr)
  case result of
    Left err -> fail $ show err
    Right d  -> return d

testM :: (Eval a) => ParseM (a,t) -> String -> IO a
testM p str = testParseM p str >>= testEvalM


badmac :: String
badmac = "[:{>int}: Build(Macro(int; Eval(int; @x)); >int @x <- {Macro(int; 0)}) :]"

badmac2 :: String
badmac2 = "Build(Macro(int; Eval(int; @x)); >int @x <- {Macro(int; 0)})"

badmac3 :: String
badmac3 = "[[Macro(int; Eval(int; Macro(int; 0)))]]"

-- Parse and evaluate a template
testDoc :: String -> IO String
testDoc str = do
  x <- testParseM pDoc str
  (result, _) <- runEvalM emptyStore (evalToText x)
  case result of
    Left err -> fail $ show err
    Right d  -> return d