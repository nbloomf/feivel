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
import Feivel.Eval
import Feivel.Store
import Feivel.Expr
import Feivel.Main
import Feivel.GUI

-- Parse an object
testParseM :: ParseM a -> String -> IO a
testParseM p str = case runParseM p "" str of
  Left err  -> fail $ show err
  Right val -> return val

-- Eval an object
testEvalM :: (Eval a) => a -> IO a
testEvalM expr = do
  (result, _) <- runEvalM emptyStore (eval expr)
  case result of
    Left err -> fail $ show err
    Right d  -> return d

-- Parse and evaluate an object
testM :: (Eval a) => ParseM a -> String -> IO a
testM p str = testParseM p str >>= testEvalM

-- Parse and evaluate a template
testDoc :: String -> IO String
testDoc str = do
  x <- testParseM (pDoc pTypedExpr) str
  (result, _) <- runEvalM emptyStore (evalToGlyph x)
  case result of
    Left err -> fail $ show err
    Right d  -> return d