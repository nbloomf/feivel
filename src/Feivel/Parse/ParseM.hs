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

module Feivel.Parse.ParseM (
  ParseM, runParseM, reportParseErr, testParseM, pAtLocus, pLiftAt, stringParseM,

  -- Constants
  pNatural, pInteger, pRatInteger, pRat, pBool, pString, pVar, pFormat, pPath, pPaths, pText,

  -- Fences
  pParens, pBrack, pBrace,

  -- Lists
  pBraceList, pBrackList
) where


import Feivel.Error
import Feivel.Locus
import Feivel.Lib (Rat((:/:)), Variable(..), Format(..), Text(..))

import Text.Parsec.Prim
  (ParsecT, runParserT, try, (<|>), (<?>), many, getPosition)
import Text.ParserCombinators.Parsec
  (digit, many1, char, string, choice, option, noneOf, spaces, sepBy, oneOf, sepBy1)

import Control.Monad.Trans.Class (lift)


type ParseM = ParsecT String () (Either Goof)

stringParseM :: ParseM a -> String -> Either Goof a
stringParseM p str = runParseM p "" str

runParseM :: ParseM a -> String -> String -> Either Goof a
runParseM p name str = case runParserT p () name str of
  Left goof -> Left goof
  Right (Left err) -> Left (parseGoof err)
  Right (Right x) -> Right x

reportParseErr :: (PromoteError err) => Locus -> err -> ParseM a
reportParseErr loc err = lift $ Left $ Goof loc (promote err)

testParseM :: (Show a) => ParseM a -> String -> IO ()
testParseM p str = case runParseM p "" str of
  Left err -> putStrLn $ show err
  Right val -> putStrLn $ show val

pAtLocus :: ParseM (a,t) -> ParseM (AtLocus a, t)
pAtLocus p = do
  start <- getPosition
  (x,t) <- p
  end <- getPosition
  return (x :@ (locus start end), t)

pLiftAt :: ParseM a -> t -> ParseM (a,t)
pLiftAt p t = do
  x <- p
  return (x,t)





{--------------}
{- :Constants -}
{--------------}

pNatural :: ParseM Integer
pNatural = do
  ds <- many1 digit
  return $ rd ds
    where rd = read :: String -> Integer

pInteger :: ParseM Integer
pInteger = pNegative <|> pNatural <?> "integer constant"
  where
    pNegative = do
      _ <- char '-'
      n <- pNatural
      return (-n)

pRatInteger :: ParseM Rat
pRatInteger = do
  k <- pInteger
  return $ k :/:1

pRat :: ParseM Rat
pRat = do
  a <- pInteger
  b <- option 1 (char '/' >> pInteger)
  return (a:/:b)

pBool :: ParseM Bool
pBool = choice
  [ try $ string "#t" >> return True
  , try $ string "#f" >> return False
  ] <?> "boolean constant"

pText :: ParseM Text
pText = fmap Text pString

pString :: ParseM String
pString = do
  _ <- char '"'
  t <- many $ choice
         [ try $ noneOf ['\\', '"', '\n']
         , try $ string "\\n"  >> return '\n'
         , try $ string "\\\"" >> return '"'
         , try $ string "\\"   >> return '\\'
         ]
  _ <- char '"'
  return t

pPath :: ParseM FilePath
pPath = many1 $ oneOf allowed
  where
    allowed =
      "abcdefghijklmnopqrstuvwxyz" ++
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
      "_[]+-.0123456789/<>=:;@#$%^&"

pPaths :: ParseM [FilePath]
pPaths = sepBy1 pPath spaces

pVar :: ParseM Variable
pVar = fmap Var $ many1 $ oneOf allowed
  where
    allowed =
      "abcdefghijklmnopqrstuvwxyz" ++
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
      "_[]"

pFormat :: ParseM Format
pFormat = pLaTeX
  where
    pLaTeX = do
      _ <- try $ string "latex"
      return LaTeX



{-----------}
{- :Fences -}
{-----------}

pParens :: ParseM a -> ParseM a
pParens p = do
  try $ char '(' >> spaces
  x <- p
  char ')' >> spaces
  return x

pBrack :: ParseM a -> ParseM a
pBrack p = do
  try $ char '[' >> spaces
  x <- p
  char ']' >> spaces
  return x

pBrace :: ParseM a -> ParseM a
pBrace p = do
  try $ char '{' >> spaces
  x <- p
  char '}' >> spaces
  return x



{---------}
{- :List -}
{---------}

pBraceList :: ParseM a -> ParseM [a]
pBraceList p = do
  try $ char '{' >> spaces
  xs <- p `sepBy` (char ';' >> spaces)
  char '}' >> spaces
  return xs

pBrackList :: ParseM a -> ParseM [a]
pBrackList p = do
  try $ char '[' >> spaces
  xs <- p `sepBy` (char ';' >> spaces)
  char ']' >> spaces
  return xs
