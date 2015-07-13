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

module Feivel.Lib.String (
  Text(..), StrErr(), Format(..),

  strCat, strRev, strLen, strUpper, strLower, strRot13, strStrip, strRoman,
  strHex, strBase36
) where

import Data.Char (toUpper, toLower)
import Data.Maybe (fromMaybe)


{----------}
{- :Types -}
{----------}

data Text = Text { unText :: String }

data Format = LaTeX deriving (Eq, Show)

data StrErr
 = OutOfRomanRange Integer
 deriving (Eq, Show)

liftText :: (String -> String) -> Text -> Text
liftText f = Text . f . unText




strCat :: Text -> Text -> Either StrErr Text
strCat (Text a) (Text b) = Right $ Text $ a ++ b

strRev :: Text -> Either StrErr Text
strRev = Right . liftText reverse

strLen :: Text -> Either StrErr Integer
strLen (Text a) = Right $ sum $ map (const 1) a

strUpper :: Text -> Either StrErr Text
strUpper = Right . liftText (map toUpper)

strLower :: Text -> Either StrErr Text
strLower = Right . liftText (map toLower)

rot13 :: String -> String
rot13 str = map rot str 
  where
    as = ['a'..'m'] ++ ['A'..'M'] ++ ['0'..'4']
    bs = ['n'..'z'] ++ ['N'..'Z'] ++ ['5'..'9']

    assocs = zip (as ++ bs) (bs ++ as)

    rot c = fromMaybe c $ lookup c assocs


strRot13 :: Text -> Either StrErr Text
strRot13 = Right . liftText rot13

strip :: String -> String -> String
strip a b = last $ foo a b
  where
    foo xs [] = [xs]
    foo [] _  = [[]]
    foo xs ys = xs : (if head xs == head ys then foo (tail xs) (tail ys) else [])

strStrip :: Text -> Text -> Either StrErr Text
strStrip (Text a) (Text b) = Right $ Text $ strip a b

strRoman :: Integer -> Either StrErr Text
strRoman n 
 | n <= 0 || n >= 4000 = Left $ OutOfRomanRange n
 | otherwise = Right $ Text $ convert n digits []
 where
   convert _ [] ds = concat $ reverse ds
   convert k rs@((i,s):rs') ds = if i > k then convert k rs' ds else convert (k-i) rs (s:ds)

   digits =
     [ (1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"), (50, "L")
     , (40, "XL"),  (10, "X"),   (9, "IX"),  (5, "V"),    (4, "IV"),  (1, "I")]

hex :: Integer -> String
hex k
  | k < 0     = '-' : hex (-k)
  | k == 0    = "0"
  | otherwise = convert k ""
     where
       convert 0 ds = ds
       convert a ds = convert q ((dig r) : ds)
         where (q,r) = (a `div` 16, a `rem` 16)

       dig 0  = '0'; dig 1  = '1'; dig 2  = '2'; dig 3  = '3'
       dig 4  = '4'; dig 5  = '5'; dig 6  = '6'; dig 7  = '7'
       dig 8  = '8'; dig 9  = '9'; dig 10 = 'a'; dig 11 = 'b'
       dig 12 = 'c'; dig 13 = 'd'; dig 14 = 'e'; dig 15 = 'f'
       dig _  = 'X';

strHex :: Integer -> Either StrErr String
strHex n = Right $ hex n

base36 :: Integer -> String
base36 k
  | k < 0     = '-' : base36 (-k)
  | k == 0    = "0"
  | otherwise = convert k ""
     where
       convert 0 ds = ds
       convert a ds = convert q ((dig r) : ds)
         where (q,r) = (a `div` 36, a `rem` 36)

       dig 0  = '0'; dig 1  = '1'; dig 2  = '2'; dig 3  = '3'
       dig 4  = '4'; dig 5  = '5'; dig 6  = '6'; dig 7  = '7'
       dig 8  = '8'; dig 9  = '9'; dig 10 = 'a'; dig 11 = 'b'
       dig 12 = 'c'; dig 13 = 'd'; dig 14 = 'e'; dig 15 = 'f'
       dig 16 = 'g'; dig 17 = 'h'; dig 18 = 'i'; dig 19 = 'j'
       dig 20 = 'k'; dig 21 = 'l'; dig 22 = 'm'; dig 23 = 'n'
       dig 24 = 'o'; dig 25 = 'p'; dig 26 = 'q'; dig 27 = 'r'
       dig 28 = 's'; dig 29 = 't'; dig 30 = 'u'; dig 31 = 'v'
       dig 32 = 'w'; dig 33 = 'x'; dig 34 = 'y'; dig 35 = 'z'
       dig _  = 'X'

strBase36 :: Integer -> Either StrErr String
strBase36 n = Right $ base36 n
