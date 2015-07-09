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

module Feivel.Parse.Store (
  pRecords
) where

import Feivel.Expr
import Feivel.Key
import Feivel.Parse.Key
import Feivel.Locus
import Feivel.Format
import Feivel.Parse.ParseM
import Feivel.Parse.Expr
import Feivel.Parse.Type

import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pRecords :: DataFormat -> ParseM [[(Key, Expr, Locus)]]
pRecords TypeKeyVal = pTypeKeyValFile

pRecords (CSV {csvHead = True,  csvDelim = delim}) = pCSVwithHead delim
pRecords (CSV {csvHead = False, csvDelim = delim}) = pCSVsansHead delim

pRecords _ = error "pRecords"



{--------------------}
{- TypeKeyVal       -}
{-   type:key:val;  -}
{--------------------}

pTypeKeyValFile :: ParseM [[(Key, Expr, Locus)]]
pTypeKeyValFile = do
  _  <- many pTypeKeyValComment
  rs <- many1 pTypeKeyValRecord
  eof
  return rs
    where
      pTypeKeyValComment :: ParseM ()
      pTypeKeyValComment = char '#' >> many (noneOf ['\n']) >> char '\n' >> return ()

      pTypeKeyValRecord :: ParseM [(Key, Expr, Locus)]
      pTypeKeyValRecord = do
        fs <- many1 pTypeKeyVal
        choice [char '\n' >> return (), eof]
        return fs
      
      pTypeKeyVal :: ParseM (Key, Expr, Locus)
      pTypeKeyVal = do
        start <- getPosition
        try $ char ':' >> spaces
        t <- pType
        char ':' >> spaces
        k <- pToken
        char ':' >> spaces
        (e,_) <- pTypedConst t
        _ <- char ';'
        end <- getPosition
        _ <- many $ char ' '
        return (Key k, e, locus start end)


{-------}
{- CSV -}
{-------}

pCSVwithHead :: Char -> ParseM [[(Key, Expr, Locus)]]
pCSVwithHead delim = undefined


pCSVsansHead :: Char -> ParseM [[(Key, Expr, Locus)]]
pCSVsansHead delim = undefined

pCSVRecordSansHead :: Char -> ParseM [(Key, Expr, Locus)]
pCSVRecordSansHead = undefined

pCSVFields :: Char -> ParseM [(Expr, Locus)]
pCSVFields delim = do
  fs <- sepBy1 (pCSVField delim) (char delim)
  choice [char '\n' >> return (), eof]
  return fs

pCSVField :: Char -> ParseM (Expr, Locus)
pCSVField delim = choice [pCSVFieldQuoted delim, pCSVFieldUnquoted delim]

pCSVFieldQuoted :: Char -> ParseM (Expr, Locus)
pCSVFieldQuoted delim = do
  start <- getPosition
  _ <- try $ char '"'
  str <- many1 (noneOf ['"'] <|> (try $ string "\"\"" >> return '"'))
  char '"'
  end <- getPosition
  let loc = locus start end
  return (StrE $ StrConst str :@ loc, loc)

pCSVFieldUnquoted :: Char -> ParseM (Expr, Locus)
pCSVFieldUnquoted delim = do
  start <- getPosition
  str   <- many1 $ noneOf [delim, '\n', '\r']
  end   <- getPosition
  let loc = locus start end
  return $ (StrE $ StrConst str :@ loc, loc)



{-

{- RFC822         -}
{-   key: val     -}
{-   Untyped      -}
{-   One per line -}

pUntypedKeyValListField :: ParseM (Key, StrExpr)
pUntypedKeyValListField = do
  kStart <- getPosition
  k      <- pToken
  kEnd   <- getPosition
  spaces >> char ':' >> spaces
  vStart <- getPosition
  v <- many1 (noneOf ['#','@','\n'])
  vEnd   <- getPosition
  return (Key k TypeStrExpr, StrConst (locus vStart vEnd) v)

pRFC822 :: ParseM (Either StateErr DocState)
pRFC822 = do
  fs <- sepBy pUntypedKeyValListField (char '\n')
  spaces >> eof
  return (fromKeyStringList fs)


{----------------}
{- AWK          -}
{-   "field" rs -}
{----------------}

pAWKFile :: String -> ParseM (Either StateErr [DocState])
pAWKFile rs = do
  rs <- many1 (pAWKRecord rs)
  eof
  return (sequence rs)

pAWKRecord :: String -> ParseM (Either StateErr DocState)
pAWKRecord rs = do
  ts <- sepBy1 (pAWKVal rs) (string rs)
  choice [char '\n' >> return (), eof]
  let nf      = (Key "NF" TypeIntExpr, ValueIntExpr (IntConst NullLocus (len ts)))
  let zero    = (Key "0" TypeStrExpr, ValueStrExpr (StrConst NullLocus (concat $ intersperse rs ts)))
  let foo k v = (Key (show k) TypeStrExpr, ValueStrExpr (StrConst NullLocus v))
  return (fromKeyValList $ ([nf,zero] ++) $ zipWith foo [1..] ts)


pAWKVal :: String -> ParseM String
pAWKVal rs = do
  s <- spaces >> pString
  if rs==" " then return () else spaces
  return s


-}