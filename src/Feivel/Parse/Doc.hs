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

module Feivel.Parse.Doc (
  pDoc, pBrackDocE, pTypedNakedExpr
) where

import Feivel.Store (AtLocus(..), Locus(..))
import Feivel.Grammar
import Feivel.Parse.Util
import Feivel.Parse.ParseM
import Feivel.Lib (Text(..))

import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)


pBrackDoc :: ParseM Doc -> ParseM Doc
pBrackDoc pDOC = do
  _ <- try $ char '['
  d <- pDOC
  _ <- char ']'
  whitespace
  return d

pBrackDocE :: ParseM Doc -> ParseM Expr
pBrackDocE pDOC = do
  _ <- try $ char '['
  d <- pDOC
  _ <- char ']'
  whitespace
  return (DocE d)

pTypedNakedExpr :: (Type -> ParseM Expr) -> ParseM Expr
pTypedNakedExpr pE = do
  t <- pType
  keyword ":"
  pE t

pDoc :: (Type -> ParseM Expr) -> ParseM Doc -> ParseM Doc
pDoc pE pDOC = choice $ map (fmap Doc . pAtLocus)
  [ eof >> return Empty
  , lookAhead (char ']') >> return Empty
  , do
    xs <- many1 $ choice $ map (fmap Doc . pAtLocus)
      [ try (char '#' >> many1 space) >> return Empty
      , try (char '#' >> eof) >> return Empty
      , many1 (noneOf "#@[]") >>= \x -> return (DocText (Text x))
      , pVarExpr NakedKey DD
      , pEscaped
      , pEval
      , pImport
      , pDefine
      , pScope
      , pNakedExpr
      , pIfThenElse
      , pShuffle
      , pCond
      , pFor
      , pAlt
      , pBail
      , pLetIn
      , pSelect
      , pNote
      ]
    return (Cat xs)
  ]
  where
    pEval = do
      try (char '[' >> keyword "eval")
      e <- pE (MacTo DD)
      vals <- option [] pVals
      _ <- option () (try (keyword "endeval")) >> char ']'
      return (DocMacro vals e)
        where
          pVals = do
            _ <- try $ keyword "("
            vs <- sepBy (pTypeKeyExpr pE) (keyword ";")
            keyword ")"
            return vs

    pEscaped = do
      x <- choice (zipWith pEsc "#@[]_.-~n" "#@[]_.- \n") <?> "escaped character"
      return (Escaped x)
      where
        pEsc c d = try (string $ '#':[c]) >> (return d)

    pImport = do
      try (char '[' >> keyword "import")
      path <- pParens pPath
      qual <- option Nothing (try (keyword "as") >> pParens pToken >>= return . Just)
      _    <- option () (try (keyword "endimport")) >> char ']'
      rest <- pDOC
      return (Import path qual rest)


    pDefine = do
      try (char '[' >> keyword "define")
      t <- pType
      whitespace
      k <- pKey
      keyword ":="
      v <- if t == DD then (pBrackDocE pDOC) else pE t
      _ <- option () (try (keyword "enddefine")) >> char ']'
      rest <- pDOC
      return (Define t k v rest)

    pScope = do
      try (char '[' >> keyword "scope")
      x <- pBrackDoc pDOC
      option () (try (keyword "endscope"))
      _ <- whitespace >> char ']'
      return (Scope x)

    pNakedExpr = do
      try (char '[' >> keyword ":")
      x <- try (pTypedNakedExpr pE)
      _ <- keyword ":" >> char ']'
      return (NakedExpr x)

    pShuffle = do
      try (char '[' >> keyword "shuffle")
      xs <- many (pBrackDoc pDOC)
      option () (try $ keyword "endshuffle")
      _ <- whitespace >> char ']'
      return (Shuffle xs)

    pIfThenElse = do
      try (char '[' >> keyword "if")
      test  <- pE BB
      true  <- keyword "then" >> (pBrackDoc pDOC)
      false <- keyword "else" >> (pBrackDoc pDOC)
      option () (try (keyword "endif"))
      _ <- whitespace >> char ']'
      return (IfThenElse test true false)

    pCond = do
      try (char '[' >> keyword "cond")
      cases <- many pCondCase
      auto <- option (Doc (Empty :@ NullLocus)) (try (keyword "default") >> (pBrackDoc pDOC))
      option () (try (keyword "endcond"))
      _ <- whitespace >> char ']'
      return (Cond cases auto)
      where
        pCondCase = do
          s <- try (keyword "case") >> (pE BB)
          t <- pBrackDoc pDOC
          return (s,t)

    pFor = do
      try (char '[' >> keyword "for")
      typ <- pType
      spaces
      k <- pKey
      r <- keyword "in"  >> (pE $ ListOf typ)
      t <- keyword "say" >> (pBrackDoc pDOC)
      b <- option Nothing $ (try (keyword "sepby")) >> (pBrackDoc pDOC) >>= (return . Just)
      option () (try (keyword "endfor"))
      _ <- whitespace >> char ']'
      return (ForSay k r t b)

    pAlt = do
      try (char '[' >> keyword "alt")
      opts  <- many pAltOpt
      option () (try (keyword "endalt"))
      _ <- whitespace >> char ']'
      return (Alt opts)
      where
        pAltOpt = try (keyword "opt") >> (pBrackDoc pDOC)

    pBail = do
      try (char '[' >> keyword "bail")
      message <- pE SS
      option () (try (keyword "endbail"))
      _ <- whitespace >> char ']'
      return (Bail message)

    pLetIn = do
      try (char '[' >> keyword "let")
      (_,k,e) <- (pTypeKeyExpr pE)
      body <- keyword "in" >> (pBrackDoc pDOC)
      option () (try (keyword "endlet"))
      _ <- whitespace >> char ']'
      return (LetIn k e body)

    pSelect = do
      try (char '[' >> keyword "select")
      ty <- pType
      k <- spaced pKey
      r <- keyword "from" >> (pE $ ListOf ty)
      t <- keyword "in"   >> (pBrackDoc pDOC)
      option () (try (keyword "endselect"))
      _ <- whitespace >> char ']'
      return (Select k r t)

    pNote = do
      try (char '[' >> keyword "~")
      _ <- many $ choice [ try (noneOf ['\\', '~'])
                         , try (string "\\~") >> return '~'
                         , try (string "\\") >> return '\\']
      _ <- keyword "~" >> char ']'
      return (Empty)


{-

  where
    pShowState :: Parser Doc
    pShowState = do
      start <- getPosition
      try (keyword "state")
      end   <- getPosition
      return $ ShowState (locus start end)
-}
