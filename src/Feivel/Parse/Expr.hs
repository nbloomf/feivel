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

module Feivel.Parse.Expr (
  pDoc, pREPL,

  pTypedConst,
    pStrConst, pIntConst, pBoolConst, pRatConst, pZZModConst,
    pListLiteral, pMatLiteral, pPolyLiteral, pPermLiteral,

  pTypedExpr,
    pIntExpr, pRatExpr, pStrExpr, pBoolExpr, pZZModExpr,
    pTypedListExpr, pTypedMatExpr, pTypedPolyExpr, pTypedPermExpr
) where

import Feivel.Store
import Feivel.Expr

import Feivel.Parse.Util
import Feivel.Parse.ParseM

import Feivel.Parse.Int
import Feivel.Parse.Str
import Feivel.Parse.Rat
import Feivel.Parse.Bool
import Feivel.Parse.ZZMod
import Feivel.Parse.Perm
import Feivel.Parse.Poly
import Feivel.Parse.Mat
import Feivel.Parse.List

import Feivel.Error


import Feivel.Lib (mFromRowList, Variable(..), Natural(..), fromListP, Monomial, fromListM, identityM, nullP, mapFst, fromCycles, zzmod, Text(..), idPerm)

import Text.Parsec.Expr
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)

{---------------}
{- Contents    -}
{-  :Doc       -}
{-  :Expr      -}
{-  :MacExpr   -}
{---------------}

{--------------}
{- :Utilities -}
{--------------}


{- Term Parsing -}

pAtPos :: Type -> (Expr -> Expr -> b) -> ParseM b
pAtPos typ fun = pFun2 "AtPos" (pTypedExpr (ListOf typ)) (pTypedExpr ZZ) fun

pAtIdx :: Type -> (Expr -> Expr -> Expr -> b) -> ParseM b
pAtIdx typ fun = pFun3 "AtIdx" (pTypedExpr (MatOf typ)) (pTypedExpr ZZ) (pTypedExpr ZZ) fun

pIfThenElseExpr :: ParseM a -> (Expr -> a -> a -> b) -> t -> ParseM b
pIfThenElseExpr p h t = do
  keyword "if"
  b  <- pTypedExpr BB
  keyword "then"
  tr <- p
  keyword "else"
  fa <- p
  return (h b tr fa)

pMacroExpr :: ([(Type, Key, Expr)] -> Expr -> a) -> ParseM a
pMacroExpr f = do
  try $ keyword "Eval"
  keyword "("
  t <- pType
  keyword ";"
  e <- pTypedExpr (MacTo t)
  vals <- option [] $ many1 (keyword ";" >> (pTypeKeyExpr pTypedExpr))
  keyword ")"
  return (f vals e)



{--------}
{- :Doc -}
{--------}

pBrackDoc :: (Type -> ParseM Expr) -> ParseM (Doc Expr)
pBrackDoc pE = do
  _ <- try $ char '['
  d <- pDoc pE
  _ <- char ']'
  whitespace
  return d

pBrackDocE :: (Type -> ParseM Expr) -> ParseM Expr
pBrackDocE pE = do
  _ <- try $ char '['
  d <- pDoc pE
  _ <- char ']'
  whitespace
  return (DocE d)

pREPL :: ParseM (Doc Expr)
pREPL = do
  x <- choice $ map pAtLocus
         [ pDefineREPL
         , pNakedExprREPL
         , pVarExpr NakedKey DD
         , pImportREPL
         ]
  eof
  return x
  where
    pDefineREPL = do
      try $ keyword "define"
      t <- pType
      whitespace
      k <- pKey
      keyword ":="
      v <- if t == DD then (pBrackDocE pTypedExpr) else pTypedExpr t
      return (Define t k v (Empty :@ NullLocus))

    pNakedExprREPL = do
      x <- try (pTypedNakedExpr pTypedExpr)
      return (NakedExpr x)

    pImportREPL = do
      try $ keyword "import"
      path <- pParens pPath
      qual <- option Nothing (try (keyword "as") >> pToken >>= return . Just)
      return (Import path qual (Empty :@ NullLocus))


pDoc :: (Type -> ParseM Expr) -> ParseM (Doc Expr)
pDoc pE = choice $ map pAtLocus
  [ eof >> return Empty
  , lookAhead (char ']') >> return Empty
  , do
    xs <- many1 $ choice $ map pAtLocus
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
      rest <- pDoc pE
      return (Import path qual rest)


    pDefine = do
      try (char '[' >> keyword "define")
      t <- pType
      whitespace
      k <- pKey
      keyword ":="
      v <- if t == DD then (pBrackDocE pE) else pE t
      _ <- option () (try (keyword "enddefine")) >> char ']'
      rest <- pDoc pE
      return (Define t k v rest)

    pScope = do
      try (char '[' >> keyword "scope")
      x <- pBrackDoc pE
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
      xs <- many (pBrackDoc pE)
      option () (try $ keyword "endshuffle")
      _ <- whitespace >> char ']'
      return (Shuffle xs)

    pIfThenElse = do
      try (char '[' >> keyword "if")
      test  <- pE BB
      true  <- keyword "then" >> (pBrackDoc pE)
      false <- keyword "else" >> (pBrackDoc pE)
      option () (try (keyword "endif"))
      _ <- whitespace >> char ']'
      return (IfThenElse test true false)

    pCond = do
      try (char '[' >> keyword "cond")
      cases <- many pCondCase
      auto <- option (Empty :@ NullLocus) (try (keyword "default") >> (pBrackDoc pE))
      option () (try (keyword "endcond"))
      _ <- whitespace >> char ']'
      return (Cond cases auto)
      where
        pCondCase = do
          s <- try (keyword "case") >> (pTypedExpr BB)
          t <- pBrackDoc pE
          return (s,t)

    pFor = do
      try (char '[' >> keyword "for")
      typ <- pType
      spaces
      k <- pKey
      r <- keyword "in"  >> (pE $ ListOf typ)
      t <- keyword "say" >> (pBrackDoc pE)
      b <- option Nothing $ (try (keyword "sepby")) >> (pBrackDoc pE) >>= (return . Just)
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
        pAltOpt = try (keyword "opt") >> (pBrackDoc pE)

    pBail = do
      try (char '[' >> keyword "bail")
      message <- pE SS
      option () (try (keyword "endbail"))
      _ <- whitespace >> char ']'
      return (Bail message)

    pLetIn = do
      try (char '[' >> keyword "let")
      (_,k,e) <- (pTypeKeyExpr pE)
      body <- keyword "in" >> (pBrackDoc pE)
      option () (try (keyword "endlet"))
      _ <- whitespace >> char ']'
      return (LetIn k e body)

    pSelect = do
      try (char '[' >> keyword "select")
      ty <- pType
      k <- spaced pKey
      r <- keyword "from" >> (pE $ ListOf ty)
      t <- keyword "in"   >> (pBrackDoc pE)
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



{---------}
{- :Expr -}
{---------}

pTypedExpr :: Type -> ParseM Expr
pTypedExpr DD = fmap DocE  (pDoc      pTypedExpr)
pTypedExpr SS = fmap StrE  (pStrExpr  pTypedExpr)
pTypedExpr ZZ = fmap IntE  (pIntExpr  pTypedExpr)
pTypedExpr BB = fmap BoolE (pBoolExpr pTypedExpr)
pTypedExpr QQ = fmap RatE  (pRatExpr  pTypedExpr)

pTypedExpr (ZZMod    n) = fmap ZZModE (pZZModExpr     pTypedExpr n)
pTypedExpr (ListOf   t) = fmap ListE  (pTypedListExpr t pTypedExpr)
pTypedExpr (MatOf    t) = fmap MatE   (pTypedMatExpr  t pTypedExpr)
pTypedExpr (MacTo    t) = fmap MacE   (pTypedMacExpr  t)
pTypedExpr (PermOf   t) = fmap PermE  (pTypedPermExpr t pTypedExpr)
pTypedExpr (PolyOver t) = fmap PolyE  (pTypedPolyExpr t pTypedExpr)

pTypedExpr XX = choice
  [ pTypedExpr ZZ
  , pTypedExpr QQ
  , pTypedExpr BB
  , pTypedExpr SS
  , fmap ListE (pListExpr pTypedExpr)
  , fmap MatE  (pMatExpr  pTypedExpr)
  , fmap PolyE (pPolyExpr pTypedExpr)
  ]


pTypedNakedExpr :: (Type -> ParseM Expr) -> ParseM Expr
pTypedNakedExpr pE = do
  t <- pType
  keyword ":"
  pE t

pTypedConst :: Type -> ParseM Expr
pTypedConst SS           = fmap StrE   pStrConst
pTypedConst ZZ           = fmap IntE   pIntConst
pTypedConst BB           = fmap BoolE  pBoolConst
pTypedConst QQ           = fmap RatE   pRatConst
pTypedConst (ZZMod n)    = fmap ZZModE (pZZModConst n)

pTypedConst (ListOf   t) = fmap ListE  (pListConst t pTypedConst)
pTypedConst (MatOf    t) = fmap MatE   (pMatConst  t pTypedConst)
pTypedConst (PolyOver t) = fmap PolyE  (pPolyConst t pTypedConst)
pTypedConst (PermOf   t) = fmap PermE  (pPermConst t pTypedConst)
pTypedConst (MacTo    t) = fmap MacE   (pMacConst  t)

pTypedConst _ = error "pTypedConst"





{------------}
{- :MacExpr -}
{------------}

pMacConst :: Type -> ParseM (MacExpr Expr)
pMacConst typ = pAtLocus $ pMacConst' typ pTypedExpr

pMacConst' :: Type -> (Type -> ParseM Expr) -> ParseM (MacExprLeaf Expr)
pMacConst' typ pE = do
  start <- getPosition
  try $ keyword "Macro"
  keyword "("
  t <- pType
  keyword ";"
  body <- if t == DD then (pBrackDocE pE) else pE t
  vals <- option [] $ many (keyword ";" >> (pTypeKeyExpr pE))
  keyword ")"
  end <- getPosition
  case unify typ t of
    Left _ -> reportParseErr (locus start end) $ TypeUnificationError typ t
    Right u -> return (MacConst u vals body (emptyStore, False))


pMacExpr :: ParseM (MacExpr Expr)
pMacExpr = pTypedMacExpr XX


pTypedMacExpr :: Type -> ParseM (MacExpr Expr)
pTypedMacExpr typ = spaced $ buildExpressionParser macOpTable pMacTerm
  where
    pMacTerm = pTerm (pMacConst' typ pTypedExpr) pMacExpr "macro expression"
      [ pVarExpr (MacVar typ) (MacTo typ)

      , pAtPos (MacTo typ) (MacAtPos typ)
      , pAtIdx (MacTo typ) (MacAtIdx typ)

      , pMacroExpr (MacMacro typ)

      , pFun1 "Rand" (pTypedExpr $ ListOf (MacTo typ)) (MacRand typ)

      , pIfThenElseExpr (pTypedMacExpr typ) (MacIfThenElse typ) (MacTo typ)
      ]

    macOpTable = []
