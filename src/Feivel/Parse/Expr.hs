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

import Feivel.Expr
import Feivel.Key
import Feivel.Parse.Key
import Feivel.Parse.Type
import Feivel.Parse.Util
import Feivel.Parse.ParseM
import Feivel.Locus
import Feivel.Type
import Feivel.Error
import Feivel.Store (emptyStore)

import Feivel.Lib (mFromRowList, Variable(..), Natural(..), fromListP, Monomial, fromListM, identityM, nullP, mapFst, fromCycles, zzmod, Text(..), idPerm)

import Text.Parsec.Expr
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim (try)

{---------------}
{- Contents    -}
{-  :Doc       -}
{-  :Expr      -}
{-  :StrExpr   -}
{-  :IntExpr   -}
{-  :RatExpr   -}
{-  :ZZModExpr -}
{-  :BoolExpr  -}
{-  :ListExpr  -}
{-  :MatExpr   -}
{-  :PolyExpr  -}
{-  :PermExpr  -}
{-  :MacExpr   -}
{---------------}

{--------------}
{- :Utilities -}
{--------------}

-- Terms in Expression Grammars
pTerm :: ParseM a -> ParseM (AtLocus a) -> String -> [ParseM a] -> ParseM (AtLocus a)
pTerm cst expr err atoms = 
  choice [try $ pAtLocus atom | atom <- cst:atoms] <|> (pParens expr) <?> err

-- Unary Operators (for expression parser)
opParser1 :: (AtLocus a -> a) -> String -> ParseM ((AtLocus a) -> (AtLocus a))
opParser1 f fun = do
  start <- getPosition
  try $ keyword fun
  end <- getPosition
  return $ \x -> (f x :@ (locus start end))

-- Binary Operators (for expression parser)
opParser2 :: (AtLocus a -> AtLocus a -> a) -> String -> ParseM ((AtLocus a) -> (AtLocus a) -> (AtLocus a))
opParser2 f fun = do
  start <- getPosition
  try $ keyword fun
  end <- getPosition
  return $ \x y -> (f x y :@ (locus start end))

{- Term Parsing -}

pConst :: ParseM a -> (a -> b) -> t -> ParseM b
pConst p h t = do
  x <- p
  return (h x)

pVarExpr :: (Key -> a) -> Type -> ParseM a
pVarExpr h t = do
  k <- pKey
  return (h k)

pAtPos :: Type -> (Expr -> Expr -> b) -> ParseM b
pAtPos typ fun = pFun2 "AtPos" (pTypedExpr (ListOf typ)) (pTypedExpr ZZ) fun typ

pAtIdx :: Type -> (Expr -> Expr -> Expr -> b) -> ParseM b
pAtIdx typ fun = pFun3 "AtIdx" (pTypedExpr (MatOf typ)) (pTypedExpr ZZ) (pTypedExpr ZZ) fun typ

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
  vals <- option [] $ many1 (keyword ";" >> pTypeKeyExpr)
  keyword ")"
  return (f vals e)

pPair :: b -> ParseM a -> ParseM (a,b)
pPair b p = do
  a <- p
  return (a,b)

{- Type-dependent expressions -}

pTypedArg :: String -> (Type -> ParseM a) -> (a -> t) -> (Type -> u) -> ParseM t
pTypedArg fun pA con typ = do
  try $ keyword fun
  keyword "("
  t <- pType
  keyword ";"
  a <- pA t
  keyword ")"
  return (con a)

pTypedArgPair
 :: String -> (Type -> ParseM a) -> (Type -> ParseM b)
     -> (a -> b -> t) -> (Type -> u) -> ParseM t
pTypedArgPair fun pA pB con typ = do
  try $ keyword fun
  keyword "("
  t <- pType
  keyword ";"
  a <- pA t
  keyword ";"
  b <- pB t
  keyword ")"
  return (con a b)



{--------}
{- :Doc -}
{--------}

pBrackDoc :: ParseM Doc
pBrackDoc = do
  _ <- try $ char '['
  d <- pDoc
  _ <- char ']'
  whitespace
  return d

pBrackDocE :: ParseM Expr
pBrackDocE = do
  _ <- try $ char '['
  d <- pDoc
  _ <- char ']'
  whitespace
  return (DocE d)

pREPL :: ParseM Doc
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
      (k,_) <- pUntypedKey
      keyword ":="
      v <- if t == DD then pBrackDocE else pTypedExpr t
      return (Define t k v (Empty :@ NullLocus))

    pNakedExprREPL = do
      x <- try pTypedNakedExpr
      return (NakedExpr x)

    pImportREPL = do
      try $ keyword "import"
      path <- pParens pPath
      qual <- option Nothing (try (keyword "as") >> pToken >>= return . Just)
      return (Import path qual (Empty :@ NullLocus))


pDoc :: ParseM Doc
pDoc = choice $ map pAtLocus
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
      , pQuit
      , pNote
      ]
    return (Cat xs)
  ]
  where
    pEval = do
      try (char '[' >> keyword "eval")
      e <- pTypedExpr (MacTo DD)
      vals <- option [] pVals
      _ <- option () (try (keyword "endeval")) >> char ']'
      return (DocMacro vals e)
        where
          pVals = do
            _ <- try $ keyword "("
            vs <- sepBy pTypeKeyExpr (keyword ";")
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
      rest <- pDoc
      return (Import path qual rest)


    pDefine = do
      try (char '[' >> keyword "define")
      t <- pType
      whitespace
      k <- pKey
      keyword ":="
      v <- if t == DD then pBrackDocE else pTypedExpr t
      _ <- option () (try (keyword "enddefine")) >> char ']'
      rest <- pDoc
      return (Define t k v rest)

    pScope = do
      try (char '[' >> keyword "scope")
      x <- pBrackDoc
      option () (try (keyword "endscope"))
      _ <- whitespace >> char ']'
      return (Scope x)

    pNakedExpr = do
      try (char '[' >> keyword ":")
      x <- try pTypedNakedExpr
      _ <- keyword ":" >> char ']'
      return (NakedExpr x)

    pShuffle = do
      try (char '[' >> keyword "shuffle")
      xs <- many pBrackDoc
      option () (try $ keyword "endshuffle")
      _ <- whitespace >> char ']'
      return (Shuffle xs)

    pIfThenElse = do
      try (char '[' >> keyword "if")
      test  <- pBoolExpr
      true  <- keyword "then" >> pBrackDoc
      false <- keyword "else" >> pBrackDoc
      option () (try (keyword "endif"))
      _ <- whitespace >> char ']'
      return (IfThenElse test true false)

    pCond = do
      try (char '[' >> keyword "cond")
      cases <- many pCondCase
      auto <- option (Empty :@ NullLocus) (try (keyword "default") >> pBrackDoc)
      option () (try (keyword "endcond"))
      _ <- whitespace >> char ']'
      return (Cond cases auto)
      where
        pCondCase = do
          s <- try (keyword "case") >> pBoolExpr
          t <- pBrackDoc
          return (s,t)

    pFor = do
      try (char '[' >> keyword "for")
      typ <- pType
      spaces
      k <- pKey
      r <- keyword "in"  >> pTypedListExpr typ
      t <- keyword "say" >> pBrackDoc
      b <- option Nothing $ (try (keyword "sepby")) >> pBrackDoc >>= (return . Just)
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
        pAltOpt = try (keyword "opt") >> pBrackDoc

    pBail = do
      try (char '[' >> keyword "bail")
      message <- pStrExpr
      option () (try (keyword "endbail"))
      _ <- whitespace >> char ']'
      return (Bail message)

    pLetIn = do
      try (char '[' >> keyword "let")
      (_,k,e) <- pTypeKeyExpr
      body <- keyword "in" >> pBrackDoc
      option () (try (keyword "endlet"))
      _ <- whitespace >> char ']'
      return (LetIn k e body)

    pSelect = do
      try (char '[' >> keyword "select")
      ty <- pType
      k <- spaced pKey
      r <- keyword "from" >> pTypedListExpr ty
      t <- keyword "in"   >> pBrackDoc
      option () (try (keyword "endselect"))
      _ <- whitespace >> char ']'
      return (Select k r t)

    pQuit = do
      try (char '[' >> keyword "quit")
      _ <- spaces >> char ']'
      reportParseErr NullLocus Quit

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
pTypedExpr DD = fmap DocE  pDoc
pTypedExpr SS = fmap StrE  pStrExpr
pTypedExpr ZZ = fmap IntE  pIntExpr
pTypedExpr BB = fmap BoolE pBoolExpr
pTypedExpr QQ = fmap RatE  pRatExpr

pTypedExpr (ZZMod    n) = fmap ZZModE (pZZModExpr     n)
pTypedExpr (ListOf   t) = fmap ListE  (pTypedListExpr t)
pTypedExpr (MatOf    t) = fmap MatE   (pTypedMatExpr  t)
pTypedExpr (MacTo    t) = fmap MacE   (pTypedMacExpr  t)
pTypedExpr (PermOf   t) = fmap PermE  (pTypedPermExpr t)
pTypedExpr (PolyOver t) = fmap PolyE  (pTypedPolyExpr t)

pTypedExpr XX = choice
  [ pTypedExpr ZZ
  , pTypedExpr QQ
  , pTypedExpr BB
  , pTypedExpr SS
  , fmap ListE pListExpr
  , fmap MatE pMatExpr
  , fmap PolyE pPolyExpr
  ]



pTypeKeyExpr :: ParseM (Type, Key, Expr)
pTypeKeyExpr = do
  t <- pType
  whitespace
  k <- pKey
  keyword ":="
  v <- pTypedExpr t
  return (t, k, v)

pTypedNakedExpr :: ParseM Expr
pTypedNakedExpr = do
  t <- pType
  keyword ":"
  pTypedExpr t

pTypedConst :: Type -> ParseM Expr
pTypedConst SS           = fmap StrE   pStrConst
pTypedConst ZZ           = fmap IntE   pIntConst
pTypedConst BB           = fmap BoolE  pBoolConst
pTypedConst QQ           = fmap RatE   pRatConst
pTypedConst (ZZMod n)    = fmap ZZModE (pZZModConst n)

pTypedConst (ListOf   t) = fmap ListE  (pListConst t)
pTypedConst (MatOf    t) = fmap MatE   (pMatConst  t)
pTypedConst (PolyOver t) = fmap PolyE  (pPolyConst t)
pTypedConst (PermOf   t) = fmap PermE  (pPermConst t)
pTypedConst (MacTo    t) = fmap MacE   (pMacConst  t)

pTypedConst _ = error "pTypedConst"



{------------}
{- :StrExpr -}
{------------}

pStrConst :: ParseM StrExpr
pStrConst = pAtLocus pStrConst'

pStrConst' :: ParseM StrExprLeaf
pStrConst' = pConst pText StrConst SS

pStrExpr :: ParseM StrExpr
pStrExpr = spaced $ buildExpressionParser strOpTable pStrTerm
  where
    pStrTerm = pTerm pStrConst' pStrExpr "string expression"
      [ pVarExpr StrVar SS
      , pMacroExpr StrMacro

      , pIfThenElseExpr pStrExpr StrIfThenElse SS

      , pAtPos SS StrAtPos
      , pAtIdx SS StrAtIdx
    
      , pFun2 "Strip" pStrExpr pStrExpr StrStrip SS
      , pFun1 "Reverse" pStrExpr Reverse SS
      , pFun1 "ToUpper" pStrExpr ToUpper SS
      , pFun1 "ToLower" pStrExpr ToLower SS
      , pFun1 "Rot13"   pStrExpr Rot13   SS

      , pFun1 "Rand"    (pTypedListExpr SS) StrRand SS

      , pFun1 "int" pIntExpr StrIntCast SS
    
      , pFun1 "Hex"    pIntExpr StrHex SS
      , pFun1 "Roman"  pIntExpr StrRoman SS
      , pFun1 "Base36" pIntExpr StrBase36 SS
    
      , pFun2 "Decimal" pRatExpr pIntExpr StrDecimal SS
    
      , pTypedArg "Tab" pTypedMatExpr StrTab (const SS)

      , pStrFormat
      ]
      where
        pStrFormat = do
          try $ keyword "Format"
          keyword "("
          t <- pType
          keyword ";"
          f <- pFormat
          keyword ";"
          e <- pTypedExpr t
          keyword ")"
          return (StrFormat f e)

    strOpTable =
      [ [Infix (opParser2 Concat "++") AssocLeft
        ]
      ]



{------------}
{- :IntExpr -}
{------------}

pIntConst :: ParseM IntExpr
pIntConst = pAtLocus pIntConst'

pIntConst' :: ParseM (IntExprLeaf Expr)
pIntConst' = pConst pInteger IntConst ZZ

pIntExpr :: ParseM IntExpr
pIntExpr = spaced $ buildExpressionParser intOpTable pIntTerm
  where
    pIntTerm = pTerm pIntConst' pIntExpr "integer expression"
      [ pVarExpr IntVar ZZ
      , pMacroExpr IntMacro

      , pAtPos ZZ IntAtPos
      , pAtIdx ZZ IntAtIdx
    
      , pIfThenElseExpr pIntExpr IntIfThenElse ZZ

      , pFun1 "SquarePart"     pIntExpr IntSqPart     ZZ
      , pFun1 "SquareFreePart" pIntExpr IntSqFreePart ZZ
      , pFun1 "Rad"            pIntExpr IntRad        ZZ

      , pFun1 "Length" (pTypedExpr (ListOf XX)) ListLen  ZZ
      , pFun1 "Rand"   (pTypedExpr (ListOf ZZ)) IntRand  ZZ
      , pFun1 "Sum"    (pTypedExpr (ListOf ZZ)) IntSum   ZZ
      , pFun1 "Prod"   (pTypedExpr (ListOf ZZ)) IntProd  ZZ
      , pFun1 "Min"    (pTypedExpr (ListOf ZZ)) IntMinim ZZ
      , pFun1 "Max"    (pTypedExpr (ListOf ZZ)) IntMaxim ZZ
      , pFun1 "GCD"    (pTypedExpr (ListOf ZZ)) IntGCDiv ZZ
      , pFun1 "LCM"    (pTypedExpr (ListOf ZZ)) IntLCMul ZZ
    
      , pFun1 "Numerator"   (pTypedExpr QQ)  RatNumer ZZ
      , pFun1 "Denominator" (pTypedExpr QQ)  RatDenom ZZ
      , pFun1 "Floor"       (pTypedExpr QQ)  RatFloor ZZ
    
      , pFun1 "StrLen" (pTypedExpr SS)  StrLength ZZ

      , pFun1 "NumRows" (pTypedExpr (MatOf XX)) MatNumRows ZZ
      , pFun1 "NumCols" (pTypedExpr (MatOf XX)) MatNumCols ZZ
      , pMatRank

      , pFun1 "PolyContent" (pTypedExpr (PolyOver ZZ)) IntContent ZZ
    
      , pFun2 "Uniform"  pIntExpr pIntExpr IntObserveUniform ZZ
      , pFun2 "Binomial" pIntExpr (pTypedExpr QQ) IntObserveBinomial ZZ
      , pFun1 "Poisson"  (pTypedExpr QQ) IntObservePoisson ZZ

      , pFun1 "str" (pTypedExpr SS) IntCastStr ZZ
      ]
      where
        pMatRank = do
          try $ keyword "MatrixRank"
          keyword "("
          t <- pType
          keyword ";"
          m <- pTypedExpr (MatOf t)
          keyword ")"
          return (MatRank m)

    intOpTable =
      [ [ Infix (opParser2 IntPow "^") AssocRight
        ]
      , [ Infix (opParser2 IntMult "*") AssocLeft
        ]
      , [ Infix (opParser2 IntQuo "div") AssocLeft
        , Infix (opParser2 IntMod "mod") AssocLeft
        ]
      , [ Prefix (opParser1 IntNeg "neg")
        , Prefix (opParser1 IntAbs "abs")
        ]
      , [ Infix (opParser2 IntAdd "+") AssocLeft
        , Infix (opParser2 IntSub "-") AssocLeft
        ]
      , [ Infix (opParser2 IntMin "min") AssocLeft
        , Infix (opParser2 IntMax "max") AssocLeft
        , Infix (opParser2 IntGCD "gcd") AssocLeft
        , Infix (opParser2 IntLCM "lcm") AssocLeft
        ]
      , [ Infix (opParser2 IntChoose "choose") AssocLeft]
      ]



{--------------}
{- :ZZModExpr -}
{--------------}

pZZModConst :: Integer -> ParseM (ZZModExpr Expr)
pZZModConst n = pAtLocus (pZZModConst' n)

pZZModConst' :: Integer -> ParseM (ZZModExprLeaf Expr)
pZZModConst' n = do
  a <- pInteger
  return (ZZModConst (ZZMod n) (a `zzmod` n))

pZZModExpr :: Integer -> ParseM (ZZModExpr Expr)
pZZModExpr n = spaced $ buildExpressionParser zzModOpTable pZZModTerm
  where
    pZZModTerm = pTerm (pZZModConst' n) (pZZModExpr n) "integer expression"
      [ pVarExpr (ZZModVar (ZZMod n)) (ZZMod n)
      , pMacroExpr (ZZModMacro (ZZMod n))

      , pAtPos (ZZMod n) (ZZModAtPos (ZZMod n))
      , pAtIdx (ZZMod n) (ZZModAtIdx (ZZMod n))
    
      , pIfThenElseExpr (pZZModExpr n) (ZZModIfThenElse (ZZMod n)) (ZZMod n)

      , pFun1 "int" (pTypedExpr ZZ) (ZZModCast (ZZMod n)) (ZZMod n)

      , pFun2 "Pow" (pZZModExpr n) (pTypedExpr ZZ) (ZZModPow (ZZMod n)) (ZZMod n)

      , pFun1 "Sum"    (pTypedExpr $ ListOf (ZZMod n)) (ZZModSum (ZZMod n))   (ZZMod n)
      , pFun1 "Prod"   (pTypedExpr $ ListOf (ZZMod n)) (ZZModProd (ZZMod n))  (ZZMod n)
      ]

    zzModOpTable =
      [ [ Infix (opParser2 (ZZModMult (ZZMod n)) "*") AssocLeft
        ]
      , [ Prefix (opParser1 (ZZModNeg (ZZMod n)) "neg")
        , Prefix (opParser1 (ZZModInv (ZZMod n)) "inv")
        ]
      , [ Infix (opParser2 (ZZModAdd (ZZMod n)) "+") AssocLeft
        , Infix (opParser2 (ZZModSub (ZZMod n)) "-") AssocLeft
        ]
      ]



{-------------}
{- :BoolExpr -}
{-------------}

pBoolConst :: ParseM BoolExpr
pBoolConst = pAtLocus pBoolConst'

pBoolConst' :: ParseM (BoolExprLeaf Expr)
pBoolConst' = pConst pBool BoolConst BB

pBoolExpr :: ParseM BoolExpr
pBoolExpr = spaced $ buildExpressionParser boolOpTable pBoolTerm
  where
    pBoolTerm = pTerm pBoolConst' pBoolExpr "boolean expression"
      [ pVarExpr BoolVar BB
      , pMacroExpr BoolMacro

      , pAtPos BB BoolAtPos
      , pAtIdx BB BoolAtIdx

      , pIfThenElseExpr pBoolExpr BoolIfThenElse BB
    
      , pFun1 "IsDefined" pKey IsDefined BB

      , pFun1 "IsSquareFree" (pTypedExpr ZZ) IntSqFree BB

      , pFun1 "Rand" (pTypedExpr $ ListOf BB) BoolRand BB

      , pTypedArgPair "Elem" pTypedExpr (pTypedExpr . ListOf) ListElem (const BB)
      , pTypedArg "IsEmpty" (pTypedExpr . ListOf) ListIsEmpty (const BB)

      , pFun1 "IsRow" (pTypedExpr $ MatOf XX) MatIsRow BB
      , pFun1 "IsCol" (pTypedExpr $ MatOf XX) MatIsCol BB
      , pTypedArg "IsGJForm" (pTypedExpr . MatOf) MatIsGJForm (const BB)

      , pTypedArgPair "Equal"    pTypedExpr pTypedExpr BoolEq  (const BB)
      , pTypedArgPair "NotEqual" pTypedExpr pTypedExpr BoolNEq (const BB)
      , pTypedArgPair "LT"       pTypedExpr pTypedExpr BoolLT  (const BB)
      , pTypedArgPair "LEq"      pTypedExpr pTypedExpr BoolLEq (const BB)
      , pTypedArgPair "GT"       pTypedExpr pTypedExpr BoolGT  (const BB)
      , pTypedArgPair "GEq"      pTypedExpr pTypedExpr BoolGEq (const BB)

      , pFun2 "Matches" (pTypedExpr SS) pText    Matches BB
      , pFun2 "Divides" (pTypedExpr ZZ) (pTypedExpr ZZ) IntDiv  BB
      ]

    boolOpTable =
      [ [ Prefix (opParser1 Neg "~")
        ]
      , [ Infix (opParser2 Conj "&&") AssocLeft
        ]
      , [ Infix (opParser2 Disj "||") AssocLeft
        ]
      , [ Infix (opParser2 Imp  "->") AssocRight
        ]
      ]



{------------}
{- :RatExpr -}
{------------}

pRatConst :: ParseM RatExpr
pRatConst = pAtLocus pRatConst'

pRatConst' :: ParseM (RatExprLeaf Expr)
pRatConst' = pConst pRat RatConst QQ

pRatExpr :: ParseM RatExpr
pRatExpr = spaced $ buildExpressionParser ratOpTable pRatTerm
  where
    pRatTerm = pTerm pRatConst' pRatExpr "rational expression"
      [ pVarExpr RatVar QQ
      , pMacroExpr RatMacro

      , pAtPos QQ RatAtPos
      , pAtIdx QQ RatAtIdx

      , pIfThenElseExpr pRatExpr RatIfThenElse QQ

      , pFun1 "Rand"   (pTypedExpr (ListOf QQ)) RatRand  QQ
      , pFun1 "Sum"    (pTypedExpr (ListOf QQ)) RatSum   QQ
      , pFun1 "Prod"   (pTypedExpr (ListOf QQ)) RatProd  QQ
      , pFun1 "Min"    (pTypedExpr (ListOf QQ)) RatMinim QQ
      , pFun1 "Max"    (pTypedExpr (ListOf QQ)) RatMaxim QQ

      , pFun1 "int" (pTypedExpr ZZ) RatCast QQ
      , pFun2 "Pow" pRatExpr (pTypedExpr ZZ) RatPow QQ
      , pMean
      , pFun2 "Sqrt" pRatExpr (pTypedExpr ZZ) RatSqrt QQ
      , pMeanDev
      , pStdDev
      , pZScore

      , pFun1 "str" (pTypedExpr SS) RatCastStr QQ
      ]
      where
        pMean = do
          try $ keyword "Mean"
          keyword "("
          t <- pType
          keyword ";"
          ks <- pTypedExpr (ListOf t)
          keyword ")"
          return (RatMean ks)

        pMeanDev = do
          try $ keyword "MeanDev"
          keyword "("
          t <- pType
          keyword ";"
          ks <- pTypedExpr (ListOf t)
          keyword ")"
          return (RatMeanDev ks)

        pStdDev = do
          try $ keyword "StdDev"
          keyword "("
          t <- pType
          keyword ";"
          ks <- pTypedExpr (ListOf t)
          keyword ";"
          n <- pTypedExpr ZZ
          keyword ")"
          return (RatStdDev ks n)

        pZScore = do
          try $ keyword "ZScore"
          keyword "("
          t <- pType
          keyword ";"
          p <- pRatExpr
          keyword ";"
          ks <- pTypedExpr (ListOf t)
          keyword ";"
          n <- pTypedExpr ZZ
          keyword ")"
          return (RatZScore p ks n)
    
    ratOpTable =
      [ [ Prefix (opParser1 RatNeg "neg" )
        , Prefix (opParser1 RatAbs "abs")
        ]
      , [ Infix (opParser2 RatMult "*") AssocLeft
        , Infix (opParser2 RatQuot "/") AssocLeft
        ]
      , [ Infix (opParser2 RatAdd "+") AssocLeft
        , Infix (opParser2 RatSub "-") AssocLeft
        ]
      , [ Infix (opParser2 RatMin "min") AssocLeft
        , Infix (opParser2 RatMax "max") AssocLeft
        ]
      ]



{-------------}
{- :ListExpr -}
{-------------}

pListLiteral :: Type -> ParseM ListExpr
pListLiteral typ = pAtLocus $ pListLiteralOf typ pTypedExpr

pListConst :: Type -> ParseM ListExpr
pListConst typ = pAtLocus $ pListLiteralOf typ pTypedConst

pListLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM ListExprLeaf
pListLiteralOf typ p = do
    start <- getPosition
    xs <- pBraceList (p typ)
    end <- getPosition
    return (ListConst typ xs)

pListExpr :: ParseM ListExpr
pListExpr = pTypedListExpr XX

pTypedListExpr :: Type -> ParseM ListExpr
pTypedListExpr typ = spaced $ buildExpressionParser listOpTable pListTerm
  where
    pListTerm = pTerm (pListLiteralOf typ pTypedExpr) (pTypedListExpr typ) "list expression"
      [ pVarExpr (ListVar typ) (ListOf typ)

      , pMacroExpr (ListMacro typ)

      , pAtPos (ListOf typ) (ListAtPos typ)
      , pAtIdx (ListOf typ) (ListAtIdx typ)

      , pIfThenElseExpr (pTypedListExpr typ) (ListIfThenElse typ) (ListOf typ)

      , pFun1 "Rand" (pTypedListExpr (ListOf typ)) (ListRand typ) (ListOf typ)

      , pFun1 "Reverse"  (pTypedListExpr typ) (ListRev typ)     (ListOf typ)
      , pFun1 "Sort"     (pTypedListExpr typ) (ListSort typ)    (ListOf typ)
      , pFun1 "Unique"   (pTypedListExpr typ) (ListUniq typ)    (ListOf typ)
      , pFun1 "Shuffle"  (pTypedListExpr typ) (ListShuffle typ) (ListOf typ)
      , pListShuffles

      , pFun2 "GetRow" pIntExpr (pTypedMatExpr typ) (ListMatRow typ) (ListOf typ)
      , pFun2 "GetCol" pIntExpr (pTypedMatExpr typ) (ListMatCol typ) (ListOf typ)

      , pListPermsOf

      , pListRange
      , pListPivotColIndices typ
      , pListBuilder
      , pFun2 "Choose" pIntExpr (pTypedListExpr typ) (ListChoose typ) (ListOf typ)
      , pListChoices
      , pListFilter
      ]
      where
        pListFilter = do
          _ <- try $ keyword "Filter"
          keyword "("
          k <- pKey
          keyword ";"
          g <- pBoolExpr
          keyword ";"
          xs <- pTypedListExpr typ
          keyword ")"
          return (ListFilter typ k g xs)

        pListChoices = do
          _ <- try $ keyword "Choices"
          case typ of
            ListOf t -> do
              keyword "("
              n <- pIntExpr
              keyword ";"
              xs <- pTypedListExpr t
              keyword ")"
              return (ListChoices typ n xs)
            _ -> error "pListChoices"

        pListShuffles = do
          _ <- try $ keyword "Shuffles"
          case typ of
            ListOf t -> do
              keyword "("
              xs <- pTypedListExpr t
              keyword ")"
              return (ListShuffles typ xs)
            _ -> error "pListShuffles"

        pListPermsOf = do
          _ <- try $ keyword "PermutationsOf"
          case typ of
            PermOf t -> do
              keyword "("
              xs <- pTypedListExpr t
              keyword ")"
              return (ListPermsOf typ xs)
            _ -> error "pListPermsOf"

        pListRange = case unify typ ZZ of
          Right _ -> do
            try $ keyword "Range"
            (a,b) <- pTuple2 pIntExpr pIntExpr
            return (ListRange ZZ a b)
          Left _ -> fail "pListRange"

        pListPivotColIndices ZZ = do
          try $ keyword "PivotCols"
          keyword "("
          t <- pType
          keyword ";"
          m <- pTypedMatExpr t
          keyword ")"
          return (ListPivotColIndices ZZ m)
        pListPivotColIndices t = fail "pListPivotColIndices"
    
        pListBuilder = do
          start <- getPosition
          try $ keyword "Build"
          keyword "("
          expr <- pTypedExpr typ
          keyword ";"
          gds <- sepBy1 (pListBind <|> pListGuard) (keyword ";")
          keyword ")"
          end <- getPosition
          return (ListBuilder typ expr gds)
            where
              pListBind = do
                w <- try pType
                whitespace
                k <- pKey
                keyword "<-"
                ls <- pTypedListExpr w
                return $ Bind k ls
          
              pListGuard = do
                e <- try pBoolExpr
                return $ Guard e
    
    listOpTable =
      [ [ Infix (opParser2 (ListCat typ) "++") AssocLeft
        ]
      , [ Infix (opParser2 (ListToss typ) "\\\\") AssocLeft
        ]
      ]



{------------}
{- :MatExpr -}
{------------}

pMatLiteral :: Type -> ParseM MatExpr
pMatLiteral typ = pAtLocus $ pMatLiteralOf typ pTypedExpr

pMatConst :: Type -> ParseM MatExpr
pMatConst typ = pAtLocus $ pMatLiteralOf typ pTypedConst

pMatLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM MatExprLeaf
pMatLiteralOf typ p = do
  start <- getPosition
  xss <- pBrackList (pBrackList (p typ))
  end <- getPosition
  case mFromRowList xss of
    Left err -> reportParseErr (locus start end) err
    Right m -> return (MatConst typ m)

pMatExpr :: ParseM MatExpr
pMatExpr = pTypedMatExpr XX

pTypedMatExpr :: Type -> ParseM MatExpr
pTypedMatExpr typ = spaced $ buildExpressionParser matOpTable pMatTerm
  where
    pMatTerm = pTerm (pMatLiteralOf typ pTypedExpr) (pTypedMatExpr typ) "matrix expression"
      [ pVarExpr (MatVar typ) (MatOf typ)

      , pAtPos (MatOf typ) (MatAtPos typ)
      , pAtIdx (MatOf typ) (MatAtIdx typ)

      , pMacroExpr (MatMacro typ)

      , pIfThenElseExpr (pTypedMatExpr typ) (MatIfThenElse typ) (MatOf typ)

      , pFun1 "Transpose" (pTypedMatExpr typ) (MatTrans typ) (MatOf typ)

      , pFun1 "ShuffleRows" (pTypedMatExpr typ) (MatShuffleRows typ) (MatOf typ)
      , pFun1 "ShuffleCols" (pTypedMatExpr typ) (MatShuffleCols typ) (MatOf typ)

      , pFun1 "RowFromList" (pTypedListExpr typ) (MatRowFromList typ) (MatOf typ)
      , pFun1 "ColFromList" (pTypedListExpr typ) (MatColFromList typ) (MatOf typ)

      , pFun1 "Rand" (pTypedListExpr (MatOf typ)) (MatRand typ) (MatOf typ)

      , pFun2 "GetRow" pIntExpr (pTypedMatExpr typ) (MatGetRow typ) (MatOf typ)
      , pFun2 "GetCol" pIntExpr (pTypedMatExpr typ) (MatGetCol typ) (MatOf typ)

      , pMatBuilder

      , pMatId
      , pMatSwapE
      , pMatScaleE
      , pMatAddE

      , pFun2T "Pow" (pTypedMatExpr typ) pIntExpr (MatPow typ)

      , pFun3T "SwapRows" (pTypedMatExpr typ) pIntExpr pIntExpr (MatSwapRows typ)
      , pFun3T "SwapCols" (pTypedMatExpr typ) pIntExpr pIntExpr (MatSwapCols typ)
      , pFun3T "ScaleRow" (pTypedMatExpr typ) (pTypedExpr typ) pIntExpr (MatScaleRow typ)
      , pFun3T "ScaleCol" (pTypedMatExpr typ) (pTypedExpr typ) pIntExpr (MatScaleCol typ)
      , pFun4T "AddRow"   (pTypedMatExpr typ) (pTypedExpr typ) pIntExpr pIntExpr (MatAddRow typ)
      , pFun4T "AddCol"   (pTypedMatExpr typ) (pTypedExpr typ) pIntExpr pIntExpr (MatAddCol typ)
      , pFun2T "DelRow"   (pTypedMatExpr typ) pIntExpr (MatDelRow typ)
      , pFun2T "DelCol"   (pTypedMatExpr typ) pIntExpr (MatDelCol typ)

      , pFun1T "GJForm"   (pTypedMatExpr typ) (MatGJForm typ)
      , pFun1T "GJFactor" (pTypedMatExpr typ) (MatGJFactor typ)
      ]
      where
        pMatId = do
          try $ keyword "Id"
          (t,n) <- pTuple2 pType pIntExpr
          return (MatId t n)

        pMatSwapE = do
          try $ keyword "SwapE"
          (t,n,h,k) <- pTuple4 pType pIntExpr pIntExpr pIntExpr
          return (MatSwapE t n h k)

        pMatScaleE = do
          try $ keyword "ScaleE"
          keyword "("
          t <- pType
          keyword ";"
          n <- pIntExpr
          keyword ";"
          k <- pIntExpr
          keyword ";"
          e <- pTypedExpr t
          keyword ")"
          return (MatScaleE t n k e)

        pMatAddE = do
          try $ keyword "AddE"
          keyword "("
          t <- pType
          keyword ";"
          n <- pIntExpr
          keyword ";"
          i <- pIntExpr
          keyword ";"
          j <- pIntExpr
          keyword ";"
          e <- pTypedExpr t
          keyword ")"
          return (MatAddE t n i j e)

        pMatBuilder = do
          try $ keyword "Build"
          keyword "("
          e <- pTypedExpr typ
          keyword ";"
          tr <- pType
          whitespace
          kr <- pKey
          keyword "<-"
          lr <- pTypedListExpr tr
          keyword ";"
          tc <- pType
          whitespace
          kc <- pKey
          keyword "<-"
          lc <- pTypedListExpr tc
          keyword ")"
          return (MatBuilder typ e kr lr kc lc)
    
    matOpTable =
      [ [ Prefix (opParser1 (MatNeg typ) "neg")
        ]
      , [ Infix (opParser2 (MatMul typ) "*") AssocLeft
        ]
      , [ Infix (opParser2 (MatAdd typ) "+") AssocLeft
        ]
      , [ Infix (opParser2 (MatHCat typ) "hcat") AssocLeft
        , Infix (opParser2 (MatVCat typ) "vcat") AssocLeft
        ]
      ]



{-------------}
{- :PolyExpr -}
{-------------}

pPolyLiteral :: Type -> ParseM PolyExpr
pPolyLiteral typ = pAtLocus $ pPolyLiteralOf typ pTypedExpr

pPolyConst :: Type -> ParseM PolyExpr
pPolyConst typ = pAtLocus $ pPolyLiteralOf typ pTypedConst

pPolyLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM PolyExprLeaf
pPolyLiteralOf typ p = do
  try $ keyword "Poly"
  keyword "("
  ts <- sepBy1 (pPolyTerm $ p typ) (try $ char ';')
  keyword ")"
  return (PolyConst typ (fromListP ts))
  where
    pPolyTerm :: ParseM a -> ParseM (a, Monomial)
    pPolyTerm q = do
      c <- q
      x <- option identityM $ try (keyword ".") >> (pIdMon <|> pMonomial)
      return (c,x)
    
    pIdMon :: ParseM Monomial
    pIdMon = (try $ char '1') >> return identityM

    pMonomial :: ParseM Monomial
    pMonomial = do
      ps <- sepBy1 pPower (try $ keyword ".")
      return $ fromListM ps
    
    pPower :: ParseM (Variable, Natural)
    pPower = do
      x <- pVar
      k <- option 1 (try (keyword "^") >> pNatural)
      return (x, Nat k)

pPolyExpr :: ParseM PolyExpr
pPolyExpr = pTypedPolyExpr XX

pTypedPolyExpr :: Type -> ParseM PolyExpr
pTypedPolyExpr typ = spaced $ buildExpressionParser polyOpTable pPolyTerm
  where
    pPolyTerm = pTerm (pPolyLiteralOf typ pTypedExpr) (pTypedPolyExpr typ) "polynomial expression"
      [ pVarExpr (PolyVar typ) (PolyOver typ)

      , pAtPos (PolyOver typ) (PolyAtPos typ)
      , pAtIdx (PolyOver typ) (PolyAtIdx typ)

      , pMacroExpr (PolyMacro typ)

      , pIfThenElseExpr (pTypedPolyExpr typ) (PolyIfThenElse typ) (PolyOver typ)

      , pFun1 "Rand" (pTypedListExpr (PolyOver typ)) (PolyRand typ) (PolyOver typ)

      , pPolyPow

      , pPolyNull

      , pFun2 "FromRoots" (pLiftAt pVar XX) (pTypedListExpr typ) (PolyFromRoots typ) (PolyOver typ)

      , pPolyEvalPoly
      ]
      where
        pPolyEvalPoly = do
          try $ keyword "EvalPoly"
          keyword "("
          p <- pTypedPolyExpr typ
          keyword ";"
          xs <- sepBy1 pSubs (keyword ";")
          keyword ")"
          return (PolyEvalPoly typ p xs)
            where
              pSubs = do
                x <- try $ pVar
                keyword "<-"
                q <- pTypedPolyExpr typ
                return (x,q)

        pPolyPow = pFun2T "Pow" (pTypedPolyExpr typ) pIntExpr (PolyPow typ)

        pPolyNull = do
          try $ keyword "Null"
          return (PolyConst typ nullP)

    polyOpTable =
      [ [ Prefix (opParser1 (PolyNeg typ) "neg")
        ]
      , [ Infix (opParser2 (PolyMul typ) "*") AssocLeft
        ]
      , [ Infix (opParser2 (PolyAdd typ) "+") AssocLeft
        , Infix (opParser2 (PolySub typ) "-") AssocLeft
        ]
      ]



{-------------}
{- :PermExpr -}
{-------------}

pPermLiteral :: Type -> ParseM PermExpr
pPermLiteral typ = pAtLocus $ pPermLiteralOf typ pTypedConst

pPermConst :: Type -> ParseM PermExpr
pPermConst typ = pAtLocus $ pPermLiteralOf typ pTypedConst

pPermLiteralOf :: Type -> (Type -> ParseM Expr) -> ParseM PermExprLeaf
pPermLiteralOf typ p = (string "id" >> return (PermConst typ idPerm)) <|> do
  start <- getPosition
  ts <- many1 pCycle
  end <- getPosition
  case fromCycles ts of
    Right q -> return (PermConst typ q)
    Left err -> reportParseErr (locus start end) err
    where
      pCycle = do
        _ <- try $ char '('
        xs <- sepBy1 (p typ) whitespace
        _ <- char ')'
        return xs

pTypedPermExpr :: Type -> ParseM PermExpr
pTypedPermExpr typ = spaced $ buildExpressionParser permOpTable pPermTerm
  where
    pPermTerm = pTerm (pPermLiteralOf typ pTypedExpr) (pTypedPermExpr typ) "permutation expression"
      [ pVarExpr (PermVar typ) (PermOf typ)

      , pAtPos (PermOf typ) (PermAtPos typ)
      , pAtIdx (PermOf typ) (PermAtIdx typ)

      , pMacroExpr (PermMacro typ)

      , pIfThenElseExpr (pTypedPermExpr typ) (PermIfThenElse typ) (PermOf typ)

      , pFun1 "Rand" (pTypedListExpr (PermOf typ)) (PermRand typ) (PermOf typ)
      ]

    permOpTable =
      [ [ Prefix (opParser1 (PermInvert typ) "inv")
        ]
      , [ Infix (opParser2 (PermCompose typ) "o") AssocLeft
        ]
      ]



{------------}
{- :MacExpr -}
{------------}

pMacConst :: Type -> ParseM MacExpr
pMacConst typ = pAtLocus $ pMacConst' typ

pMacConst' :: Type -> ParseM MacExprLeaf
pMacConst' typ = do
  start <- getPosition
  try $ keyword "Macro"
  keyword "("
  t <- pType
  keyword ";"
  body <- if t == DD then pBrackDocE else pTypedExpr t
  vals <- option [] $ many (keyword ";" >> pTypeKeyExpr)
  keyword ")"
  end <- getPosition
  case unify typ t of
    Left _ -> reportParseErr (locus start end) $ TypeUnificationError typ t
    Right u -> return (MacConst u vals body (emptyStore, False))


pMacExpr :: ParseM MacExpr
pMacExpr = pTypedMacExpr XX


pTypedMacExpr :: Type -> ParseM MacExpr
pTypedMacExpr typ = spaced $ buildExpressionParser macOpTable pMacTerm
  where
    pMacTerm = pTerm (pMacConst' typ) pMacExpr "macro expression"
      [ pVarExpr (MacVar typ) (MacTo typ)

      , pAtPos (MacTo typ) (MacAtPos typ)
      , pAtIdx (MacTo typ) (MacAtIdx typ)

      , pMacroExpr (MacMacro typ)

      , pFun1 "Rand" (pTypedListExpr (MacTo typ)) (MacRand typ) (MacTo typ)

      , pIfThenElseExpr (pTypedMacExpr typ) (MacIfThenElse typ) (MacTo typ)
      ]

    macOpTable = []
