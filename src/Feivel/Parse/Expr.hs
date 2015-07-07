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
    pListLiteral, pMatLiteral, pPolyLiteral, pPermLiteral
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

import Feivel.Lib (mFromRowList, Variable(..), Natural(..), fromListP, Monomial, fromListM, identityM, nullP, mapFst, fromCycles, zzmod)

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
pTerm :: ParseM (a, Type) -> ParseM (AtLocus a, Type) -> String -> [ParseM (a, Type)] -> ParseM (AtLocus a, Type)
pTerm cst expr err atoms = 
  choice [try $ pAtLocus atom | atom <- cst:atoms] <|> (pParens expr) <?> err

-- Unary Operators (for expression parser)
opParser1 :: (AtLocus a -> a) -> String -> ParseM ((AtLocus a, t) -> (AtLocus a, t))
opParser1 f fun = do
  start <- getPosition
  try $ keyword fun
  end <- getPosition
  return $ \(x,t) -> (f x :@ (locus start end), t)

-- Binary Operators (for expression parser)
opParser2 :: (AtLocus a -> AtLocus a -> a) -> String -> ParseM ((AtLocus a, t) -> (AtLocus a, t) -> (AtLocus a, t))
opParser2 f fun = do
  start <- getPosition
  try $ keyword fun
  end <- getPosition
  return $ \(x,t) (y,_) -> (f x y :@ (locus start end), t)

{- Term Parsing -}

pConst :: ParseM a -> (a -> b) -> t -> ParseM (b,t) 
pConst p h t = do
  x <- p
  return (h x, t)

pVarExpr :: (Key -> a) -> Type -> ParseM (a,Type)
pVarExpr h t = do
  (k,_) <- pKey
  return (h k, t)

pAtPos :: Type -> (ListExpr -> IntExpr -> b) -> ParseM (b, Type) 
pAtPos typ fun = pFun2 "AtPos" (pTypedListExpr typ) pIntExpr fun typ

pAtIdx :: Type -> (MatExpr -> IntExpr -> IntExpr -> b) -> ParseM (b, Type)
pAtIdx typ fun = pFun3 "AtIdx" (pTypedMatExpr typ) pIntExpr pIntExpr fun typ

pIfThenElseExpr :: ParseM (a,t) -> (BoolExpr -> a -> a -> b) -> t -> ParseM (b,t)
pIfThenElseExpr p h t = do
  keyword "if"
  (b,_) <- pBoolExpr
  keyword "then"
  (tr,_) <- p
  keyword "else"
  (fa,_) <- p
  return (h b tr fa, t)

pMacroExpr :: ([(Type, Key, Expr)] -> MacExpr -> a) -> ParseM (a, Type)
pMacroExpr f = do
  try $ keyword "Eval"
  keyword "("
  t <- pType
  keyword ";"
  (e,_) <- pTypedMacExpr t
  vals <- option [] $ many1 (keyword ";" >> pTypeKeyExpr)
  keyword ")"
  return (f vals e, t)

pPair :: b -> ParseM a -> ParseM (a,b)
pPair b p = do
  a <- p
  return (a,b)

{- Type-dependent expressions -}

pTypedArg :: String -> (Type -> ParseM (a, Type)) -> (a -> t) -> (Type -> u) -> ParseM (t,u)
pTypedArg fun pA con typ = do
  try $ keyword fun
  keyword "("
  t <- pType
  keyword ";"
  (a,_) <- pA t
  keyword ")"
  return (con a, typ t)

pTypedArgPair
 :: String -> (Type -> ParseM (a, Type)) -> (Type -> ParseM (b,Type))
     -> (a -> b -> t) -> (Type -> u) -> ParseM (t, u)
pTypedArgPair fun pA pB con typ = do
  try $ keyword fun
  keyword "("
  t <- pType
  keyword ";"
  (a,_) <- pA t
  keyword ";"
  (b,_) <- pB t
  keyword ")"
  return $ (con a b, typ t)



{--------}
{- :Doc -}
{--------}

pBrackDoc :: ParseM (Doc, Type)
pBrackDoc = do
  _ <- try $ char '['
  (d,_) <- pDoc
  _ <- char ']'
  whitespace
  return (d, DD)

pBrackDocE :: ParseM (Expr, Type)
pBrackDocE = do
  _ <- try $ char '['
  (d,_) <- pDoc
  _ <- char ']'
  whitespace
  return (DocE d, DD)

pREPL :: ParseM (Doc, Type)
pREPL = do
  x <- choice $ map pAtLocus
         [ pDefineREPL
         , pNakedExprREPL
         , pVarExpr NakedKey DD
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
      (v,_) <- if t == DD then pBrackDocE else pTypedExpr t
      return (Define t k v (Empty :@ NullLocus), DD)

    pNakedExprREPL = do
      (x,_) <- try pTypedNakedExpr
      return (NakedExpr x, DD)


pDoc :: ParseM (Doc, Type)
pDoc = choice $ map pAtLocus
  [ eof >> return (Empty, DD)
  , lookAhead (char ']') >> return (Empty, DD)
  , do
    xs <- many1 $ choice $ map pAtLocus
      [ try (char '#' >> many space) >> return (Empty, DD)
      , many1 (noneOf "#@[]") >>= \x -> return (DocText x, DD)
      , pVarExpr NakedKey DD
      , pEscaped
      , pEval
      , pDefine
      , pPull
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
      , pInput
      , pQuit
      , pNote
      ]
    return (Cat $ map fst xs, DD)
  ]
  where
    pEval = do
      try (char '[' >> keyword "eval")
      (e,_) <- pTypedMacExpr DD
      vals <- option [] pVals
      _ <- option () (try (keyword "endeval")) >> char ']'
      return (DocMacro vals e, DD)
        where
          pVals = do
            _ <- try $ keyword "("
            vs <- sepBy pTypeKeyExpr (keyword ";")
            keyword ")"
            return vs

    pEscaped = do
      x <- choice (zipWith pEsc "#@[]_.-~n" "#@[]_.- \n") <?> "escaped character"
      return (Escaped x, DD)
      where
        pEsc c d = try (string $ '#':[c]) >> (return d)

    pDefine = do
      try (char '[' >> keyword "define")
      t <- pType
      whitespace
      (k,_) <- pUntypedKey
      keyword ":="
      (v,_) <- if t == DD then pBrackDocE else pTypedExpr t
      _ <- option () (try (keyword "enddefine")) >> char ']'
      (rest,_) <- pDoc
      return (Define t k v rest, DD)

    pPull = do
      try (char '[' >> keyword "pull")
      (file,_) <- keyword "from" >> pParens pStrExpr
      name <- option Nothing (try (keyword "as") >> pParens (pJust pStrExpr))
      (rest,_) <- char ']' >> pDoc
      return (Pull file name rest, DD)
        where
          pJust p = do
            (x,_) <- p
            return (Just x)

    pScope = do
      try (char '[' >> keyword "scope")
      (x,_) <- pBrackDoc
      option () (try (keyword "endscope"))
      _ <- whitespace >> char ']'
      return (Scope x, DD)

    pNakedExpr = do
      try (char '[' >> keyword ":")
      (x,_) <- try pTypedNakedExpr <|> (pTypedExpr XX)
      _ <- keyword ":" >> char ']'
      return (NakedExpr x, DD)

    pShuffle = do
      try (char '[' >> keyword "shuffle")
      xs <- many pBrackDoc
      option () (try $ keyword "endshuffle")
      _ <- whitespace >> char ']'
      return (Shuffle $ map fst xs, DD)

    pIfThenElse = do
      try (char '[' >> keyword "if")
      (test,_) <- pBoolExpr
      (true,_) <- keyword "then" >> pBrackDoc
      (false,_) <- keyword "else" >> pBrackDoc
      option () (try (keyword "endif"))
      _ <- whitespace >> char ']'
      return (IfThenElse test true false, DD)

    pCond = do
      try (char '[' >> keyword "cond")
      cases <- many pCondCase
      (auto,_) <- option (Empty :@ NullLocus, DD) (try (keyword "default") >> pBrackDoc)
      option () (try (keyword "endcond"))
      _ <- whitespace >> char ']'
      return (Cond cases auto, DD)
      where
        pCondCase = do
          (s,_) <- try (keyword "case") >> pBoolExpr
          (t,_) <- pBrackDoc
          return (s,t)

    pFor = do
      try (char '[' >> keyword "for")
      typ <- pType
      spaces
      (k,_) <- pKey
      (r,_) <- keyword "in"  >> pTypedListExpr typ
      (t,_) <- keyword "say" >> pBrackDoc
      b <- option Nothing $ (try (keyword "sepby")) >> pBrackDoc >>= (return . Just . fst)
      option () (try (keyword "endfor"))
      _ <- whitespace >> char ']'
      return (ForSay k r t b, DD)

    pAlt = do
      try (char '[' >> keyword "alt")
      opts  <- many pAltOpt
      option () (try (keyword "endalt"))
      _ <- whitespace >> char ']'
      return (Alt opts, DD)
      where
        pAltOpt = try (keyword "opt") >> pBrackDoc >>= return . fst

    pBail = do
      try (char '[' >> keyword "bail")
      (message,_) <- pStrExpr
      option () (try (keyword "endbail"))
      _ <- whitespace >> char ']'
      return (Bail message, DD)

    pLetIn = do
      try (char '[' >> keyword "let")
      (_,k,e) <- pTypeKeyExpr
      (body,_) <- keyword "in" >> pBrackDoc
      option () (try (keyword "endlet"))
      _ <- whitespace >> char ']'
      return (LetIn k e body, DD)

    pSelect = do
      try (char '[' >> keyword "select")
      ty <- pType
      (k,_) <- spaced pKey
      (r,_) <- keyword "from" >> pTypedListExpr ty
      (t,_) <- keyword "in"   >> pBrackDoc
      option () (try (keyword "endselect"))
      _ <- whitespace >> char ']'
      return (Select k r t, DD)

    pInput = do
      try (char '[' >> keyword "input")
      (x,_) <- pStrExpr
      _ <- whitespace >> char ']'
      return (Input x, DD)

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
      return (Empty, DD)



{-

  where
    pShowState :: Parser Doc
    pShowState = do
      start <- getPosition
      try (keyword "state")
      end   <- getPosition
      return $ ShowState (locus start end)


    pSplice = do
      try (char '[' >> keyword "splice")
      (t,_) <- pParens pStrExpr
      d <- option 
             Nothing
             (choice 
               [ try (keyword "with") >> pParens pStrExpr >>= (return . Just . Left)
               , try (keyword "at")   >> pParens pStrExpr >>= (return . Just . Right)])
      f <- option
             Nothing
             (try (keyword "as") >> pDataFormat >>= (return . Just))
      end <- getPosition
      return $ Splice (locus start end) t d f
-}



{---------}
{- :Expr -}
{---------}

pTypedExpr :: Type -> ParseM (Expr, Type)
pTypedExpr DD = pDoc      >>= return . mapFst DocE
pTypedExpr SS = pStrExpr  >>= return . mapFst StrE
pTypedExpr ZZ = pIntExpr  >>= return . mapFst IntE
pTypedExpr BB = pBoolExpr >>= return . mapFst BoolE
pTypedExpr QQ = pRatExpr  >>= return . mapFst RatE

pTypedExpr (ZZMod n) = pZZModExpr n >>= return . mapFst ZZModE

pTypedExpr (ListOf   t) = pTypedListExpr t >>= return . mapFst ListE
pTypedExpr (MatOf    t) = pTypedMatExpr  t >>= return . mapFst MatE
pTypedExpr (MacTo    t) = pTypedMacExpr  t >>= return . mapFst MacE
pTypedExpr (PermOf   t) = pTypedPermExpr t >>= return . mapFst PermE
pTypedExpr (PolyOver t) = pTypedPolyExpr t >>= return . mapFst PolyE

pTypedExpr XX = choice
  [ try pStrExpr  >>= return . mapFst StrE
  , try pIntExpr  >>= return . mapFst IntE
  , try pBoolExpr >>= return . mapFst BoolE
  , try pRatExpr  >>= return . mapFst RatE
  , try pListExpr >>= return . mapFst ListE
  , try pMatExpr  >>= return . mapFst MatE
  , try pMacExpr  >>= return . mapFst MacE
  , try pPolyExpr >>= return . mapFst PolyE
  ] <?> "expression"


pTypeKeyExpr :: ParseM (Type, Key, Expr)
pTypeKeyExpr = do
  start <- getPosition
  t <- pType
  whitespace
  (k,_) <- pUntypedKey
  keyword ":="
  (v,u) <- pTypedExpr t
  end <- getPosition
  case unify t u of
    Left err -> reportParseErr (locus start end) err
    Right y -> return (y, k, v)

pTypedNakedExpr :: ParseM (Expr, Type)
pTypedNakedExpr = do
  start <- getPosition
  t <- pType
  keyword ":"
  (v,u) <- pTypedExpr t
  end <- getPosition
  case unify t u of
    Left err -> reportParseErr (locus start end) err
    Right w -> return (v, w)

pTypedConst :: Type -> ParseM (Expr, Type)
pTypedConst SS          = pStrConst     >>= return . mapFst StrE
pTypedConst ZZ          = pIntConst     >>= return . mapFst IntE
pTypedConst BB          = pBoolConst    >>= return . mapFst BoolE
pTypedConst QQ          = pRatConst     >>= return . mapFst RatE
pTypedConst (ZZMod n)   = pZZModConst n >>= return . mapFst ZZModE

pTypedConst (ListOf   t) = pListConst t >>= return . mapFst ListE
pTypedConst (MatOf    t) = pMatConst  t >>= return . mapFst MatE
pTypedConst (PolyOver t) = pPolyConst t >>= return . mapFst PolyE
pTypedConst (PermOf   t) = pPermConst t >>= return . mapFst PermE
pTypedConst (MacTo    t) = pMacConst  t >>= return . mapFst MacE

pTypedConst _ = error "pTypedConst"



{------------}
{- :StrExpr -}
{------------}

pStrConst :: ParseM (StrExpr, Type)
pStrConst = pAtLocus pStrConst'

pStrConst' :: ParseM (StrExprLeaf, Type)
pStrConst' = pConst pString StrConst SS

pStrExpr :: ParseM (StrExpr, Type)
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
          (e,_) <- pTypedExpr t
          keyword ")"
          return (StrFormat f e, SS)

    strOpTable =
      [ [Infix (opParser2 Concat "++") AssocLeft
        ]
      ]



{------------}
{- :IntExpr -}
{------------}

pIntConst :: ParseM (IntExpr, Type)
pIntConst = pAtLocus pIntConst'

pIntConst' :: ParseM (IntExprLeaf, Type)
pIntConst' = pConst pInteger IntConst ZZ

pIntExpr :: ParseM (IntExpr, Type)
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

      , pFun1 "Length" pListExpr           ListLen  ZZ
      , pFun1 "Rand"   (pTypedListExpr ZZ) IntRand  ZZ
      , pFun1 "Sum"    (pTypedListExpr ZZ) IntSum   ZZ
      , pFun1 "Prod"   (pTypedListExpr ZZ) IntProd  ZZ
      , pFun1 "Min"    (pTypedListExpr ZZ) IntMinim ZZ
      , pFun1 "Max"    (pTypedListExpr ZZ) IntMaxim ZZ
      , pFun1 "GCD"    (pTypedListExpr ZZ) IntGCDiv ZZ
      , pFun1 "LCM"    (pTypedListExpr ZZ) IntLCMul ZZ
    
      , pFun1 "Numerator"   pRatExpr  RatNumer ZZ
      , pFun1 "Denominator" pRatExpr  RatDenom ZZ
      , pFun1 "Floor"       pRatExpr  RatFloor ZZ
    
      , pFun1 "StrLen" pStrExpr  StrLength ZZ

      , pFun1 "NumRows" pMatExpr MatNumRows ZZ
      , pFun1 "NumCols" pMatExpr MatNumCols ZZ
    
      , pFun2 "Uniform"  pIntExpr pIntExpr IntObserveUniform ZZ
      , pFun2 "Binomial" pIntExpr pRatExpr IntObserveBinomial ZZ
      , pFun1 "Poisson"  pRatExpr IntObservePoisson ZZ
      ]

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

pZZModConst :: Integer -> ParseM (ZZModExpr, Type)
pZZModConst n = pAtLocus (pZZModConst' n)

pZZModConst' :: Integer -> ParseM (ZZModExprLeaf, Type)
pZZModConst' n = do
  a <- pInteger
  return (ZZModConst (a `zzmod` n), ZZMod n)

pZZModExpr :: Integer -> ParseM (ZZModExpr, Type)
pZZModExpr n = spaced $ buildExpressionParser zzModOpTable pZZModTerm
  where
    pZZModTerm = pTerm (pZZModConst' n) (pZZModExpr n) "integer expression"
      [ pVarExpr ZZModVar (ZZMod n)
      , pMacroExpr ZZModMacro

      , pAtPos (ZZMod n) ZZModAtPos
      , pAtIdx (ZZMod n) ZZModAtIdx
    
      , pIfThenElseExpr (pZZModExpr n) ZZModIfThenElse (ZZMod n)

      , pFun1 "int" pIntExpr (ZZModCast n) (ZZMod n)

      , pFun2 "Pow" (pZZModExpr n) pIntExpr ZZModPow (ZZMod n)

      , pFun1 "Sum"    (pTypedListExpr (ZZMod n)) ZZModSum   (ZZMod n)
      , pFun1 "Prod"   (pTypedListExpr (ZZMod n)) ZZModProd  (ZZMod n)
      ]

    zzModOpTable =
      [ [ Infix (opParser2 ZZModMult "*") AssocLeft
        ]
      , [ Prefix (opParser1 ZZModNeg "neg")
        , Prefix (opParser1 ZZModInv "inv")
        ]
      , [ Infix (opParser2 ZZModAdd "+") AssocLeft
        , Infix (opParser2 ZZModSub "-") AssocLeft
        ]
      ]



{-------------}
{- :BoolExpr -}
{-------------}

pBoolConst :: ParseM (BoolExpr, Type)
pBoolConst = pAtLocus pBoolConst'

pBoolConst' :: ParseM (BoolExprLeaf, Type)
pBoolConst' = pConst pBool BoolConst BB

pBoolExpr :: ParseM (BoolExpr, Type)
pBoolExpr = spaced $ buildExpressionParser boolOpTable pBoolTerm
  where
    pBoolTerm = pTerm pBoolConst' pBoolExpr "boolean expression"
      [ pVarExpr BoolVar BB
      , pMacroExpr BoolMacro

      , pAtPos BB BoolAtPos
      , pAtIdx BB BoolAtIdx

      , pIfThenElseExpr pBoolExpr BoolIfThenElse BB
    
      , pFun1 "IsDefined" pKey IsDefined BB

      , pFun1 "IsSquareFree" pIntExpr IntSqFree BB

      , pFun1 "Rand" (pTypedListExpr BB) BoolRand BB

      , pTypedArgPair "Elem" pTypedExpr pTypedListExpr ListElem (const BB)
      , pTypedArg "IsEmpty" pTypedListExpr ListIsEmpty (const BB)

      , pFun1 "IsRow" pMatExpr MatIsRow BB
      , pFun1 "IsCol" pMatExpr MatIsCol BB
      , pTypedArg "IsGJForm" pTypedMatExpr MatIsGJForm (const BB)

      , pTypedArgPair "Equal"    pTypedExpr pTypedExpr BoolEq  (const BB)
      , pTypedArgPair "NotEqual" pTypedExpr pTypedExpr BoolNEq (const BB)
      , pTypedArgPair "LT"       pTypedExpr pTypedExpr BoolLT  (const BB)
      , pTypedArgPair "LEq"      pTypedExpr pTypedExpr BoolLEq (const BB)
      , pTypedArgPair "GT"       pTypedExpr pTypedExpr BoolGT  (const BB)
      , pTypedArgPair "GEq"      pTypedExpr pTypedExpr BoolGEq (const BB)

      , pFun2 "Matches" pStrExpr (pPair SS pString) Matches BB
      , pFun2 "Divides" pIntExpr pIntExpr IntDiv BB
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

pRatConst :: ParseM (RatExpr, Type)
pRatConst = pAtLocus pRatConst'

pRatConst' :: ParseM (RatExprLeaf, Type)
pRatConst' = pConst pRat RatConst QQ

pRatExpr :: ParseM (RatExpr, Type)
pRatExpr = spaced $ buildExpressionParser ratOpTable pRatTerm
  where
    pRatTerm = pTerm pRatConst' pRatExpr "rational expression"
      [ pVarExpr RatVar QQ
      , pMacroExpr RatMacro

      , pAtPos QQ RatAtPos
      , pAtIdx QQ RatAtIdx

      , pIfThenElseExpr pRatExpr RatIfThenElse QQ

      , pFun1 "Rand"   (pTypedListExpr QQ) RatRand  QQ
      , pFun1 "Sum"    (pTypedListExpr QQ) RatSum   QQ
      , pFun1 "Prod"   (pTypedListExpr QQ) RatProd  QQ
      , pFun1 "Min"    (pTypedListExpr QQ) RatMinim QQ
      , pFun1 "Max"    (pTypedListExpr QQ) RatMaxim QQ

      , pFun1 "int" pIntExpr RatCast QQ
      , pFun2 "Pow" pRatExpr pIntExpr RatPow QQ
      , pMean
      , pFun2 "Sqrt" pRatExpr pIntExpr RatSqrt QQ
      , pMeanDev
      , pStdDev
      , pZScore
      ]
      where
        pMean = do
          try $ keyword "Mean"
          keyword "("
          (t,_) <- pTypeT
          keyword ";"
          (ks,_) <- pTypedListExpr t
          keyword ")"
          return (RatMean ks, QQ)

        pMeanDev = do
          try $ keyword "MeanDev"
          keyword "("
          (t,_) <- pTypeT
          keyword ";"
          (ks,_) <- pTypedListExpr t
          keyword ")"
          return (RatMeanDev ks, QQ)

        pStdDev = do
          try $ keyword "StdDev"
          keyword "("
          (t,_) <- pTypeT
          keyword ";"
          (ks,_) <- pTypedListExpr t
          keyword ";"
          (n,_) <- pIntExpr
          keyword ")"
          return (RatStdDev ks n, QQ)

        pZScore = do
          try $ keyword "ZScore"
          keyword "("
          (t,_) <- pTypeT
          keyword ";"
          (p,_) <- pRatExpr
          keyword ";"
          (ks,_) <- pTypedListExpr t
          keyword ";"
          (n,_) <- pIntExpr
          keyword ")"
          return (RatZScore p ks n, QQ)
    
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

pListLiteral :: Type -> ParseM (ListExpr, Type)
pListLiteral typ = pAtLocus $ pListLiteralOf typ pTypedExpr

pListConst :: Type -> ParseM (ListExpr, Type)
pListConst typ = pAtLocus $ pListLiteralOf typ pTypedConst

pListLiteralOf :: Type -> (Type -> ParseM (Expr, Type)) -> ParseM (ListExprLeaf, Type)
pListLiteralOf typ p = do
    start <- getPosition
    xs <- pBraceList (p typ)
    end <- getPosition
    case map snd xs of
      [] -> return (ListConst typ [], ListOf typ)
      ts -> case unifyAll ts of
        Right _ -> return (ListConst typ (map fst xs), ListOf typ)
        Left err -> reportParseErr (locus start end) err

pListExpr :: ParseM (ListExpr, Type)
pListExpr = pTypedListExpr XX

pTypedListExpr :: Type -> ParseM (ListExpr, Type)
pTypedListExpr typ = spaced $ buildExpressionParser listOpTable pListTerm
  where
    pListTerm = pTerm (pListLiteralOf typ pTypedExpr) (pTypedListExpr typ) "list expression"
      [ pVarExpr ListVar (ListOf typ)

      , pMacroExpr ListMacro

      , pAtPos (ListOf typ) ListAtPos
      , pAtIdx (ListOf typ) ListAtIdx

      , pIfThenElseExpr (pTypedListExpr typ) ListIfThenElse (ListOf typ)

      , pFun1 "Rand" (pTypedListExpr (ListOf typ)) ListRand (ListOf typ)

      , pFun1 "Reverse"  (pTypedListExpr typ) ListRev      (ListOf typ)
      , pFun1 "Sort"     (pTypedListExpr typ) ListSort     (ListOf typ)
      , pFun1 "Unique"   (pTypedListExpr typ) ListUniq     (ListOf typ)
      , pFun1 "Shuffle"  (pTypedListExpr typ) ListShuffle  (ListOf typ)
      , pListShuffles

      , pFun2 "GetRow" pIntExpr (pTypedMatExpr typ) ListMatRow (ListOf typ)
      , pFun2 "GetCol" pIntExpr (pTypedMatExpr typ) ListMatCol (ListOf typ)

      , pListPermsOf

      , pListRange
      , pListBuilder
      , pFun2 "Choose" pIntExpr (pTypedListExpr typ) ListChoose (ListOf typ)
      , pListChoices
      , pListFilter
      ]
      where
        pListFilter = do
          _ <- try $ keyword "Filter"
          keyword "("
          (k,_) <- pKey
          keyword ";"
          (g,_) <- pBoolExpr
          keyword ";"
          (xs,t) <- pTypedListExpr typ
          keyword ")"
          return (ListFilter k g xs, t)

        pListChoices = do
          _ <- try $ keyword "Choices"
          case typ of
            ListOf t -> do
              keyword "("
              (n,_) <- pIntExpr
              keyword ";"
              (xs,_) <- pTypedListExpr t
              keyword ")"
              return (ListChoices n xs, ListOf (ListOf t))
            _ -> error "pListChoices"

        pListShuffles = do
          _ <- try $ keyword "Shuffles"
          case typ of
            ListOf t -> do
              keyword "("
              (xs,_) <- pTypedListExpr t
              keyword ")"
              return (ListShuffles xs, ListOf (ListOf t))
            _ -> error "pListShuffles"

        pListPermsOf = do
          _ <- try $ keyword "PermutationsOf"
          case typ of
            PermOf t -> do
              keyword "("
              (xs,_) <- pTypedListExpr t
              keyword ")"
              return (ListPermsOf typ xs, ListOf typ)
            _ -> error "pListPermsOf"

        pListRange = case unify typ ZZ of
          Right _ -> do
            try $ keyword "Range"
            (a,b) <- pTuple2 pIntExpr pIntExpr
            return (ListRange a b, ListOf ZZ)
          Left _ -> fail "pListRange"
    
        pListBuilder = do
          start <- getPosition
          try $ keyword "Build"
          keyword "("
          (expr,t) <- pTypedExpr typ
          keyword ";"
          gds <- sepBy1 (pListBind <|> pListGuard) (keyword ";")
          keyword ")"
          end <- getPosition
          case unify typ t of
            Left _ -> reportParseErr (locus start end) $ TypeUnificationError typ t
            Right u -> return (ListBuilder expr gds, ListOf u)
            where
              pListBind = do
                w <- try pType
                whitespace
                (k,_) <- pKey
                keyword "<-"
                (ls,_) <- pTypedListExpr w
                return $ Bind k ls
          
              pListGuard = do
                (e,_) <- try pBoolExpr
                return $ Guard e
    
    listOpTable =
      [ [ Infix (opParser2 ListCat "++") AssocLeft
        ]
      , [ Infix (opParser2 ListToss "\\\\") AssocLeft
        ]
      ]



{------------}
{- :MatExpr -}
{------------}

pMatLiteral :: Type -> ParseM (MatExpr, Type)
pMatLiteral typ = pAtLocus $ pMatLiteralOf typ pTypedExpr

pMatConst :: Type -> ParseM (MatExpr, Type)
pMatConst typ = pAtLocus $ pMatLiteralOf typ pTypedConst

pMatLiteralOf :: Type -> (Type -> ParseM (Expr, Type)) -> ParseM (MatExprLeaf, Type)
pMatLiteralOf typ p = do
  start <- getPosition
  xss <- pBrackList (pBrackList (p typ))
  end <- getPosition
  case concatMap (map snd) xss of
    [] -> case mFromRowList [] of
      Left err -> reportParseErr (locus start end) err
      Right m -> return (MatConst typ m, MatOf typ)
    ts -> case unifyAll ts of
      Left err -> reportParseErr (locus start end) err
      Right _ -> case mFromRowList (map (map fst) xss) of
        Left err -> fail $ show err
        Right m -> return (MatConst typ m, MatOf typ)

pMatExpr :: ParseM (MatExpr, Type)
pMatExpr = pTypedMatExpr XX

pTypedMatExpr :: Type -> ParseM (MatExpr, Type)
pTypedMatExpr typ = spaced $ buildExpressionParser matOpTable pMatTerm
  where
    pMatTerm = pTerm (pMatLiteralOf typ pTypedExpr) (pTypedMatExpr typ) "matrix expression"
      [ pVarExpr MatVar (MatOf typ)

      , pAtPos (MatOf typ) MatAtPos
      , pAtIdx (MatOf typ) MatAtIdx

      , pMacroExpr MatMacro

      , pIfThenElseExpr (pTypedMatExpr typ) MatIfThenElse (MatOf typ)

      , pFun1 "Transpose" (pTypedMatExpr typ) MatTrans (MatOf typ)

      , pFun1 "ShuffleRows" (pTypedMatExpr typ) MatShuffleRows (MatOf typ)
      , pFun1 "ShuffleCols" (pTypedMatExpr typ) MatShuffleCols (MatOf typ)

      , pFun1 "RowFromList" (pTypedListExpr typ) (MatRowFromList typ) (MatOf typ)
      , pFun1 "ColFromList" (pTypedListExpr typ) (MatColFromList typ) (MatOf typ)

      , pFun1 "Rand" (pTypedListExpr (MatOf typ)) MatRand (MatOf typ)

      , pFun2 "GetRow" pIntExpr (pTypedMatExpr typ) MatGetRow (MatOf typ)
      , pFun2 "GetCol" pIntExpr (pTypedMatExpr typ) MatGetCol (MatOf typ)

      , pMatBuilder

      , pMatId
      , pMatSwapE
      , pMatScaleE
      , pMatAddE

      , pFun2T "Pow" (pTypedMatExpr typ) pIntExpr MatPow

      , pFun3T "SwapRows" (pTypedMatExpr typ) pIntExpr pIntExpr MatSwapRows
      , pFun3T "SwapCols" (pTypedMatExpr typ) pIntExpr pIntExpr MatSwapCols
      , pFun3T "ScaleRow" (pTypedMatExpr typ) (pTypedExpr typ) pIntExpr MatScaleRow
      , pFun3T "ScaleCol" (pTypedMatExpr typ) (pTypedExpr typ) pIntExpr MatScaleCol
      , pFun4T "AddRow"   (pTypedMatExpr typ) (pTypedExpr typ) pIntExpr pIntExpr MatAddRow
      , pFun4T "AddCol"   (pTypedMatExpr typ) (pTypedExpr typ) pIntExpr pIntExpr MatAddCol
      , pFun2T "DelRow"   (pTypedMatExpr typ) pIntExpr MatDelRow
      , pFun2T "DelCol"   (pTypedMatExpr typ) pIntExpr MatDelCol

      , pFun1T "GJForm"   (pTypedMatExpr typ) MatGJForm
      , pFun1T "GJFactor" (pTypedMatExpr typ) MatGJFactor
      ]
      where
        pMatId = do
          try $ keyword "Id"
          (t,n) <- pTuple2 pTypeT pIntExpr
          return (MatId t n, MatOf t)

        pMatSwapE = do
          try $ keyword "SwapE"
          (t,n,h,k) <- pTuple4 pTypeT pIntExpr pIntExpr pIntExpr
          return (MatSwapE t n h k, MatOf t)

        pMatScaleE = do
          try $ keyword "ScaleE"
          keyword "("
          (t,_) <- pTypeT
          keyword ";"
          (n,_) <- pIntExpr
          keyword ";"
          (k,_) <- pIntExpr
          keyword ";"
          (e,_) <- pTypedExpr t
          keyword ")"
          return (MatScaleE t n k e, MatOf t)

        pMatAddE = do
          try $ keyword "AddE"
          keyword "("
          (t,_) <- pTypeT
          keyword ";"
          (n,_) <- pIntExpr
          keyword ";"
          (i,_) <- pIntExpr
          keyword ";"
          (j,_) <- pIntExpr
          keyword ";"
          (e,_) <- pTypedExpr t
          keyword ")"
          return (MatAddE t n i j e, MatOf t)

        pMatBuilder = do
          try $ keyword "Build"
          keyword "("
          (e,_) <- pTypedExpr typ
          keyword ";"
          tr <- pType
          whitespace
          (kr,_) <- pKey
          keyword "<-"
          (lr,_) <- pTypedListExpr tr
          keyword ";"
          tc <- pType
          whitespace
          (kc,_) <- pKey
          keyword "<-"
          (lc,_) <- pTypedListExpr tc
          keyword ")"
          return (MatBuilder typ e kr lr kc lc, MatOf typ)
    
    matOpTable =
      [ [ Prefix (opParser1 MatNeg "neg")
        ]
      , [ Infix (opParser2 MatMul "*") AssocLeft
        ]
      , [ Infix (opParser2 MatAdd "+") AssocLeft
        ]
      , [ Infix (opParser2 MatHCat "hcat") AssocLeft
        , Infix (opParser2 MatVCat "vcat") AssocLeft
        ]
      ]



{-------------}
{- :PolyExpr -}
{-------------}

pPolyLiteral :: Type -> ParseM (PolyExpr, Type)
pPolyLiteral typ = pAtLocus $ pPolyLiteralOf typ pTypedExpr

pPolyConst :: Type -> ParseM (PolyExpr, Type)
pPolyConst typ = pAtLocus $ pPolyLiteralOf typ pTypedConst

pPolyLiteralOf :: Type -> (Type -> ParseM (Expr, Type)) -> ParseM (PolyExprLeaf, Type)
pPolyLiteralOf typ p = do
  try $ keyword "Poly"
  keyword "("
  ts <- sepBy1 (pPolyTerm $ p typ) (try $ char ';')
  keyword ")"
  return (PolyConst typ (fromListP $ map foo ts), PolyOver typ)
  where
    foo ((a,_),c) = (a,c)

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

pPolyExpr :: ParseM (PolyExpr, Type)
pPolyExpr = pTypedPolyExpr XX

pTypedPolyExpr :: Type -> ParseM (PolyExpr, Type)
pTypedPolyExpr typ = spaced $ buildExpressionParser polyOpTable pPolyTerm
  where
    pPolyTerm = pTerm (pPolyLiteralOf typ pTypedExpr) (pTypedPolyExpr typ) "polynomial expression"
      [ pVarExpr PolyVar (PolyOver typ)

      , pAtPos (PolyOver typ) PolyAtPos
      , pAtIdx (PolyOver typ) PolyAtIdx

      , pMacroExpr PolyMacro

      , pIfThenElseExpr (pTypedPolyExpr typ) PolyIfThenElse (PolyOver typ)

      , pFun1 "Rand" (pTypedListExpr (PolyOver typ)) PolyRand (PolyOver typ)

      , pPolyPow

      , pPolyNull

      , pFun2 "FromRoots" (pLiftAt pVar XX) (pTypedListExpr typ) PolyFromRoots (PolyOver typ)

      , pPolyEvalPoly
      ]
      where
        pPolyEvalPoly = do
          try $ keyword "EvalPoly"
          keyword "("
          (p,_) <- pTypedPolyExpr typ
          keyword ";"
          xs <- sepBy1 pSubs (keyword ";")
          keyword ")"
          return (PolyEvalPoly p xs, PolyOver typ)
            where
              pSubs = do
                x <- try $ pVar
                keyword "<-"
                (q,_) <- pTypedPolyExpr typ
                return (x,q)

        pPolyPow = pFun2T "Pow" (pTypedPolyExpr typ) pIntExpr PolyPow

        pPolyNull = do
          try $ keyword "Null"
          return (PolyConst typ nullP, PolyOver typ)

    polyOpTable =
      [ [ Prefix (opParser1 PolyNeg "neg")
        ]
      , [ Infix (opParser2 PolyMul "*") AssocLeft
        ]
      , [ Infix (opParser2 PolyAdd "+") AssocLeft
        , Infix (opParser2 PolySub "-") AssocLeft
        ]
      ]



{-------------}
{- :PermExpr -}
{-------------}

pPermLiteral :: Type -> ParseM (PermExpr, Type)
pPermLiteral typ = pAtLocus $ pPermLiteralOf typ pTypedConst

pPermConst :: Type -> ParseM (PermExpr, Type)
pPermConst typ = pAtLocus $ pPermLiteralOf typ pTypedConst

pPermLiteralOf :: Type -> (Type -> ParseM (Expr, Type)) -> ParseM (PermExprLeaf, Type)
pPermLiteralOf typ p = do
  start <- getPosition
  ts <- many1 pCycle
  end <- getPosition
  case fromCycles ts of
    Right q -> return (PermConst typ q, PermOf typ)
    Left err -> reportParseErr (locus start end) err
    where
      pCycle = do
        _ <- try $ char '('
        xs <- sepBy1 (p typ) whitespace
        _ <- char ')'
        return $ map fst xs

pTypedPermExpr :: Type -> ParseM (PermExpr, Type)
pTypedPermExpr typ = spaced $ buildExpressionParser permOpTable pPermTerm
  where
    pPermTerm = pTerm (pPermLiteralOf typ pTypedExpr) (pTypedPermExpr typ) "permutation expression"
      [ pVarExpr PermVar (PermOf typ)

      , pAtPos (PermOf typ) PermAtPos
      , pAtIdx (PermOf typ) PermAtIdx

      , pMacroExpr PermMacro

      , pIfThenElseExpr (pTypedPermExpr typ) PermIfThenElse (PermOf typ)

      , pFun1 "Rand" (pTypedListExpr (PermOf typ)) PermRand (PermOf typ)
      ]

    permOpTable =
      [ [ Prefix (opParser1 PermInvert "inv")
        ]
      , [ Infix (opParser2 PermCompose "o") AssocLeft
        ]
      ]



{------------}
{- :MacExpr -}
{------------}

pMacConst :: Type -> ParseM (MacExpr, Type)
pMacConst typ = pAtLocus $ pMacConst' typ

pMacConst' :: Type -> ParseM (MacExprLeaf, Type)
pMacConst' typ = do
  start <- getPosition
  try $ keyword "Macro"
  keyword "("
  t <- pType
  keyword ";"
  (body,_) <- if t == DD then pBrackDocE else pTypedExpr t
  vals <- option [] $ many (keyword ";" >> pTypeKeyExpr)
  keyword ")"
  end <- getPosition
  case unify typ t of
    Left _ -> reportParseErr (locus start end) $ TypeUnificationError typ t
    Right u -> return (MacConst u vals body (emptyStore, False), MacTo u)


pMacExpr :: ParseM (MacExpr, Type)
pMacExpr = pTypedMacExpr XX


pTypedMacExpr :: Type -> ParseM (MacExpr, Type)
pTypedMacExpr typ = spaced $ buildExpressionParser macOpTable pMacTerm
  where
    pMacTerm = pTerm (pMacConst' typ) pMacExpr "macro expression"
      [ pVarExpr MacVar (MacTo typ)

      , pAtPos (MacTo typ) MacAtPos
      , pAtIdx (MacTo typ) MacAtIdx

      , pMacroExpr MacMacro

      , pFun1 "Rand" (pTypedListExpr (MacTo typ)) MacRand (MacTo typ)

      , pIfThenElseExpr (pTypedMacExpr typ) MacIfThenElse (MacTo typ)
      ]

    macOpTable = []
