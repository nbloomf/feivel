{---------------------------------------------------------------------}
{- Copyright 2015, 2016 Nathan Bloomfield                            -}
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

{-# OPTIONS_GHC -XTypeSynonymInstances #-}

module Feivel.Error (
  Err(..), PromoteError, promote, RandErr(..), SysErr(..),
  Goof(..), reportErr, parseGoof
) where


import Feivel.Store (StateErr(), Locus(..), locus, shortReport)
import Carl.AlgErr (AlgErr())
import Carl.String (StrErr())
import Carl.List (ListErr())
import Carl.Bool (BoolErr())
import Feivel.Grammar (ExprErr(), GetErr(), TypeErr())

import Text.Parsec.Error (ParseError(), errorPos)
import Control.Monad.Trans.Error
import System.Exit (ExitCode())

{------------}
{- Contents -}
{-   :Err   -}
{-   :Goof  -}
{------------}

{--------}
{- :Err -}
{--------}

data Err
  = Success
  | ErrState StateErr
  | ErrStr   StrErr
  | ErrList  ListErr
  | ErrBool  BoolErr
  | ErrFile  IOError
  | ErrExpr  ExprErr
  | ErrParse ParseError
  | ErrType  TypeErr
  | ErrRand  RandErr
  | ErrAlg   AlgErr
  | ErrGet   GetErr
  | ErrSys   SysErr

instance Show Err where
  show Success      = "Success!"

  -- Data Errors
  show (ErrStr   x) = show x
  show (ErrList  x) = show x
  show (ErrBool  x) = show x
  show (ErrAlg   x) = show x

  -- IO Errors
  show (ErrFile  x) = show x
  show (ErrParse x) = show x
  show (ErrState x) = show x
  show (ErrSys   x) = show x

  -- Misc Errors
  show (ErrExpr  x) = show x
  show (ErrType  x) = show x
  show (ErrRand  x) = show x
  show (ErrGet   x) = show x



class PromoteError t where
  promote :: t -> Err

instance PromoteError StateErr   where promote = ErrState
instance PromoteError StrErr     where promote = ErrStr
instance PromoteError ListErr    where promote = ErrList
instance PromoteError IOError    where promote = ErrFile
instance PromoteError BoolErr    where promote = ErrBool
instance PromoteError AlgErr     where promote = ErrAlg
instance PromoteError ExprErr    where promote = ErrExpr
instance PromoteError ParseError where promote = ErrParse
instance PromoteError TypeErr    where promote = ErrType
instance PromoteError RandErr    where promote = ErrRand
instance PromoteError GetErr     where promote = ErrGet
instance PromoteError SysErr     where promote = ErrSys


data SysErr
  = ExitFail ExitCode String String

instance Show SysErr where
  show (ExitFail code stdout stderr) = 
    concat [show code, stdout, stderr]


{------------}
{- :RandErr -}
{------------}

data RandErr
  = EmptySampleSpace
  | InvalidProbability Double
  | NonpositiveParameter Double
  deriving Eq

instance Show RandErr where
  show EmptySampleSpace =
    "The sample space of this random variable is empty. (Perhaps a parameter is invalid?)"

  show (InvalidProbability p) =
    "The number " ++ show p ++ " is not a valid probability."

  show (NonpositiveParameter x) =
    "The parameter whose value is " ++ show x ++ " should be positive."



{---------}
{- :Goof -}
{---------}

data Goof = Goof Locus Err

instance Error Goof where
  noMsg = Goof NullLocus Success

instance Show Goof where
  show (Goof loc err) = "Error at " ++ shortReport loc ++ "\n  " ++ show err

reportErr :: (Monad m, PromoteError t) => Locus -> t -> ErrorT Goof m a
reportErr loc err = throwError $ Goof loc $ promote err

parseGoof :: ParseError -> Goof
parseGoof err = Goof (locus (errorPos err) (errorPos err)) (promote err)
