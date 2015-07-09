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

module Feivel.Format (
  DataFormat(..)
) where

data DataFormat
  = TypeKeyVal               -- :type:key:val;

  | CSV                      -- CSV with header (RFC 4180)
      { csvHead  :: Bool
      , csvDelim :: Char
      }

  | RFC822                            -- key: val\n
  | JSON                              -- 
  | AWK {recordSeparator :: String}   -- @$1, @$2, ...
  deriving (Eq, Show)
