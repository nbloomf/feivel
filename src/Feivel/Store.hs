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

module Feivel.Store (
  -- Errors
  StateErr(),

  -- Construct
  Store(), emptyStore, fromKeyValList, fromKeyValLocList,

  -- Query
  isDefinedKey, getTemplatePath, getDataPath, getDataFormat, valueOf,

  -- Mutate
  addKey, removeKey, setPaths, clearKeys, mergeState, qualify,

  -- Stack
  getStack, push, pop
) where

{---------------}
{- Contents    -}
{-  :Store     -}
{-  :StateErr  -}
{-  :Construct -}
{-  :Query     -}
{-  :Mutate    -}
{-  :Stack     -}
{---------------}

import qualified Data.Map as M

import Feivel.Key
import Feivel.Locus
import Feivel.Format

import Data.Maybe (isJust)
import Control.Monad (foldM)



{----------}
{- :Store -}
{----------}

data Store a = Store
 { keyValues       :: M.Map Key (a, Locus)
 , templatePath    :: FilePath
 , dataPath        :: FilePath
 , dataFormat      :: DataFormat
 , stack           :: [(String, Locus)]
 } deriving (Eq, Show)



{-------------}
{- :StateErr -}
{-------------}

data StateErr
 = KeyNotDefined     Key
 | KeyAlreadyDefined Key
 deriving (Eq)

instance Show StateErr where
  show (KeyNotDefined k) = 
    "Key " ++ show k ++ " not defined."

  show (KeyAlreadyDefined k) = 
    "Key " ++ show k ++ " is already defined."


{--------------}
{- :Construct -}
{--------------}

emptyStore :: Store a
emptyStore = Store
 { keyValues       = M.empty
 , templatePath    = ""
 , dataPath        = ""
 , dataFormat      = TypeKeyVal
 , stack           = []
 }

addKey :: Key -> a -> Locus -> Store a -> Either StateErr (Store a)
addKey key a loc st
 | isDefinedKey st key = Left $ KeyAlreadyDefined key
 | otherwise = Right $ st { keyValues = M.insert key (a, loc) (keyValues st) }

(<#<) :: Store a -> (Key, a, Locus) -> Either StateErr (Store a)
state <#< (key, a, loc) = addKey key a loc state

fromKeyValLocList :: [(Key, a, Locus)] -> Either StateErr (Store a)
fromKeyValLocList = foldM (<#<) emptyStore

fromKeyValList :: [(Key, a)] -> Either StateErr (Store a)
fromKeyValList = fromKeyValLocList . map (\(k, a) -> (k, a, NullLocus))



{----------}
{- :Query -}
{----------}

isDefinedKey :: Store a -> Key -> Bool
isDefinedKey st k = isJust $ M.lookup k (keyValues st)

valueOf :: Store a -> Key -> Either StateErr a
valueOf st k = case M.lookup k (keyValues st) of
  Nothing     -> Left $ KeyNotDefined k
  Just (a, _) -> Right a

getTemplatePath, getDataPath :: Store a -> FilePath
getTemplatePath = templatePath
getDataPath     = dataPath

getDataFormat :: Store a -> DataFormat
getDataFormat   = dataFormat



{-----------}
{- :Mutate -}
{-----------}

removeKey :: Key -> Store a -> Store a
removeKey key st = st
 { keyValues = M.delete key (keyValues st) }

clearKeys :: Store a -> Store a
clearKeys st = st
 { keyValues = M.empty }

qualify :: String -> Store a -> Store a
qualify foo st = st
 { keyValues = M.mapKeys (qualifyKey foo) (keyValues st) }

mergeState :: Store a -> Store a -> Store a
mergeState a b = a
 { keyValues = M.union (keyValues a) (keyValues b) }

setPaths :: FilePath -> FilePath -> DataFormat -> Store a -> Store a
setPaths tmp dat for st = st
 {templatePath = tmp, dataPath = dat, dataFormat = for}



{----------}
{- :Stack -}
{----------}

getStack :: Store a -> [(String, Locus)]
getStack = stack

push :: String -> Locus -> Store a -> Store a
push str loc state = state { stack = (str, loc):(stack state) }

pop :: Store a -> Store a
pop state = case stack state of
  []     -> state
  (_:st) -> state { stack = st }
