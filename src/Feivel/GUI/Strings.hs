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

module Feivel.GUI.Strings where

ui_name_string :: String
ui_name_string =
  "Feivel"

ui_version_string :: String
ui_version_string =
  "0.2.2"

ui_license_string :: String
ui_license_string =
  "GNU GPLv3"

ui_about_comment :: String
ui_about_comment =
  "A simple templating language and interactive calculator.\n\n"
    ++ "Copyright 2014--2016 Nathan Bloomfield"

ui_authors :: [String]
ui_authors =
  [ "Nathan Bloomfield <nbloomf@gmail.com>"
  ]