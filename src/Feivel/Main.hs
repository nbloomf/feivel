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

module Feivel.Main where

import Feivel.Eval
import Feivel.Store (setPaths)
import Feivel.EvalM
import Feivel.Format
import Feivel.Store (emptyStore)
import Feivel.Error

import System.IO
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getArgs)
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(ReqArg, NoArg), usageInfo)


main :: IO ()
main = do
  args               <- getArgs
  (actions, _, errs) <- return $ getOpt Permute options args
  opts               <- foldl (>>=) (return defaultArgs) actions

  {---------------------------------------------------}
  {- If any command line errors, show usage and bail -}
  {---------------------------------------------------}
  _ <- if errs == []
        then return ()
        else showErrors errs >> showUsage >> exitWith (ExitFailure 1)


  {-----------------------------------------}
  {- If --help is set, show usage and quit -}
  {-----------------------------------------}
  _ <- if (helpFlag opts) == True
        then showUsage >> exitWith ExitSuccess
        else return ()


  {----------------------------------------------}
  {- If --version is set, show version and quit -}
  {----------------------------------------------}
  _ <- if (versionFlag opts) == True
        then showVersion >> exitWith ExitSuccess
        else return ()


  _ <- if (replFlag opts) == True
        then repl
        else return ()


  {-----------------}
  {- Let's Do This -}
  {-----------------}

  -- Read and parse the template file.
  template <- attempt $ readAndParseDoc (templatePath opts)

  -- Read and parse the records file.
  records' <- attempt $ readAndParseStates (recordPath opts) (dataFormat opts)

  -- Note the user's template and record directories.
  let records = map (setPaths (templateDirPath opts) (recordDirPath opts) (dataFormat opts)) records'

  -- Evaluate template against all records.
  results <- attemptsWith records $ evalToText template

  -- Write results to stdout or a file(s).
  let (outFlag, outName) = outputFlag opts
  case results of
    [t] -> if outFlag == True
            then writeFile outName t
            else putStr t

    ts  -> if outFlag == True
            then padWrite outName ts
            else putStr $ unlines ts

  -- OOOHH YEAHH
  exitWith ExitSuccess



-- Write strings to name01, name02, etc. with leading zeros and file extensions.
padWrite :: String -> [String] -> IO ()
padWrite name strs = sequence_ $ zipWith foo strs ([1..] :: [Integer])
  where
    foo str k = writeFile (nam ++ pad k ++ "." ++ ext) str
    pad i = reverse $ take l $ (++ repeat '0') $ reverse $ show i
    l = length $ show $ length strs
    (x,y) = break (== '.') $ reverse name
    ext = reverse x
    nam = init $ reverse y


showUsage, showVersion :: IO ()
showUsage     = hPutStrLn stdout (usageInfo "Usage: feivel [OPTION...]" options)
showVersion   = hPutStrLn stdout version

showErrors :: [String] -> IO ()
showErrors es = hPutStrLn stderr (concat es)

data Flag = Flag
 { templateFlag    :: Bool, templatePath    :: String
 , recordFlag      :: Bool, recordPath      :: String
 , templateDirFlag :: Bool, templateDirPath :: String
 , recordDirFlag   :: Bool, recordDirPath   :: String
 , dataFormat      :: DataFormat
 , outputFlag      :: (Bool, String)
 , helpFlag        :: Bool
 , versionFlag     :: Bool
 , replFlag        :: Bool
 } deriving (Eq, Show)

defaultArgs :: Flag
defaultArgs = Flag 
 { templateFlag    = False, templatePath    = ""
 , recordFlag      = False, recordPath      = ""
 , templateDirFlag = False, templateDirPath = ""
 , recordDirFlag   = False, recordDirPath   = ""
 , dataFormat      = TypeKeyVal
 , outputFlag      = (False, "")
 , helpFlag        = False
 , versionFlag     = False
 , replFlag        = False
 }



options :: [OptDescr (Flag -> IO Flag)]
options = 
 [ Option [] ["help"]
     (NoArg (\opt -> return $ opt {helpFlag = True}))
     "show usage"

 , Option [] ["version"]
     (NoArg (\opt -> return $ opt {versionFlag = True}))
     "show version info"

 , Option [] ["repl"]
     (NoArg (\opt -> return $ opt {replFlag = True}))
     "REPL mode"

 , Option ['t'] ["template"]
     (ReqArg (\arg opt -> return $ opt {templateFlag = True, templatePath = arg}) "FILE")
     "template (if not set, use stdin)"

 , Option ['T'] ["template-dir"]
     (ReqArg (\arg opt -> return $ opt {templateDirFlag = True, templateDirPath = arg}) "PATH")
     "path to template directory"

 , Option ['d'] ["data"]
     (ReqArg (\arg opt -> return $ opt {recordFlag = True, recordPath = arg}) "FILE")
     "data (if not set, use empty context)"

 , Option ['D'] ["data-dir"]
     (ReqArg (\arg opt -> return $ opt {recordDirFlag = True, recordDirPath = arg}) "PATH")
     "path to data directory"

 , Option ['o'] ["output-name"]
     (ReqArg (\arg opt -> return $ opt {outputFlag = (True, arg)}) "STRING")
     "name for output files"

 , Option [] ["typekeyval"]
     (NoArg (\opt -> return $ opt {dataFormat = TypeKeyVal}))
     "parse data as typekeyval"

 , Option [] ["awk"]
     (ReqArg (\arg opt -> return $ opt {dataFormat = AWK {recordSeparator = arg}}) "STRING")
     "parse data in awk-mode"
 ]


version :: String
version = "feivel-0.0.1"

repl :: IO ()
repl = runRepl emptyStore
  where
    runRepl store = do
      str <- getLine
      (res,_) <- runEvalM emptyStore $ parseDoc "" str
      case res of
        Left (Goof _ (ErrREPL Quit)) -> do
          putStrLn "> bye!"
          exitWith ExitSuccess
        Left err -> do
          putStrLn (show err)
          runRepl store
        Right t -> do
          (txt, store') <- runEvalM store $ evalToText t
          case txt of
            Left err -> do
              putStrLn (show err)
              runRepl store
            Right x -> do
              putStrLn $ (if x=="" then "(ok)" else x) ++ "\n"
              runRepl store'
