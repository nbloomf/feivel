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

module Feivel.GUI (gui) where

import Feivel.Store
import Feivel.Expr
import Feivel.EvalM
import Feivel.Eval
import Feivel.Parse
import Feivel.GUI.Strings

import System.IO (hPutStrLn, stderr)
import Data.IORef
import Graphics.UI.Gtk
import Control.Monad.IO.Class


{--------------------}
{- :MenuBar         -}
{- :InteractionPane -}
{- :ButtonBox       -}
{- :VBox            -}
{- :MainWindow      -}
{- :EventHandlers   -}
{--------------------}

gui :: IO ()
gui = do
  -- GUI State
  interaction <- newIORef []

  _ <- initGUI


  {- :MenuBar -}
  clearMenuItem <- menuItemNewWithMnemonic "_Clear Interaction"
  saveMenuItem  <- menuItemNewWithMnemonic "_Save Interaction as... (Ctrl s)"

  fileMenu <- menuNew
  menuShellAppend fileMenu clearMenuItem
  menuShellAppend fileMenu saveMenuItem

  fileMenuItem <- menuItemNewWithMnemonic "_File"
  menuItemSetSubmenu fileMenuItem fileMenu

  fontSizeIncMenuItem <- menuItemNewWithLabel "Increase Font Size (Ctrl +)"
  fontSizeDecMenuItem <- menuItemNewWithLabel "Decrease Font Size (Ctrl -)"

  viewMenu <- menuNew
  menuShellAppend viewMenu fontSizeIncMenuItem
  menuShellAppend viewMenu fontSizeDecMenuItem

  viewMenuItem <- menuItemNewWithMnemonic "_View"
  menuItemSetSubmenu viewMenuItem viewMenu

  aboutMenuItem <- menuItemNewWithLabel "About"

  helpMenu <- menuNew
  menuShellAppend helpMenu aboutMenuItem

  helpMenuItem <- menuItemNewWithMnemonic "_Help"
  menuItemSetSubmenu helpMenuItem helpMenu

  menuBar <- menuBarNew
  menuShellAppend menuBar fileMenuItem
  menuShellAppend menuBar viewMenuItem
  menuShellAppend menuBar helpMenuItem


  {- :InteractionPane -}
  fontDesc <- fontDescriptionNew
  fontDescriptionSetFamily fontDesc "Monospace"
  fontDescriptionSetSize   fontDesc 14

  outputBuf    <- textBufferNew Nothing
  outputView   <- textViewNewWithBuffer outputBuf
  widgetModifyFont outputView (Just fontDesc)
  textViewSetWrapMode outputView WrapChar
  textViewSetEditable outputView False
  outputScroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy outputScroll PolicyNever PolicyAlways
  set outputScroll [scrolledWindowShadowType := ShadowEtchedIn]
  scrolledWindowAddWithViewport outputScroll outputView

  inputBuf    <- textBufferNew Nothing
  inputView   <- textViewNewWithBuffer inputBuf
  widgetModifyFont inputView (Just fontDesc)
  textViewSetWrapMode inputView WrapChar
  inputScroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy inputScroll PolicyNever PolicyAlways
  set inputScroll [scrolledWindowShadowType := ShadowEtchedIn]
  scrolledWindowAddWithViewport inputScroll inputView

  interactPane <- vPanedNew
  panedPack1 interactPane outputScroll True True
  panedPack2 interactPane inputScroll  True True
  panedSetPosition interactPane 320


  {- :ButtonBox -}
  evalButton <- buttonNewWithMnemonic "_Eval"
  undoButton <- buttonNewWithMnemonic "_Undo"

  buttonBox  <- hBoxNew False 5
  boxPackStart buttonBox evalButton PackNatural 0
  boxPackEnd   buttonBox undoButton PackNatural 0


  {- :VBox -}
  vBox <- vBoxNew False 5
  boxPackStart vBox menuBar      PackNatural 0
  boxPackStart vBox interactPane PackGrow    0
  boxPackStart vBox buttonBox    PackNatural 0


  {- :MainWindow -}
  mainWindow <- windowNew
  set mainWindow
    [ windowTitle         := "Feivel"
    , windowDefaultWidth  := 640
    , windowDefaultHeight := 480
    ]
  containerAdd mainWindow vBox


  {- :EventHandlers -}
  _ <- mainWindow `on` deleteEvent $ do
         liftIO mainQuit
         return False



  let textViewGetValue tv = do
        buf   <- textViewGetBuffer tv
        start <- textBufferGetStartIter buf
        end   <- textBufferGetEndIter buf
        value <- textBufferGetText buf start end True
        return value

  let scrollOutput = do
        adj <- scrolledWindowGetVAdjustment outputScroll
        adjustmentGetUpper adj >>= adjustmentSetValue adj
        scrolledWindowSetVAdjustment outputScroll adj

  let evalAction = do
        x <- readIORef interaction
        input <- textViewGetValue inputView
        if input == ""
          then return ()
          else do
            c@((Step _ output _):_) <- evalString x input
            set inputBuf  [textBufferText := ""]
            set outputBuf [textBufferText := history c]
            writeIORef interaction c
            scrollOutput
            return ()

  _ <- evalButton `on` buttonPressEvent $ do
         liftIO evalAction 
         return True

  _ <- mainWindow `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "Return"  <- eventKeyName
         liftIO evalAction



  let undoAction = do
        x <- readIORef interaction
        case x of
          [] -> return ()
          _:ys -> do
            writeIORef interaction ys
            outBuf <- textViewGetBuffer outputView
            set outBuf [textBufferText := history ys]
            return ()

  _ <- undoButton `on` buttonPressEvent $ do
         liftIO undoAction
         return True

  _ <- mainWindow `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "u"       <- eventKeyName
         liftIO undoAction



  let clearAction = do
        writeIORef interaction []
        outBuf <- textViewGetBuffer outputView
        set outBuf [textBufferText := ""]
        return ()
  
  _ <- clearMenuItem `on` buttonPressEvent $ do
         liftIO clearAction
         return True


  let adjustFontSize tweak = postGUIAsync $ do
        Just sz <- fontDescriptionGetSize fontDesc
        fontDescriptionSetSize fontDesc (tweak sz)
        widgetModifyFont outputView (Just fontDesc)
        widgetModifyFont inputView (Just fontDesc)

  _ <- fontSizeIncMenuItem `on` buttonPressEvent $ do
         liftIO $ adjustFontSize (\s -> s + 1)
         return True

  _ <- fontSizeDecMenuItem `on` buttonPressEvent $ do
         liftIO $ adjustFontSize (\s -> s - 1)
         return True

  _ <- mainWindow `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "equal"   <- eventKeyName
         liftIO $ adjustFontSize (\s -> s + 1)

  _ <- mainWindow `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "minus"   <- eventKeyName
         liftIO $ adjustFontSize (\s -> s - 1)



  let tellAbout = do
        ad <- aboutDialogNew
        aboutDialogSetName     ad ui_name_string
        aboutDialogSetVersion  ad ui_version_string
        aboutDialogSetAuthors  ad ui_authors
        aboutDialogSetComments ad ui_about_comment
        dialogRun ad
        widgetDestroy ad

  _ <- aboutMenuItem `on` buttonPressEvent $ do
         liftIO tellAbout
         return True



  let saveInteraction = do
        sd <- fileChooserDialogNew
                (Just "Save interaction as...")
                (Just mainWindow)
                FileChooserActionSave
                [("Cancel", ResponseCancel), ("Save", ResponseOk)]
        resp <- dialogRun sd
        case resp of
          ResponseOk -> do
            path <- fileChooserGetFilename sd
            acts <- readIORef interaction
            case path of
              Nothing   -> hPutStrLn stderr "Bad filename chosen!"
              Just file -> writeFile file (history acts)
          _ -> return ()
        widgetDestroy sd

  _ <- saveMenuItem `on` buttonPressEvent $ do
         liftIO saveInteraction
         return True

  _ <- mainWindow `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "s"       <- eventKeyName
         liftIO saveInteraction



  {- :Finally -}
  widgetGrabFocus inputView
  widgetShowAll mainWindow
  mainGUI



{----------------}
{- :Interaction -}
{----------------}

data Step = Step
  { inStr  :: String
  , outStr :: String
  , state  :: Store Expr
  }

type Interaction = [Step]

ios :: Interaction -> [(String, String)]
ios = reverse . map (\s -> (inStr s, outStr s))

history :: Interaction -> String
history x = concat $ zipWith foo (ios x) [(1::Integer)..]
  where
    foo (inp,outp) k = "(" ++ show k ++ ") " ++ inp ++ "\n\n"
      ++ outp ++ (if outp == "" then "" else "\n\n")

evalString :: Interaction -> String -> IO Interaction
evalString [] input = do
  (output, st) <- parseAndEvalIO emptyStore input
  return [Step input output st]

evalString context@(s:_) input = do
  (output, st) <- parseAndEvalIO (state s) input
  return ((Step input output st):context)

parseAndEvalIO :: Store Expr -> String -> IO (String, Store Expr)
parseAndEvalIO st input = do
  case runParseM pREPL "" input of
    Left goof -> return (show goof, st)
    Right (x,_) -> do
      (y, stNew) <- runEvalM st (evalToText x)
      case y of
        Left goof -> return (show goof, st)
        Right z -> return (z, stNew)







