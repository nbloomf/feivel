module Feivel.GUI (gui) where

import Feivel.Store
import Feivel.Expr
import Feivel.EvalM
import Feivel.Eval
import Feivel.Parse

import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
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
  interaction <- newIORef []

  initGUI


  {- :Helpers -}
  let makeMonospaced widget = postGUIAsync $ do
        fd <- widgetGetPangoContext widget >>= contextGetFontDescription
        fontDescriptionSetFamily fd "Monospace"
        widgetModifyFont widget (Just fd)


  {- :MenuBar -}
  clearMenuItem <- menuItemNewWithMnemonic "_Clear Interaction"
  saveMenuItem  <- menuItemNewWithMnemonic "_Save Interaction"

  fileMenu <- menuNew
  menuShellAppend fileMenu clearMenuItem
  menuShellAppend fileMenu saveMenuItem

  fileMenuItem <- menuItemNewWithMnemonic "_File"
  menuItemSetSubmenu fileMenuItem fileMenu

  fontSizeIncMenuItem <- menuItemNewWithLabel "Increase Font Size\tCtrl +"
  fontSizeDecMenuItem <- menuItemNewWithLabel "Decrease Font Size\tCtrl -"

  viewMenu <- menuNew
  menuShellAppend viewMenu fontSizeIncMenuItem
  menuShellAppend viewMenu fontSizeDecMenuItem

  viewMenuItem <- menuItemNewWithMnemonic "_View"
  menuItemSetSubmenu viewMenuItem viewMenu

  menuBar <- menuBarNew
  menuShellAppend menuBar fileMenuItem
  menuShellAppend menuBar viewMenuItem


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
  mainWindow `on` deleteEvent $ do
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

  evalButton `on` buttonPressEvent $ do
    liftIO evalAction 
    return True

  mainWindow `on` keyPressEvent $ tryEvent $ do
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

  undoButton `on` buttonPressEvent $ do
    liftIO undoAction
    return True

  mainWindow `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "u"       <- eventKeyName
    liftIO undoAction



  let clearAction = do
        writeIORef interaction []
        outBuf <- textViewGetBuffer outputView
        set outBuf [textBufferText := ""]
        return ()
  
  clearMenuItem `on` buttonPressEvent $ do
    liftIO clearAction
    return True


  let adjustFontSize tweak = postGUIAsync $ do
        Just sz <- fontDescriptionGetSize fontDesc
        fontDescriptionSetSize fontDesc (tweak sz)
        widgetModifyFont outputView (Just fontDesc)
        widgetModifyFont inputView (Just fontDesc)

  fontSizeIncMenuItem `on` buttonPressEvent $ do
    liftIO $ adjustFontSize (\s -> s + 1)
    return True

  fontSizeDecMenuItem `on` buttonPressEvent $ do
    liftIO $ adjustFontSize (\s -> s - 1)
    return True

  mainWindow `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "equal"   <- eventKeyName
    liftIO $ adjustFontSize (\s -> s + 1)

  mainWindow `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "minus"   <- eventKeyName
    liftIO $ adjustFontSize (\s -> s - 1)


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
history x = concat $ zipWith foo (ios x) [1..]
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







