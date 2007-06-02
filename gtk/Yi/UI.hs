--
-- Copyright (c) 2007 Jean-Philippe Bernardy
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--


-- | This module defines a user interface implemented using gtk2hs.

module Yi.UI (

        -- * UI initialisation
        start, end, suspend, main,

        -- * Refresh
        refreshAll, scheduleRefresh, prepareAction,

        -- * Window manipulation
        newWindow, enlargeWindow, shrinkWindow, deleteWindow,
        hasRoomForExtraWindow, setWindowBuffer, setWindow,

        -- * Command line
        setCmdLine,

        -- * UI type, abstract.
        UI,

        -- * For dynamic hacking, give access to the window.
        uiWindow,

        module Yi.Event   -- UIs need to export the symbolic key names
  )   where

import Prelude hiding (error)

import Yi.Buffer
import Yi.FastBuffer
import Yi.Editor
import Yi.Keymap
import Yi.Window as Window
import Yi.Event
import Yi.Debug
import Yi.Undo

import Control.Concurrent ( yield )
import Control.Concurrent.Chan
import Control.Monad.Reader

import Data.IORef
import Data.List
import Data.Maybe
import Data.Unique
import qualified Data.Map as M

import Graphics.UI.Gtk hiding ( Window, Event )          
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.SourceView

------------------------------------------------------------------------

data UI = UI {
              uiWindow :: Gtk.Window
             ,uiBox :: VBox
             ,uiCmdLine :: Label
             ,uiBuffers :: IORef (M.Map Unique SourceBuffer)
             }

-- | Initialise the ui
start :: EditorM ()
start = modifyEditor_ $ \e -> do
  initGUI

  win <- windowNew

  ch <- newChan
  onKeyPress win (processEvent ch)

  vb <- vBoxNew False 1  -- Top-level vbox
  vb' <- vBoxNew False 1

  set win [ containerChild := vb ]
  onDestroy win mainQuit
                
  cmd <- labelNew Nothing
  set cmd [ miscXalign := 0.01 ]
  f <- fontDescriptionNew
  fontDescriptionSetFamily f "Monospace"
  widgetModifyFont cmd (Just f)

  set vb [ containerChild := vb', 
           containerChild := cmd, 
           boxChildPacking cmd := PackNatural] 

  -- use our magic threads thingy (http://haskell.org/gtk2hs/archives/2005/07/24/writing-multi-threaded-guis/)
  timeoutAddFull (yield >> return True) priorityDefaultIdle 50

  widgetShowAll win

  bufs <- newIORef M.empty
  return $ e { input = ch, ui = UI win vb' cmd bufs }

main :: IORef Editor -> IO ()
main _editor = 
    do logPutStrLn "GTK main loop running"
       mainGUI


instance Show Gtk.Event where
    show (Key _eventRelease _eventSent _eventTime eventModifier' _eventWithCapsLock _eventWithNumLock 
                  _eventWithScrollLock _eventKeyVal eventKeyName' eventKeyChar') 
        = show eventModifier' ++ " " ++ show eventKeyName' ++ " " ++ show eventKeyChar'
    show _ = "Not a key event"

instance Show Gtk.Modifier where
    show Control = "Ctrl"
    show Alt = "Alt"
    show Shift = "Shift"
    show Apple = "Apple"
    show Compose = "Compose"
    
processEvent :: Chan Event -> Gtk.Event -> IO Bool
processEvent ch ev = do
  -- logPutStrLn $ "Gtk.Event: " ++ show ev
  -- logPutStrLn $ "Event: " ++ show (gtkToYiEvent ev)
  case gtkToYiEvent ev of
    Nothing -> logPutStrLn $ "Event not translatable: " ++ show ev
    Just e -> writeChan ch e
  -- This is very ridiculous, but improves responsivity dramatically.
  -- The idea is to give other threads the chance to process their queues immediately.
  -- One yield is not enough for this (I guess GHC 6.6.1 scheduler is to blame), I 
  -- empirically found that 4 is a good value for me.
  yield 
  yield
  yield
  yield
  return True
            
gtkToYiEvent :: Gtk.Event -> Maybe Event
gtkToYiEvent (Key {eventKeyName = keyName, eventModifier = modifier, eventKeyChar = char})
    = fmap (\k -> Event k $ (nub $ (if isShift then filter (not . (== MShift)) else id) $ map modif modifier)) key'
      where (key',isShift) = 
                case char of
                  Just c -> (Just $ KASCII c, True)
                  Nothing -> (M.lookup keyName keyTable, False)
            modif Control = MCtrl
            modif Alt = MMeta
            modif Shift = MShift
            modif Apple = MMeta
            modif Compose = MMeta
gtkToYiEvent _ = Nothing

-- | Map GTK long names to Keys
keyTable :: M.Map String Key
keyTable = M.fromList 
    [("Down",       KDown)
    ,("Up",         KUp)
    ,("Left",       KLeft)
    ,("Right",      KRight)
    ,("Home",       KHome)
    ,("End",        KEnd)
    ,("BackSpace",  KBS)
    ,("Delete",     KDel)
    ,("Page_Up",    KPageUp)
    ,("Page_Down",  KPageDown)
    ,("Insert",     KIns)
    ,("Escape",     KEsc)
    ,("Return",     KEnter)
    ,("Tab",        KASCII '\t')
    ]


addWindow :: IORef Editor -> Window -> IO ()
addWindow editor w = do
  i <- liftM ui $ readIORef editor
  set (uiBox i) [containerChild := widget w,
                 boxChildPacking (widget w) := if isMini w then PackNatural else PackGrow]
  textview w `onFocusIn` (\_event -> (modifyIORef editor $ \e -> e { curwin = Just $ key w }) >> return False)
  -- We have to return false so that GTK correctly focuses the window when we use widgetGrabFocus
  textview w `onMoveCursor` \step amount user -> do
      logPutStrLn $ "moveCursor: " ++ show step ++ show amount ++ show user
      -- gtk experts: we don't seem to get any of those events... why? 
      -- Duncan Coutts advises binding to the mark-set signal

      --forgetPerferCol (findBufferWith e (bufkey w))

  widgetShowAll (widget w)

instance Show MovementStep where
    show MovementLogicalPositions = "MovementLogicalPosition"		
    show MovementVisualPositions  = "MovementVisualPositions"         
    show MovementWords	      = "MovementWords	        " 
    show MovementDisplayLines     = "MovementDisplayLines	" 
    show MovementDisplayLineEnds  = "MovementDisplayLineEnds"         
    show MovementParagraphs	      = "MovementParagraphs	"         
    show MovementParagraphEnds    = "MovementParagraphEnds	" 
    show MovementPages	      = "MovementPages	        " 
    show MovementBufferEnds	      = "MovementBufferEnds	"         
    show MovementHorizontalPages  = "MovementHorizontalPages"         

-- | Clean up and go home
end :: UI -> IO ()
end _ = mainQuit

-- | Suspend the program
suspend :: EditorM ()
suspend = do 
  i <- readEditor ui
  lift $ windowIconify (uiWindow i) 




------------------------------------------------------------------------
-- | Window manipulation

-- | Create a new window onto this buffer.
--
newWindow :: Bool -> FBuffer -> EditorM Window
newWindow mini b = do
  editor <- ask
  win <- lift $ do 
    win <- emptyWindow mini b
    logPutStrLn $ "Creating " ++ show win
    addWindow editor win
    return win
  setWindowBuffer b (Just win)
  return win

-- ---------------------------------------------------------------------
-- | Grow the given window, and pick another to shrink
-- grow and shrink compliment each other, they could be refactored.
--
enlargeWindow :: Maybe Window -> EditorM ()
enlargeWindow _ = return () -- TODO

-- | shrink given window (just grow another)
shrinkWindow :: Maybe Window -> EditorM ()
shrinkWindow _ = return () -- TODO


--
-- | Delete a window. Note that the buffer that was connected to this
-- window is still open.
--
deleteWindow :: (Maybe Window) -> EditorM ()
deleteWindow Nothing    = return ()
deleteWindow (Just win) = do
  deleteWindow' win
  i <- readEditor ui
  lift $ containerRemove (uiBox i) (widget win)
  -- now switch focus to a random window
  ws <- readEditor getWindows
  case ws of
    [] -> lift $ logPutStrLn "All windows deleted!"
    (w:_) -> setWindow w 

-- | Has the frame enough room for an extra window.
hasRoomForExtraWindow :: EditorM Bool
hasRoomForExtraWindow = return True

refreshAll :: EditorM ()
refreshAll = return ()

scheduleRefresh :: EditorM ()
scheduleRefresh = modifyEditor_ $ \e-> do
    let ws = getWindows e
    bufs <- readIORef $ uiBuffers $ ui $ e
    sequence_ [applyUpdate (bufs M.! b) u >> logPutStrLn (show $ u) | (b,u) <- editorUpdates e]
    flip mapM_ ws $ \w -> 
        do let buf = findBufferWith e (bufkey w)
               gtkBuf = bufs M.! (bufkey w)
           (p0, []) <- runBuffer buf pointB
           insert <- textBufferGetInsert gtkBuf
           i <- textBufferGetIterAtOffset gtkBuf p0
           textBufferPlaceCursor gtkBuf i
           textViewScrollMarkOnscreen (textview w) insert
           (txt, []) <- runBuffer buf getModeLine 
           set (modeline w) [labelText := txt]
    return e {editorUpdates = []}

applyUpdate buf (Insert p s) = do
  i <- textBufferGetIterAtOffset buf p
  textBufferInsert buf i s
applyUpdate buf (Delete p s) = do
  start <- textBufferGetIterAtOffset buf p
  end <- textBufferGetIterAtOffset buf (p + s)
  textBufferDelete buf start end

prepareAction :: EditorM ()
prepareAction = do
  bufsRef <- withUI $ return . uiBuffers
  withBuffer $ do
    p0 <- pointB
    b <- ask
    
    p1 <- lift $ do
            tb <- liftM (M.! bkey b) $ readIORef bufsRef
            insert <- textBufferGetInsert tb
            i <- textBufferGetIterAtMark tb insert
            get i textIterOffset
    when (p1 /= p0) $ do
       moveTo p1 -- as a side effect we forget the prefered column

setCmdLine :: UI -> String -> IO ()
setCmdLine i s = do
  set (uiCmdLine i) [labelText := if length s > 132 then take 129 s ++ "..." else s]

-- | Display the given buffer in the given window.
setWindowBuffer :: FBuffer -> Maybe Window -> EditorM ()
setWindowBuffer b Nothing = do w <- newWindow False b; setWindowBuffer b (Just w)
 -- if there is no window, just create a new one.
setWindowBuffer b (Just w) = do
    lift $ logPutStrLn $ "Setting buffer for " ++ show w ++ " to " ++ show b
    bufsRef <- withUI $ return . uiBuffers
    bufs <- lift $ readIORef bufsRef
    gtkBuf <- case M.lookup (bkey b) bufs of
      Just gtkBuf -> return gtkBuf
      Nothing -> lift $ newBuffer b
    lift $ textViewSetBuffer (textview w) gtkBuf
    lift $ modifyIORef bufsRef (M.insert (bkey b) gtkBuf)
    let w' = w { bufkey = bkey b }
    modifyEditor_ $ \e -> return $ e { windows = M.insert (key w') w' (windows e) }
    debugWindows "Buffer set"

-- FIXME: when a buffer is deleted its GTK counterpart should be too.
newBuffer :: FBuffer -> IO SourceBuffer
newBuffer b = do
  buf <- sourceBufferNew Nothing
  lm <- sourceLanguagesManagerNew
  Just haskellLang <- sourceLanguagesManagerGetLanguageFromMimeType lm "text/x-haskell"
  sourceBufferSetLanguage buf haskellLang
  sourceBufferSetHighlight buf True
  (txt, []) <- runBuffer b elemsB
  textBufferSetText buf txt
  return buf
  

--
-- | Set current window
-- !! reset the buffer point from the window point
--
-- Factor in shift focus.
--
setWindow :: Window -> EditorM ()
setWindow w = do
  lift $ logPutStrLn $ "Focusing " ++ show w 
  modifyEditor_ $ \e -> return $ e { curwin = Just $ key w }
  lift $ widgetGrabFocus (textview w)
  debugWindows "Focused"
