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
        hasRoomForExtraWindow, setFocusedWindowBuffer, setWindow,
        withWindow0, getWindow, getWindows,

        -- * Command line
        setCmdLine,

        -- * UI type, abstract.
        UI,

        -- * For dynamic hacking, give access to the window.
        uiWindow,

        module Yi.Event   -- UIs need to export the symbolic key names
  )   where

import Prelude hiding (error, sequence_)

import Yi.Buffer
import Yi.Editor
import Yi.Window as Window
import Yi.Event
import Yi.Debug
import Yi.Undo
import Yi.Monad
import qualified Yi.CommonUI as Common
import qualified Yi.WindowSet as WS

import Control.Concurrent ( yield )
import Control.Concurrent.Chan
import Control.Monad.Reader (ask, lift, liftIO, liftM, when, MonadIO)

import Data.IORef
import Data.List
import Data.Maybe
import Data.Unique
import Data.Foldable
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
             ,windows   :: IORef (WS.WindowSet Window)     -- ^ all the windows
             }


mkUI ui = Common.UI 
  {
   Common.main                  = main                  ui,
   Common.end                   = end,
   Common.suspend               = suspend               ui,
   Common.refreshAll            = return (),
   Common.scheduleRefresh       = scheduleRefresh       ui,
   Common.prepareAction         = prepareAction         ui,
   Common.newWindow             = newWindow             ui,
   Common.enlargeWindow         = enlargeWindow         ui,
   Common.shrinkWindow          = shrinkWindow          ui,
   Common.deleteWindow          = deleteWindow          ui,
   Common.hasRoomForExtraWindow = hasRoomForExtraWindow ui,
   Common.setFocusedWindowBuffer= setFocusedWindowBuffer ui,
   Common.setWindow             = setWindow             ui,
   Common.setCmdLine            = setCmdLine            ui,
   Common.withWindow0           = withWindow0           ui,
   Common.getWindows            = getWindows            ui,
   Common.setWindows            = setWindows            ui,
   Common.getWindow             = getWindow             ui
  }

-- | Initialise the ui
start :: FBuffer -> EditorM (Chan Event, Common.UI)
start buf = lift $ do
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

  -- creation of the 1st window is a bit tricky because we disallow an empty WindowSet.
  w0 <- emptyWindow False buf
  ws0 <- newIORef (WS.new w0)
  let ui = UI win vb' cmd bufs ws0
  addWindow ui w0

  return (ch, mkUI ui)


main :: UI -> IORef Editor -> IO ()
main _editor _ui = 
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


addWindow :: UI -> Window -> IO ()
addWindow i w = do
  modifyRef (windows i) (WS.add w)
  set (uiBox i) [containerChild := widget w,
                 boxChildPacking (widget w) := if isMini w then PackNatural else PackGrow]
  textview w `onFocusIn` (\_event -> (modifyIORef (windows i) (WS.setFocus w)) >> return False)
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
end :: IO ()
end = mainQuit

-- | Suspend the program
suspend :: UI -> EditorM ()
suspend ui = do 
 lift $ windowIconify (uiWindow ui) 




------------------------------------------------------------------------
-- | Window manipulation

-- | Create a new window onto this buffer.
--
newWindow :: UI -> Bool -> FBuffer -> EditorM Window
newWindow ui mini b = do
  win <- lift $ do 
    win <- emptyWindow mini b
    logPutStrLn $ "Creating " ++ show win
    addWindow ui win
    return win
  setWindow ui win
  setFocusedWindowBuffer ui b
  return win

-- ---------------------------------------------------------------------
-- | Grow the given window, and pick another to shrink
-- grow and shrink compliment each other, they could be refactored.
--
enlargeWindow :: UI -> Window -> EditorM ()
enlargeWindow _ _ = return () -- TODO

-- | shrink given window (just grow another)
shrinkWindow :: UI -> Window -> EditorM ()
shrinkWindow _ _ = return () -- TODO


--
-- | Delete a window. Note that the buffer that was connected to this
-- window is still open.
--
deleteWindow :: UI -> Window -> EditorM ()
deleteWindow i win = do
  modifyRef (windows i) (WS.delete . WS.setFocus win)
  lift $ containerRemove (uiBox i) (widget win)
  w <- getWindow i
  setWindow i w

-- | Has the frame enough room for an extra window.
hasRoomForExtraWindow :: UI -> EditorM Bool
hasRoomForExtraWindow _ = return True

refreshAll :: EditorM ()
refreshAll = return ()

scheduleRefresh :: UI -> EditorM ()
scheduleRefresh ui = modifyEditor_ $ \e-> do
    ws <- readRef (windows ui)
    bufs <- readIORef $ uiBuffers $ ui
    sequence_ [applyUpdate (bufs M.! b) u >> logPutStrLn (show $ u) | (b,u) <- editorUpdates e]
    forM_ ws $ \w -> 
        do let buf = findBufferWith e (bufkey w)
               gtkBuf = bufs M.! (bufkey w)
           (p0, []) <- runBuffer buf pointB
           insertMark <- textBufferGetInsert gtkBuf
           i <- textBufferGetIterAtOffset gtkBuf p0
           textBufferPlaceCursor gtkBuf i
           textViewScrollMarkOnscreen (textview w) insertMark
           (txt, []) <- runBuffer buf getModeLine 
           set (modeline w) [labelText := txt]
    return e {editorUpdates = []}

applyUpdate :: SourceBuffer -> URAction -> IO ()
applyUpdate buf (Insert p s) = do
  i <- textBufferGetIterAtOffset buf p
  textBufferInsert buf i s
applyUpdate buf (Delete p s) = do
  i0 <- textBufferGetIterAtOffset buf p
  i1 <- textBufferGetIterAtOffset buf (p + s)
  textBufferDelete buf i0 i1

prepareAction :: UI -> EditorM ()
prepareAction ui = do
  let bufsRef = uiBuffers ui
  withBuffer0 $ do
    p0 <- pointB
    b <- ask
    
    p1 <- lift $ do
            tb <- liftM (M.! bkey b) $ readIORef bufsRef
            insertMark <- textBufferGetInsert tb
            i <- textBufferGetIterAtMark tb insertMark
            get i textIterOffset
    when (p1 /= p0) $ do
       moveTo p1 -- as a side effect we forget the prefered column

setCmdLine :: UI -> String -> IO ()
setCmdLine i s = do
  set (uiCmdLine i) [labelText := if length s > 132 then take 129 s ++ "..." else s]

-- | Display the given buffer in the given window.
setFocusedWindowBuffer :: UI -> FBuffer -> EditorM ()
setFocusedWindowBuffer ui b = do
    let bufsRef = uiBuffers ui
    bufs <- readRef bufsRef
    gtkBuf <- case M.lookup (bkey b) bufs of
      Just gtkBuf -> return gtkBuf
      Nothing -> lift $ newBuffer b
    ws <- readRef (windows ui)
    lift $ textViewSetBuffer (textview $ WS.current ws) gtkBuf
    modifyRef bufsRef (M.insert (bkey b) gtkBuf)
    modifyRef (windows ui) (WS.modifyCurrent $ \w -> w { bufkey = bkey b })

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
  
-- | Set current window

-- FIXME: reset the buffer point from the window point

setWindow :: UI -> Window -> EditorM ()
setWindow ui w = do
  logPutStrLn $ "Focusing " ++ show w 
  setBuffer (bufkey w)
  modifyRef (windows ui) (WS.setFocus w)
  liftIO $ widgetGrabFocus (textview w)

withWindow0 :: MonadIO m => UI -> (Window -> a) -> m a
withWindow0 ui f = do
  ws <- readRef (windows ui)
  return (f $ WS.current ws)

getWindows :: MonadIO m => UI -> m (WS.WindowSet Window)
getWindows ui = readRef $ windows ui

setWindows ui ws = writeRef (windows ui) ws

getWindow :: MonadIO m => UI -> m Window
getWindow ui = do
  ws <- getWindows ui
  return (WS.current ws)
