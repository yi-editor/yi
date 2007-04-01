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
        refreshAll, scheduleRefresh,

        -- * Window manipulation
        newWindow, enlargeWindow, shrinkWindow, deleteWindow,
        hasRoomForExtraWindow, setWindowBuffer, setWindow,

        -- * Command line
        setCmdLine,

        -- * UI type, abstract.
        UI,

        module Yi.Event   -- UIs need to export the symbolic key names


  )   where

import Prelude hiding (error)

import Yi.Buffer
import Yi.FastBuffer
import Yi.Editor
import Yi.Window as Window
import Yi.Event
import Yi.Debug

import Control.Concurrent ( yield )
import Control.Concurrent.Chan
import Control.Monad.Reader

import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Map as M

import Graphics.UI.Gtk hiding ( Window, Event )          
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.SourceView


------------------------------------------------------------------------

data UI = UI {
              uiWindow :: Gtk.Window
             ,uiBox :: VBox
             ,uiCmdLine :: Label
             }

-- | Initialise the ui
start :: EditorM ()
start = modifyEditor_ $ \e -> do
  initGUI

  win <- windowNew

  ch <- newChan
  onKeyPress win (processEvent ch)

  vb <- vBoxNew False 1
  vb' <- vBoxNew False 1
  set win [ containerChild := vb ]
  onDestroy win mainQuit
                
  cmd <- labelNew Nothing
  set cmd [ miscXalign := 0.01 ]
  set vb [ containerChild := vb', 
           containerChild := cmd, 
           boxChildPacking cmd := PackNatural] 

  -- use our magic threads thingy (http://haskell.org/gtk2hs/archives/2005/07/24/writing-multi-threaded-guis/)
  timeoutAddFull (yield >> return True) priorityDefaultIdle 50

  widgetShowAll win
  return $ e { input = ch, ui = UI win vb' cmd }

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
  set (uiBox i) [containerChild := widget w]
  f <- fontDescriptionNew
  fontDescriptionSetFamily f "Monospace"
  widgetModifyFont (textview w) (Just f)
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
newWindow :: FBuffer -> EditorM Window
newWindow b = do
  editor <- ask
  modifyEditor $ \e -> do
    win <- emptyWindow b
    logPutStrLn $ "Creating " ++ show win
    addWindow editor win
    let e' = e { windows = M.fromList $ mkAssoc (win : M.elems (windows e)) }
    return (e', win)

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

scheduleRefresh :: UI -> IO ()
scheduleRefresh _gui = return ()

setCmdLine :: UI -> String -> IO ()
setCmdLine i s = do 
  set (uiCmdLine i) [labelText := s]

                
-- | Display the given buffer in the given window.
setWindowBuffer :: FBuffer -> Maybe Window -> EditorM ()
setWindowBuffer b mw = do
    lift $ logPutStrLn $ "Setting buffer for " ++ show mw ++ " to " ++ show b
    w'' <- case mw of 
      Just w -> do lift $ textViewSetBuffer (textview w) (textbuf $ rawbuf b)
                   let w' = w { bufkey = bkey b }
                   return $ w' { key = key w }
      Nothing -> newWindow b
                   -- if there is no window, just create a new one.
    modifyEditor_ $ \e -> return $ e { windows = M.insert (key w'') w'' (windows e) }
    debugWindows "Buffer set"


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
