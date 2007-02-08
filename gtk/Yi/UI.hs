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

        -- * Window manipulation
        newWindow, enlargeWindow, shrinkWindow, 
        doResizeAll, deleteWindow, deleteWindow',
        hasRoomForExtraWindow,

        -- * Command line
        setCmdLine,

        -- * UI type, abstract.
        UI,

        module Yi.Event   -- UIs need to export the symbolic key names


  )   where

import Prelude hiding (error)

import Yi.Buffer
import Yi.Editor
import Yi.Window as Window
import Yi.Event
import Yi.Debug

import Control.Concurrent ( yield )
import Control.Concurrent.Chan

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Graphics.UI.Gtk hiding ( Window, Event )          
import qualified Graphics.UI.Gtk as Gtk


------------------------------------------------------------------------

data UI = UI {
              uiWindow :: Gtk.Window
             ,uiBox :: VBox
             ,uiCmdLine :: Label
             }

-- | Initialise the ui
start :: IO UI
start = do
  initGUI

  win <- windowNew

  ch <- newChan
  modifyEditor_ $ \e -> return $ e { input = ch }
  onKeyPress win (processEvent ch)

  vb <- vBoxNew False 1
  set win [ containerChild := vb ]
  onDestroy win mainQuit
                
  cmd <- labelNew Nothing
  set cmd [ miscXalign := 0.01 ]
  set vb [ containerChild := cmd, 
           boxChildPacking cmd := PackNatural, 
           boxChildPosition cmd := 10000 ] 

  -- use our magic threads thingy (http://haskell.org/gtk2hs/archives/2005/07/24/writing-multi-threaded-guis/)
  timeoutAddFull (yield >> return True) priorityDefaultIdle 50

  widgetShowAll win
  return $ UI win vb cmd

main :: IO ()
main = do logPutStrLn "GTK main loop running"
          mainGUI


instance Show Gtk.Event where
    show (Key _eventRelease _eventSent _eventTime eventModifier' _eventWithCapsLock _eventWithNumLock 
                  _eventWithScrollLock eventKeyName' eventKeyChar') 
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
    ]


addWindow :: UI -> Window -> IO ()
addWindow i w = do
  set (uiBox i) [containerChild := widget w, 
                 boxChildPosition (widget w) := 0]
  f <- fontDescriptionNew
  fontDescriptionSetFamily f "Monospace"
  widgetModifyFont (textview w) (Just f)
  textview w `onFocusIn` (\_event -> setWindow w >> return True)
  widgetShowAll (widget w)


-- | Clean up and go home
end :: UI -> IO ()
end _ = mainQuit

-- | Suspend the program
suspend :: IO ()
suspend = do 
  i <- readEditor ui
  windowIconify (uiWindow i) 




------------------------------------------------------------------------
-- | Window manipulation

-- | Create a new window onto this buffer.
--
newWindow :: FBuffer -> IO Window
newWindow b = modifyEditor $ \e -> do
    win  <- emptyWindow b (1,1) -- FIXME
    addWindow (ui e) win
    let e' = e { windows = M.fromList $ mkAssoc (win : M.elems (windows e)) }
    return (e', win)

-- ---------------------------------------------------------------------
-- | Grow the given window, and pick another to shrink
-- grow and shrink compliment each other, they could be refactored.
--
enlargeWindow :: Maybe Window -> IO ()
enlargeWindow _ = return () -- TODO

-- | shrink given window (just grow another)
shrinkWindow :: Maybe Window -> IO ()
shrinkWindow _ = return () -- TODO


--
-- | Delete a window. Note that the buffer that was connected to this
-- window is still open.
--
deleteWindow :: (Maybe Window) -> IO ()
deleteWindow Nothing    = return ()
deleteWindow (Just win) = modifyEditor_ $ \e -> deleteWindow' e win

-- internal, non-thread safe
deleteWindow' :: Editor -> Window -> IO Editor
deleteWindow' e win = do
  let i = ui e
  containerRemove (uiBox i) (widget win)
  return e

-- | Has the frame enough room for an extra window.
hasRoomForExtraWindow :: IO Bool
hasRoomForExtraWindow = return True

doResizeAll :: IO ()
doResizeAll = return ()

setCmdLine :: UI -> String -> IO ()
setCmdLine i s = do 
  set (uiCmdLine i) [labelText := s]

                
