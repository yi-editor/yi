--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

-- UI-related code common between all frontends.

module Yi.CoreUI where

import Prelude hiding (error)
import Yi.CommonUI
import Yi.Editor
import Yi.Debug
import Yi.Buffer
import Data.List
import Yi.Keymap
import Yi.Monad
import Control.Monad.Reader
import Yi.WindowSet as WS
import Control.Concurrent.MVar
import Data.Foldable (toList)
--------------------------------------------------------


-- | Rotate focus to the next window
nextWinE :: YiM ()
nextWinE = modifyWindows WS.forward

-- | Rotate focus to the previous window
prevWinE :: YiM ()
prevWinE = modifyWindows WS.backward

-- | Apply a function to the windowset.
modifyWindows :: (WindowSet Window -> WindowSet Window) -> YiM ()
modifyWindows f = do
  wsRef <- asks yiWindows
  b <- liftIO $ modifyMVar wsRef $ \ws -> let ws' = f ws in return (ws', bufkey $ WS.current ws')
  withEditor (setBuffer b)
  return ()

withWindows :: (WindowSet Window -> a) -> YiM a
withWindows f = do
  wsRef <- asks yiWindows
  liftIO $ withMVar wsRef $ \ws -> return (f ws)

withWindow :: (Window -> a) -> YiM a
withWindow f = withWindows (f . WS.current)

withWindowAndBuffer :: (Window -> BufferM a) -> YiM a
withWindowAndBuffer f = do
  wsRef <- asks yiWindows 
  editorRef <- asks yiEditor
  liftIO $ withMVar wsRef $ \ws -> do e <- readRef editorRef
                                      let (a,e') = runEditor (withBuffer0 (f (WS.current ws))) e
                                      writeRef editorRef e'
                                      return a

-- | Split the current window, opening a second window onto current buffer.
splitE :: YiM ()
splitE = do 
  b <- withEditor $ getBuffer
  let w = Window False b 0 0 0
  modifyWindows (WS.add w)

-- | Switch focus to some other window. If none is available, create one.
shiftOtherWindow :: YiM ()
shiftOtherWindow = do
  len <- withWindows (length . toList)
  when (len == 1) splitE
  nextWinE


withOtherWindow :: YiM () -> YiM ()
withOtherWindow f = do
  shiftOtherWindow
  f
  prevWinE


-- | Enlarge the current window
enlargeWinE :: YiM ()
enlargeWinE = error "enlargeWinE: not implemented"

-- | Shrink the current window
shrinkWinE :: YiM ()
shrinkWinE = error "shrinkWinE: not implemented"


-- | Close the current window, unless it is the last window open.
tryCloseE :: YiM ()
tryCloseE = modifyWindows WS.delete

-- | Make the current window the only window on the screen
closeOtherE :: YiM ()
closeOtherE = modifyWindows WS.deleteOthers
