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
import Yi.Accessor
import Yi.Editor
import Yi.Debug
import Yi.Buffer
import Data.List
import Control.Monad.Reader
import Yi.WindowSet as WS
import Data.Foldable (toList)
--------------------------------------------------------


-- | Rotate focus to the next window
nextWinE :: EditorM ()
nextWinE = modifyWindows WS.forward

-- | Rotate focus to the previous window
prevWinE :: EditorM ()
prevWinE = modifyWindows WS.backward

-- | Apply a function to the windowset.
modifyWindows :: (WindowSet Window -> WindowSet Window) -> EditorM ()
modifyWindows f = do
  b <- getsAndModifyA windowsA $ \ws -> let ws' = f ws in (ws', bufkey $ WS.current ws')
  setBuffer b
  return ()

withWindows :: (WindowSet Window -> a) -> EditorM a
withWindows = getsA windowsA

withWindow :: (Window -> a) -> EditorM a
withWindow f = getsA (WS.currentA .> windowsA) f

withWindowAndBuffer :: (Window -> BufferM a) -> EditorM a
withWindowAndBuffer f = do
  w <- getA (WS.currentA .> windowsA)
  withBuffer0 (f w)

-- | Split the current window, opening a second window onto current buffer.
splitE :: EditorM ()
splitE = do 
  b <- getBuffer
  let w = Window False b 0 0 0
  modifyWindows (WS.add w)


-- | Enlarge the current window
enlargeWinE :: EditorM ()
enlargeWinE = error "enlargeWinE: not implemented"

-- | Shrink the current window
shrinkWinE :: EditorM ()
shrinkWinE = error "shrinkWinE: not implemented"


-- | Close the current window, unless it is the last window open.
tryCloseE :: EditorM ()
tryCloseE = modifyWindows WS.delete

-- | Make the current window the only window on the screen
closeOtherE :: EditorM ()
closeOtherE = modifyWindows WS.deleteOthers

-- | Switch focus to some other window. If none is available, create one.
shiftOtherWindow :: EditorM ()
shiftOtherWindow = do
  len <- withWindows (length . toList)
  when (len == 1) splitE
  nextWinE
