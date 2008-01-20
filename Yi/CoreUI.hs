--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2007 Jean-Philippe Bernardy
--
--

-- UI-related code common between all frontends.

module Yi.CoreUI where

import Prelude hiding (error)
import Yi.Accessor
import Yi.Editor
import Yi.Debug
import Yi.Window
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
  -- TODO: push this fiddling with current buffer into windowsA
  setBuffer b
  return ()

withWindows :: (WindowSet Window -> a) -> EditorM a
withWindows = getsA windowsA

withWindow :: (Window -> a) -> EditorM a
withWindow f = getsA (WS.currentA .> windowsA) f

-- | Split the current window, opening a second window onto current buffer.
splitE :: EditorM ()
splitE = do 
  b <- getBuffer
  modifyWindows (WS.add $ dummyWindow b)


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
