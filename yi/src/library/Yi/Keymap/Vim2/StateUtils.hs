module Yi.Keymap.Vim2.StateUtils
  ( switchMode
  , switchModeE
  , resetCount
  , resetCountE
  , modifyStateE
  , getCountE
  ) where

import Yi.Prelude
import Prelude ()

import Data.Maybe (fromMaybe)

import Yi.Editor
import Yi.Keymap.Vim2.Common

switchMode :: VimMode -> VimState -> VimState
switchMode mode state = state { vsMode = mode }

switchModeE :: VimMode -> EditorM ()
switchModeE mode = modifyStateE $ switchMode mode

modifyStateE :: (VimState -> VimState) -> EditorM ()
modifyStateE f = do
    currentState <- getDynamic
    setDynamic $ f currentState

resetCount :: VimState -> VimState
resetCount s = s { vsCount = Nothing }

resetCountE :: EditorM ()
resetCountE = modifyStateE resetCount

getCountE :: EditorM Int
getCountE = do
    currentState <- getDynamic
    return $! fromMaybe 1 (vsCount currentState)
