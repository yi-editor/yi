module Yi.Keymap.Vim2.Utils
  ( mkBinding
  , switchMode
  , resetCount
  , vimMoveE
  ) where

import Yi.Prelude
import Prelude ()

import Data.Maybe (fromMaybe)

import Yi.Buffer
import Yi.Event
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim2.Common

mkBinding :: VimMode -> (Event, YiM (), VimState -> VimState) -> VimBinding
mkBinding mode (event, action, mutate) = VimBinding event prereq action mutate
    where prereq (VimState m _) = m == mode

switchMode :: VimMode -> VimState -> VimState
switchMode mode state = state { vsMode = mode }

resetCount :: VimState -> VimState
resetCount s = s { vsCount = Nothing }

vimMoveE :: VimMotion -> EditorM ()
vimMoveE motion = do
    count <- fmap (fromMaybe 1 . vsCount) getDynamic
    withBuffer0 $ do
      case motion of
        VMLeft -> moveXorSol count
        VMRight -> moveXorEol count
        VMUp -> discard $ lineMoveRel (-count)
        VMDown -> discard $ lineMoveRel count
      leftOnEol
