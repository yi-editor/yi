module Yi.Keymap.Vim2.Utils
  ( mkBindingE
  , mkBindingY
  , switchMode
  , resetCount
  , vimMoveE
  , isBindingApplicable
  ) where

import Yi.Prelude
import Prelude ()

import Data.Maybe (fromMaybe)

import Yi.Buffer
import Yi.Editor
import Yi.Event
import Yi.Keymap
import Yi.Keymap.Vim2.Common

mkBindingE :: VimMode -> (Event, EditorM (), VimState -> VimState) -> VimBinding
mkBindingE mode (event, action, mutate) = VimBindingE prereq combinedAction
    where prereq ev (VimState m _) = m == mode && ev == event
          combinedAction = do
              currentState <- getDynamic
              action
              setDynamic $ mutate currentState

mkBindingY :: VimMode -> (Event, YiM (), VimState -> VimState) -> VimBinding
mkBindingY mode (event, action, mutate) = VimBindingY prereq combinedAction
    where prereq ev (VimState m _) = m == mode && ev == event
          combinedAction = do
              currentState <- withEditor $ getDynamic
              action
              withEditor $ setDynamic $ mutate currentState

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

isBindingApplicable :: Event -> VimState -> VimBinding -> Bool
isBindingApplicable e s (VimBindingE prereq _) = prereq e s
isBindingApplicable e s (VimBindingY prereq _) = prereq e s
