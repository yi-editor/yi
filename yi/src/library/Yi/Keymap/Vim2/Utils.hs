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

import Control.Monad (replicateM_)

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
    let repeat = replicateM_ count
    withBuffer0 $ do
      case motion of
        (VMChar Backward) -> moveXorSol count
        (VMChar Forward) -> moveXorEol count
        (VMLine Backward) -> discard $ lineMoveRel (-count)
        (VMLine Forward) -> discard $ lineMoveRel count
        (VMWordStart Backward) -> repeat $ moveB unitViWord Backward
        (VMWordStart Forward) -> repeat $ genMoveB unitViWord (Backward, InsideBound) Forward
        (VMWordEnd Backward) -> repeat $ genMoveB unitViWord (Backward, InsideBound) Backward
        (VMWordEnd Forward) -> repeat $ genMoveB unitViWord (Forward, InsideBound) Forward
        (VMWORDStart Backward) -> repeat $ moveB unitViWORD Backward
        (VMWORDStart Forward) -> repeat $ genMoveB unitViWORD (Backward, InsideBound) Forward
        (VMWORDEnd Backward) -> repeat $ genMoveB unitViWORD (Backward, InsideBound) Backward
        (VMWORDEnd Forward) -> repeat $ genMoveB unitViWORD (Forward, InsideBound) Forward
        VMSOL -> moveToSol
        VMEOL -> do
            when (count > 1) $ discard $ lineMoveRel (count - 1)
            moveToEol
        VMNonEmptySOL -> firstNonSpaceB
      leftOnEol

isBindingApplicable :: Event -> VimState -> VimBinding -> Bool
isBindingApplicable e s (VimBindingE prereq _) = prereq e s
isBindingApplicable e s (VimBindingY prereq _) = prereq e s
