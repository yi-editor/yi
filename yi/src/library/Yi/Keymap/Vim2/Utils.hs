module Yi.Keymap.Vim2.Utils
  ( mkBindingE
  , mkBindingY
  , mkStringBindingE
  , vimMoveE
  , isBindingApplicable
  , splitCountedCommand
  , normalizeCount
  ) where

import Yi.Prelude
import Prelude ()

import Control.Monad (replicateM_)

import Data.Char (isDigit)
import Data.List (break, isPrefixOf)

import Yi.Buffer
import Yi.Editor
import Yi.Event
import Yi.Keymap
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.TextObject
import Yi.Keymap.Vim2.EventUtils

-- 'mkBindingE' and 'mkBindingY' are helper functions for bindings
-- where VimState mutation is not dependent on action performed
-- and prerequisite has form (mode == ... && event == ...)

mkStringBindingE :: VimMode -> RepeatToken
    -> (String, EditorM (), VimState -> VimState) -> VimBinding
mkStringBindingE mode rtoken (eventString, action, mutate) = VimBindingE prereq combinedAction
    -- TODO: Consider making prereq return Match | Partial | None
    -- instead of Bool
    where prereq ev vs = vsMode vs == mode
                       && (vsAccumulator vs ++ eventToString ev) `isPrefixOf` eventString
          combinedAction ev = do
              currentState <- getDynamic
              let accum = vsAccumulator currentState
              if accum ++ eventToString ev == eventString
              then do
                  action
                  setDynamic $ mutate currentState
                  return rtoken
              else return Continue

mkBindingE :: VimMode -> RepeatToken -> (Event, EditorM (), VimState -> VimState) -> VimBinding
mkBindingE mode rtoken (event, action, mutate) = VimBindingE prereq combinedAction
    where prereq ev vs = vsMode vs == mode && ev == event
          combinedAction _ = do
              currentState <- getDynamic
              action
              setDynamic $ mutate currentState
              return rtoken

mkBindingY :: VimMode -> (Event, YiM (), VimState -> VimState) -> VimBinding
mkBindingY mode (event, action, mutate) = VimBindingY prereq combinedAction
    where prereq ev vs = vsMode vs == mode && ev == event
          combinedAction _ = do
              currentState <- withEditor getDynamic
              action
              withEditor $ setDynamic $ mutate currentState
              return Drop

vimMoveE :: VimMotion -> EditorM ()
vimMoveE motion = do
    count <- getCountE
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
isBindingApplicable e s b = vbPrerequisite b e s

-- 2d3w -> 6dw
-- 6dw -> 6dw
-- dw -> dw
normalizeCount :: String -> String
normalizeCount s = if null countedObject
                   then s
                   else show (operatorCount * objectCount) ++ operator ++ object
    where (operatorCount, rest1) = splitCountedCommand s
          (operator, countedObject) = break isDigit rest1
          (objectCount, object) = splitCountedCommand countedObject

