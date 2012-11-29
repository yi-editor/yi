module Yi.Keymap.Vim2.Utils
  ( mkBindingE
  , mkBindingY
  , mkStringBindingE
  , isBindingApplicable
  , splitCountedCommand
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
              then combineAction action mutate rtoken
              else return Continue

mkBindingE :: VimMode -> RepeatToken -> (Event, EditorM (), VimState -> VimState) -> VimBinding
mkBindingE mode rtoken (event, action, mutate) = VimBindingE prereq combinedAction
    where prereq ev vs = vsMode vs == mode && ev == event
          combinedAction _ = combineAction action mutate rtoken

mkBindingY :: VimMode -> (Event, YiM (), VimState -> VimState) -> VimBinding
mkBindingY mode (event, action, mutate) = VimBindingY prereq combinedAction
    where prereq ev vs = vsMode vs == mode && ev == event
          combinedAction _ = combineAction action mutate Drop

combineAction :: MonadEditor m => m () -> (VimState -> VimState) -> RepeatToken -> m RepeatToken
combineAction action mutateState rtoken = do
    action
    withEditor $ modifyStateE mutateState
    return rtoken

isBindingApplicable :: Event -> VimState -> VimBinding -> Bool
isBindingApplicable e s b = vbPrerequisite b e s

