module Yi.Keymap.Vim2.Utils
  ( mkBindingE
  , mkBindingY
  , mkStringBindingE
  , matchBinding 
  , splitCountedCommand
  , selectBinding
  , matchFromBool
  , mkMotionBinding
  ) where

import Yi.Prelude
import Prelude ()

import Data.List (isPrefixOf)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Event
import Yi.Keymap
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Motion
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.EventUtils

-- 'mkBindingE' and 'mkBindingY' are helper functions for bindings
-- where VimState mutation is not dependent on action performed
-- and prerequisite has form (mode == ... && event == ...)

mkStringBindingE :: VimMode -> RepeatToken
    -> (String, EditorM (), VimState -> VimState) -> VimBinding
mkStringBindingE mode rtoken (eventString, action, mutate) = VimBindingE prereq combinedAction
    where prereq _ vs | vsMode vs /= mode = NoMatch
          prereq ev vs | (vsAccumulator vs ++ eventToString ev) == eventString = WholeMatch ()
          prereq ev vs | (vsAccumulator vs ++ eventToString ev) `isPrefixOf` eventString = PartialMatch
          prereq _ _ = NoMatch
          combinedAction _ = combineAction action mutate rtoken

mkBindingE :: VimMode -> RepeatToken -> (Event, EditorM (), VimState -> VimState) -> VimBinding
mkBindingE mode rtoken (event, action, mutate) = VimBindingE prereq combinedAction
    where prereq ev vs = if vsMode vs == mode && ev == event then WholeMatch () else NoMatch
          combinedAction _ = combineAction action mutate rtoken

mkBindingY :: VimMode -> (Event, YiM (), VimState -> VimState) -> VimBinding
mkBindingY mode (event, action, mutate) = VimBindingY prereq combinedAction
    where prereq ev vs = if vsMode vs == mode && ev == event then WholeMatch () else NoMatch
          combinedAction _ = combineAction action mutate Drop

combineAction :: MonadEditor m => m () -> (VimState -> VimState) -> RepeatToken -> m RepeatToken
combineAction action mutateState rtoken = do
    action
    withEditor $ modifyStateE mutateState
    return rtoken

matchBinding :: Event -> VimState -> VimBinding -> MatchResult ()
matchBinding e s b = vbPrerequisite b e s

selectBinding :: Event -> VimState -> [VimBinding] -> MatchResult VimBinding
selectBinding e s = foldl go NoMatch
    where go match b = match <|> fmap (const b) (vbPrerequisite b e s)

matchFromBool :: Bool -> MatchResult ()
matchFromBool b = if b then WholeMatch () else NoMatch

mkMotionBinding :: (VimMode -> Bool) -> VimBinding
mkMotionBinding pred = VimBindingE prereq action
    where prereq e (VimState { vsMode = mode, vsBindingAccumulator = bacc }) | pred mode =
              let s = bacc ++ eventToString e
              in fmap (const ()) (stringToMove s)
          prereq _ _ = NoMatch
          action e = do
              state <- getDynamic
              let s = vsBindingAccumulator state ++ estring
                  estring = eventToString e
                  WholeMatch (Move _style move) = stringToMove s
              count <- getMaybeCountE
              withBuffer0 $ move count >> leftOnEol
              resetCountE

              -- moving with j/k after $ sticks cursor to the right edge
              when (estring == "$") $ setStickyEolE True
              when (estring `elem` ["j", "k"] && vsStickyEol state) $
                  withBuffer0 $ moveToEol >> leftB
              when (estring `notElem` ["j", "k", "$"]) $ setStickyEolE False

              return Drop
