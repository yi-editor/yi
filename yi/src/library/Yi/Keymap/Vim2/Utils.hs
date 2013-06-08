module Yi.Keymap.Vim2.Utils
  ( mkBindingE
  , mkBindingY
  , mkStringBindingE
  , splitCountedCommand
  , selectBinding
  , matchFromBool
  , mkMotionBinding
  , matchesString
  ) where

import Yi.Prelude
import Prelude ()

import Data.List (isPrefixOf, group)

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
          prereq evs _ = evs `matchesString` eventString
          combinedAction _ = combineAction action mutate rtoken

mkBindingE :: VimMode -> RepeatToken -> (Event, EditorM (), VimState -> VimState) -> VimBinding
mkBindingE mode rtoken (event, action, mutate) = VimBindingE prereq combinedAction
    where prereq evs vs = matchFromBool $ vsMode vs == mode && evs == eventToString event
          combinedAction _ = combineAction action mutate rtoken

mkBindingY :: VimMode -> (Event, YiM (), VimState -> VimState) -> VimBinding
mkBindingY mode (event, action, mutate) = VimBindingY prereq combinedAction
    where prereq evs vs = matchFromBool $ vsMode vs == mode && evs == eventToString event
          combinedAction _ = combineAction action mutate Drop

combineAction :: MonadEditor m => m () -> (VimState -> VimState) -> RepeatToken -> m RepeatToken
combineAction action mutateState rtoken = do
    action
    withEditor $ modifyStateE mutateState
    return rtoken

selectBinding :: String -> VimState -> [VimBinding] -> MatchResult VimBinding
selectBinding eventString state = foldl go NoMatch
    where go match b = match <|> fmap (const b) (vbPrerequisite b eventString state)

matchFromBool :: Bool -> MatchResult ()
matchFromBool b = if b then WholeMatch () else NoMatch

mkMotionBinding :: RepeatToken -> (VimMode -> Bool) -> VimBinding
mkMotionBinding token condition = VimBindingE prereq action
    where prereq evs state | condition (vsMode state) = fmap (const ()) (stringToMove evs)
          prereq _ _ = NoMatch
          action evs = do
              state <- getDynamic
              let WholeMatch (Move _style isJump move) = stringToMove evs
              count <- getMaybeCountE
              when isJump addJumpHereE
              withBuffer0 $ move count >> leftOnEol
              resetCountE

              -- moving with j/k after $ sticks cursor to the right edge
              when (evs == "$") $ setStickyEolE True
              when (evs `elem` group "jk" && vsStickyEol state) $
                  withBuffer0 $ moveToEol >> moveXorSol 1
              when (evs `notElem` group "jk$") $ setStickyEolE False

              let m = head evs
              when (m `elem` "fFtT") $ do
                  let c = last evs
                      (dir, style) =
                          case m of
                              'f' -> (Forward, Inclusive)
                              't' -> (Forward, Exclusive)
                              'F' -> (Backward, Inclusive)
                              'T' -> (Backward, Exclusive)
                              _ -> error "can't happen"
                      command = GotoCharCommand c dir style
                  modifyStateE $ \s -> s { vsLastGotoCharCommand = Just command}

              return token

matchesString :: String -> String -> MatchResult ()
matchesString got expected | expected == got = WholeMatch ()
                           | got `isPrefixOf` expected = PartialMatch
                           | otherwise = NoMatch
