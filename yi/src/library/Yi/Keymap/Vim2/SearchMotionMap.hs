module Yi.Keymap.Vim2.SearchMotionMap
    ( defSearchMotionMap
    ) where

import Prelude ()
import Yi.Prelude

import Yi.Editor
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StateUtils
import Yi.Search

defSearchMotionMap :: [VimBinding]
defSearchMotionMap = [enterBinding, escBinding, otherBinding]

enterBinding :: VimBinding
enterBinding = VimBindingE prereq action
    where prereq "<CR>" (VimState { vsMode = Search {}} ) = WholeMatch ()
          prereq _ _ = NoMatch
          action _ = do
              Search cmd prevMode dir <- fmap vsMode getDynamic
              -- TODO: parse cmd into regex and flags
              doSearch (Just cmd) [] dir
              switchModeE prevMode
              case prevMode of
                  Visual _ -> return Continue
                  _ -> return Finish

otherBinding :: VimBinding
otherBinding = VimBindingE prereq action
    where prereq _ (VimState { vsMode = Search {}} ) = WholeMatch ()
          prereq _ _ = NoMatch
          action evs = do
              Search cmd pm dir <- fmap vsMode getDynamic
              case evs of
                [c] -> do
                    switchModeE $ Search (cmd ++ [c]) pm dir
                    return ()
                _ -> return ()
              return Continue

escBinding :: VimBinding
escBinding = VimBindingE prereq action
    where prereq "<Esc>" (VimState { vsMode = Search {}} ) = WholeMatch ()
          prereq _ _ = NoMatch
          action _ = do
              Search cmd prevMode dir <- fmap vsMode getDynamic
              switchModeE prevMode
              return Drop
