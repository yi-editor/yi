module Yi.Keymap.Vim2.SearchMotionMap
    ( defSearchMotionMap
    ) where

import Prelude ()
import Yi.Prelude

import Control.Monad (replicateM_)

import Yi.Buffer
import Yi.Editor
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Search
import Yi.Keymap.Vim2.StateUtils
import Yi.Search

defSearchMotionMap :: [VimBinding]
defSearchMotionMap = [enterBinding, escBinding, otherBinding]

enterBinding :: VimBinding
enterBinding = VimBindingE prereq action
    where prereq "<CR>" (VimState { vsMode = Search {}} ) = WholeMatch ()
          prereq _ _ = NoMatch
          action _ = do
              Search _cmd prevMode dir <- fmap vsMode getDynamic
              -- TODO: parse cmd into regex and flags
              count <- getCountE
              isearchFinishE
              switchModeE prevMode

              Just regex <- getRegexE
              withBuffer0 $ if count == 1 && dir == Forward
                            then do
                                -- Workaround for isearchFinishE leaving cursor after match
                                continueVimSearch (regex, Backward)
                                continueVimSearch (regex, Forward)
                            else replicateM_ (count - 1) $ continueVimSearch (regex, dir)

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
                "<BS>" -> isearchDelE
                "<C-h>" -> isearchDelE
                "<lt>" -> do
                    switchModeE $ Search (cmd ++ ['<']) pm dir
                    isearchAddE "<"
                [c] -> do
                    switchModeE $ Search (cmd ++ [c]) pm dir
                    isearchAddE evs
                _ -> return ()
              return Continue

escBinding :: VimBinding
escBinding = VimBindingE prereq action
    where prereq "<Esc>" (VimState { vsMode = Search {}} ) = WholeMatch ()
          prereq _ _ = NoMatch
          action _ = do
              Search _cmd prevMode _dir <- fmap vsMode getDynamic
              isearchCancelE
              switchModeE prevMode
              return Drop
