module Yi.Keymap.Vim2.SearchMotionMap
    ( defSearchMotionMap
    ) where

import Prelude ()
import Yi.Prelude

import Control.Monad (replicateM_)
import Data.List (drop)
import Data.Maybe (fromMaybe)

import Yi.Buffer
import Yi.Editor
import Yi.History
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Search
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.Utils
import Yi.Search

defSearchMotionMap :: [VimBinding]
defSearchMotionMap = [enterBinding, editBinding, exitBinding]

enterBinding :: VimBinding
enterBinding = VimBindingE prereq action
    where prereq "<CR>" (VimState { vsMode = Search {}} ) = WholeMatch ()
          prereq _ _ = NoMatch
          action _ = do
              Search prevMode dir <- fmap vsMode getDynamic
              -- TODO: parse cmd into regex and flags
              isearchFinishE
              historyFinish
              switchModeE prevMode

              count <- getCountE
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

editBinding :: VimBinding
editBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = Search {}} ) = matchFromBool $
            evs `elem` (fmap fst binds)
            || (null (drop 1 evs))
          prereq _ _ = NoMatch
          action evs = do
              fromMaybe (isearchAddE evs) (lookup evs binds)
              withBuffer0 elemsB >>= historyPrefixSet
              return Continue
          binds = [ ("<BS>", isearchDelE)
                  , ("<C-h>", isearchDelE)
                  , ("<C-p>", isearchHistory 1)
                  , ("<Up>", isearchHistory 1)
                  , ("<C-n>", isearchHistory (-1))
                  , ("<Down>", isearchHistory (-1))
                  , ("<lt>", isearchAddE "<")
                  ]

exitBinding :: VimBinding
exitBinding = VimBindingE prereq action
    where prereq _ (VimState { vsMode = Search {}} ) = WholeMatch ()
          prereq _ _ = NoMatch
          action _ = do
              Search prevMode _dir <- fmap vsMode getDynamic
              isearchCancelE
              switchModeE prevMode
              return Drop
