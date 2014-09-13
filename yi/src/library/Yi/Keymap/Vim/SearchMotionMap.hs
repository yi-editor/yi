{-# LANGUAGE LambdaCase #-}
module Yi.Keymap.Vim.SearchMotionMap
    ( defSearchMotionMap
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe (fromMaybe)

import Yi.Buffer.Adjusted
import Yi.Editor
import Yi.History
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.Search
import Yi.Keymap.Vim.StateUtils
import Yi.Keymap.Vim.Utils
import Yi.Search

defSearchMotionMap :: [VimBinding]
defSearchMotionMap = [enterBinding, editBinding, exitBinding]

enterBinding :: VimBinding
enterBinding = VimBindingE f
    where f "<CR>" (VimState { vsMode = Search {}} ) = WholeMatch $ do
              Search prevMode dir <- fmap vsMode getDynamic
              -- TODO: parse cmd into regex and flags
              isearchFinishE
              historyFinish
              switchModeE prevMode

              count <- getCountE
              getRegexE >>= \case
                Nothing -> return ()
                Just regex -> withBuffer0 $
                  if count == 1 && dir == Forward
                  then do
                    -- Workaround for isearchFinishE leaving cursor after match
                    continueVimSearch (regex, Backward)
                    continueVimSearch (regex, Forward)
                  else replicateM_ (count - 1) $ continueVimSearch (regex, dir)
              case prevMode of
                  Visual _ -> return Continue
                  _ -> return Finish
          f _ _ = NoMatch

editBinding :: VimBinding
editBinding = VimBindingE f
    where f evs (VimState { vsMode = Search {}} )
            = action evs <$
              matchFromBool (evs `elem` fmap fst binds || null (drop 1 evs))
          f _ _ = NoMatch
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
exitBinding = VimBindingE f
    where f _ (VimState { vsMode = Search {}} ) = WholeMatch $ do
              Search prevMode _dir <- fmap vsMode getDynamic
              isearchCancelE
              switchModeE prevMode
              return Drop
          f _ _ = NoMatch
