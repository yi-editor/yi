{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.SearchMotionMap
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.SearchMotionMap (defSearchMotionMap) where

import           Control.Applicative      ((<$))
import           Control.Monad            (replicateM_)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T (pack, unpack)
import           Yi.Buffer.Adjusted       (Direction (Backward, Forward), elemsB)
import           Yi.Editor                (getEditorDyn, withCurrentBuffer)
import           Yi.History               (historyFinish, historyPrefixSet)
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.Search     (continueVimSearch)
import           Yi.Keymap.Vim.StateUtils (getCountE, switchModeE)
import           Yi.Keymap.Vim.Utils      (matchFromBool)
import qualified Yi.Rope                  as R (toText)
import           Yi.Search

defSearchMotionMap :: [VimBinding]
defSearchMotionMap = [enterBinding, editBinding, exitBinding]

enterBinding :: VimBinding
enterBinding = VimBindingE f
    where f "<CR>" (VimState { vsMode = Search {}} ) = WholeMatch $ do
              Search prevMode dir <- fmap vsMode getEditorDyn
              -- TODO: parse cmd into regex and flags
              isearchFinishE
              historyFinish
              switchModeE prevMode

              count <- getCountE
              getRegexE >>= \case
                Nothing -> return ()
                Just regex -> withCurrentBuffer $
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
editBinding = VimBindingE (f . T.unpack . _unEv)
  where
    f evs (VimState { vsMode = Search {}} )
      = action evs <$
        matchFromBool (evs `elem` fmap (T.unpack . fst) binds
                       || null (drop 1 evs))
    f _ _ = NoMatch
    action evs = do
      let evs' = T.pack evs
      fromMaybe (isearchAddE evs') (lookup evs' binds)
      withCurrentBuffer elemsB >>= historyPrefixSet . R.toText
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
              Search prevMode _dir <- fmap vsMode getEditorDyn
              isearchCancelE
              switchModeE prevMode
              return Drop
          f _ _ = NoMatch
