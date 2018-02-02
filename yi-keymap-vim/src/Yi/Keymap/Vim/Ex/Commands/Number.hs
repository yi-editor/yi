{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Number
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Toggles line numbers.

module Yi.Keymap.Vim.Ex.Commands.Number (parse) where

import           Data.Monoid                      ((<>))
import           Yi.Editor                        (getEditorDyn, printMsg, putEditorDyn)
import           Yi.Keymap                        (Action (EditorA))
import           Yi.Keymap.Vim.Common             (EventString)
import           Yi.Keymap.Vim.Ex.Commands.Common (BoolOptionAction (..), parseBoolOption)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand)
import           Yi.String                        (showT)
import           Yi.UI.LineNumbers                (DisplayLineNumbers (..))

parse :: EventString -> Maybe ExCommand
parse = parseBoolOption "number" action

action :: BoolOptionAction -> Action
action BoolOptionAsk = EditorA $ do
    value <- getDisplayLineNumbers <$> getEditorDyn
    printMsg $ "number = " <> showT value
action (BoolOptionSet b) = EditorA $ putEditorDyn (DisplayLineNumbers b)
action BoolOptionInvert = EditorA $ do
  x <- getDisplayLineNumbers <$> getEditorDyn
  putEditorDyn (DisplayLineNumbers (not x))
