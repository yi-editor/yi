{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Nohl
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Nohl (parse) where

import           Data.Text                        ()
import           Yi.Keymap                        (Action (EditorA))
import           Yi.Keymap.Vim.Common             (EventString)
import           Yi.Keymap.Vim.Ex.Commands.Common (pureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.Search                        (resetRegexE)

parse :: EventString -> Maybe ExCommand
parse s = if s == "nohl" || s == "nohlsearch"
    then Just nohl
    else Nothing

nohl :: ExCommand
nohl = pureExCommand {
    cmdAction = EditorA resetRegexE
  , cmdShow = "nohlsearch"
  }
