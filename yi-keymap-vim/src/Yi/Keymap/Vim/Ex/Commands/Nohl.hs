{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Nohl
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Nohl (parse) where

import qualified Data.Text                        as T
import           Yi.Keymap                        (Action (EditorA))
import           Yi.Keymap.Vim.Common             (EventString(..))
import           Yi.Keymap.Vim.Ex.Commands.Common (pureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.Search                        (resetRegexE)

parse :: EventString -> Maybe ExCommand
parse (Ev s)
  | T.isPrefixOf s "nohlsearch" && T.compareLength s 2 == GT = Just nohl
  | otherwise                                                = Nothing

nohl :: ExCommand
nohl = pureExCommand {
    cmdAction = EditorA resetRegexE
  , cmdShow = "nohlsearch"
  }
