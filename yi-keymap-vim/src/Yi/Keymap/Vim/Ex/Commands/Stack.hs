{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Stack
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Stack (parse) where

import           Control.Applicative              (Alternative ((<|>)), optional)
import           Data.Attoparsec.Text             () -- import IsString (Parser a)
import           Yi.Command                       (stackBuildE)
import           Yi.Keymap                        (Action (YiA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (commandArgs, impureExCommand, parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.MiniBuffer                    (CommandArguments (CommandArguments))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    args <- "stack" *> optional " build" *> Common.commandArgs
    return $ Common.impureExCommand {
        cmdShow = "stack build"
      , cmdAction = YiA $ stackBuildE $ CommandArguments args
      }
