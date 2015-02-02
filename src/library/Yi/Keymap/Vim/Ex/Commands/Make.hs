{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Make
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Make (parse) where

import           Control.Applicative              (Applicative ((*>)))
import qualified Data.Text                        as T (pack)
import qualified Text.ParserCombinators.Parsec    as P (string)
import           Yi.Command                       (makeBuildE)
import           Yi.Keymap                        (Action (YiA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (commandArgs, impureExCommand, parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.MiniBuffer                    (CommandArguments (CommandArguments))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    args <- P.string "make" *> Common.commandArgs
    return $ Common.impureExCommand {
        cmdShow = T.pack "make"
      , cmdAction = YiA $ makeBuildE $ CommandArguments args
      }
