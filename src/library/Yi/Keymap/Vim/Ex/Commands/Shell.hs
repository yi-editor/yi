{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Shell
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Shell (parse) where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (void)
import qualified Data.Text                        as T (pack)
import qualified Text.ParserCombinators.Parsec    as P (char, many1, noneOf)
import           Yi.Command                       (buildRun)
import           Yi.Keymap                        (Action (YiA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (commandArgs, impureExCommand, parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.char '!'
    cmd <- T.pack <$> P.many1 (P.noneOf " ")
    args <- Common.commandArgs
    return $ Common.impureExCommand {
        cmdShow = T.pack "!"
      , cmdAction = YiA $ buildRun cmd args (const $ return ())
      }
