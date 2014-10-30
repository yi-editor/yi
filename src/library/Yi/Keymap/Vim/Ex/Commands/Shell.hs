{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Shell
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Shell (parse) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Command
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.char '!'
    cmd <- T.pack <$> P.many1 (P.noneOf " ")
    args <- Common.commandArgs
    return $ Common.impureExCommand {
        cmdShow = T.pack "!"
      , cmdAction = YiA $ buildRun cmd args (const $ return ())
      }
