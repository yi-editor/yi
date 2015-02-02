{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.BufferDelete
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.BufferDelete (parse) where

import           Control.Applicative              (Alternative ((<|>)))
import           Control.Monad                    (void)
import           Data.Text                        ()
import qualified Text.ParserCombinators.Parsec    as P (string, try)
import           Yi.Editor                        (closeBufferAndWindowE)
import           Yi.Keymap                        (Action (EditorA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, pureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try ( P.string "bdelete") <|> P.try ( P.string "bdel") <|> P.try (P.string "bd")
    return $ Common.pureExCommand {
        cmdShow = "bdelete"
      , cmdAction = EditorA closeBufferAndWindowE
      }
