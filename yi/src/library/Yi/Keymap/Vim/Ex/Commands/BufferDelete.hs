{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.BufferDelete
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.BufferDelete (parse) where

import           Control.Applicative
import           Control.Monad
import           Data.Text ()
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Editor
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try ( P.string "bdelete") <|> P.try ( P.string "bdel") <|> P.try (P.string "bd")
    return $ Common.pureExCommand {
        cmdShow = "bdelete"
      , cmdAction = EditorA closeBufferAndWindowE
      }
