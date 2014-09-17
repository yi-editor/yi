{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Edit
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implements quit commands.

module Yi.Keymap.Vim.Ex.Commands.Edit (parse) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Editor
import           Yi.File
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    tab <- P.many (P.string "tab")
    void $ P.try ( P.string "edit") <|> P.string "e"
    void $ P.many1 P.space
    filename <- T.pack <$> P.many1 P.anyChar
    return $! edit (not (null tab)) filename

edit :: Bool -> T.Text -> ExCommand
edit tab f = Common.impureExCommand {
    cmdShow = showEdit tab f
  , cmdAction = YiA $ do
        when tab $ withEditor newTabE
        void . editFile $ T.unpack f
  , cmdComplete = (fmap . fmap)
                    (showEdit tab) (Common.filenameComplete f)
  }

showEdit :: Bool -> T.Text -> T.Text
showEdit tab f = (if tab then "tab" else "") `T.append` "edit " `T.append` f
