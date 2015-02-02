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

import           Control.Applicative              (Alternative ((<|>)), (<$>))
import           Control.Monad                    (void, when)
import qualified Data.Text                        as T (Text, append, pack, unpack)
import qualified Text.ParserCombinators.Parsec    as P (anyChar, many, many1, space, string, try)
import           Yi.Editor                        (MonadEditor (withEditor), newTabE)
import           Yi.File                          (openNewFile)
import           Yi.Keymap                        (Action (YiA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (filenameComplete, impureExCommand, parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdComplete, cmdShow))

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
        openNewFile $ T.unpack f
  , cmdComplete = (fmap . fmap)
                    (showEdit tab) (Common.filenameComplete f)
  }

showEdit :: Bool -> T.Text -> T.Text
showEdit tab f = (if tab then "tab" else "") `T.append` "edit " `T.append` f
