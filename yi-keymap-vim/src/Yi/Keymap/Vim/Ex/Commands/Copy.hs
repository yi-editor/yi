{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Copy
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- :copy ex command to copy selection to the clipboard.
module Yi.Keymap.Vim.Ex.Commands.Copy (parse) where

import           Control.Monad                    (void)
import qualified Text.ParserCombinators.Parsec    as P (string)
import           Yi.Editor                        (withCurrentBuffer)
import           Yi.Keymap                        (Action (YiA))
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, impureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.Keymap.Vim.Common             (EventString)
import           Yi.Types                         (YiM)
import           Yi.Rope                          (toString, toText)
import           Yi.Buffer.HighLevel              (getRawestSelectRegionB)
import           Yi.Buffer.Region                 (readRegionB)
import           Control.Monad.Base               (liftBase)
import           System.Hclip                     (setClipboard)
import           Yi.Core                          (errorEditor)
import           Yi.Editor                        (printMsg, withEditor)
import           Data.Monoid                      ((<>))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void (P.string "'<,'>copy")
    return $ Common.impureExCommand {
        cmdShow = "copy"
      , cmdAction = YiA copy
      }

copy :: YiM ()
copy = do
  selectionString <- withCurrentBuffer (readRegionB =<< getRawestSelectRegionB)
  withEditor $ printMsg ("copied \"" <> toText selectionString <> "\"")
  case toString selectionString of
    "" -> errorEditor "Cannot copy empty selection"
    s  -> liftBase (setClipboard s)
