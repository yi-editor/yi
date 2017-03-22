{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Delete
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Delete (parse) where

import           Control.Applicative              (Alternative ((<|>)))
import           Control.Monad                    (void)
import qualified Data.Attoparsec.Text             as P (string, try, match)
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        ()
import           Data.Semigroup                   ((<>))
import           Lens.Micro.Platform
import           Yi.Buffer.Adjusted               hiding (Delete)
import           Yi.Keymap                        (Action (BufferA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, pureExCommand, parseRange)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    (rangeText, rangeB) <- over _2 (fromMaybe currentLineRegionB) 
      <$> P.match Common.parseRange
    void $ P.try ( P.string "delete") <|> P.string "d"
    return $ Common.pureExCommand {
        cmdShow = rangeText <> "delete"
      , cmdAction = BufferA $ deleteRegionB =<< rangeB
      }
  where currentLineRegionB = flip convertRegionToStyleB LineWise =<< regionOfB Line
