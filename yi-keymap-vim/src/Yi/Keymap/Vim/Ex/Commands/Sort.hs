{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Sort
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Sort (parse) where

import           Control.Monad                    (void)
import qualified Data.Attoparsec.Text             as P (match, string)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T (Text)
import           Yi.Buffer
import           Yi.Keymap                        (Action (BufferA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, parseRange, pureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdComplete, cmdShow))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    (regionText, region) <- P.match Common.parseRange
    void $ P.string "sort"
    return $ sort region regionText

sort :: Maybe (BufferM Region) -> T.Text -> ExCommand
sort r rt = Common.pureExCommand {
    cmdShow = rt <> "sort"
  , cmdAction = BufferA $ sortA r
  , cmdComplete = return [rt <> "sort"]
  }

sortA :: Maybe (BufferM Region) -> BufferM ()
sortA r = do
    region <- case r of
        Nothing -> regionOfB Document
        Just r' -> r'
    sortLinesWithRegion region{regionEnd = regionEnd region - 1}
