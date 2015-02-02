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
import qualified Text.ParserCombinators.Parsec    as P (string)
import           Yi.Buffer
import           Yi.Keymap                        (Action (BufferA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, parseRange, pureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdComplete, cmdShow))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    region <- Common.parseRange
    void $ P.string "sort"
    return $ sort region

sort :: Maybe (BufferM Region) -> ExCommand
sort r = Common.pureExCommand {
    cmdShow = "sort"
  , cmdAction = BufferA $ sortA r
  , cmdComplete = return ["sort"]
  }

sortA :: Maybe (BufferM Region) -> BufferM ()
sortA r = do
    region <- case r of
        Nothing -> regionOfB Document
        Just r' -> r'
    sortLinesWithRegion region
