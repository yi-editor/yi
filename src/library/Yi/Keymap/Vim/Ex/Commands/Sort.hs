{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Sort
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Sort (parse) where

import           Control.Monad
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Buffer
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types

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