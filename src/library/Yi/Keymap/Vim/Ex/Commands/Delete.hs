{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Delete
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Delete (parse) where

import Control.Applicative ( Alternative((<|>)) )
import Control.Monad ( void )
import Data.Text ()
import qualified Text.ParserCombinators.Parsec as P ( try, string )
import Yi.Buffer.Adjusted
    ( Direction(Forward), TextUnit(Line), deleteUnitB, deleteN )
import Yi.Keymap ( Action(BufferA) )
import Yi.Keymap.Vim.Common ( EventString )
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
    ( parse, pureExCommand )
import Yi.Keymap.Vim.Ex.Types ( ExCommand(cmdAction, cmdShow) )

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try ( P.string "delete") <|> P.string "d"
    return $ Common.pureExCommand {
        cmdShow = "delete"
      , cmdAction = BufferA $ do
            deleteUnitB Line Forward
            deleteN 1
      }
