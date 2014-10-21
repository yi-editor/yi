{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Cabal
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Cabal (parse) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Command
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types
import           Yi.MiniBuffer

-- TODO: Either hack Text into these parsec parsers or use Attoparsec.
-- Attoparsec is faster anyway and backtracks by default so we may
-- want to use that anyway.

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try (P.string "cabal build") <|> P.try (P.string "cabal")
    args <- Common.commandArgs
    return $ Common.impureExCommand {
        cmdShow = T.pack "cabal build"
      , cmdAction = YiA $ cabalBuildE $ CommandArguments args
      }
