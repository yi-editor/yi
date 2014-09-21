{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Write
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Write (parse) where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import           Yi.File
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types

parse :: EventString -> Maybe ExCommand
parse = Common.parse $
               P.choice [parseWrite, parseWriteAs]
    where parseWrite = do
            void $ P.try ( P.string "write") <|> P.string "w"
            alls <- P.many (P.try ( P.string "all") <|> P.string "a")
            return $! writeCmd $ not (null alls)

          parseWriteAs = do
            void $ P.try ( P.string "write") <|> P.string "w"
            void $ P.many1 P.space
            filename <- T.pack <$> P.many1 P.anyChar
            return $! writeAsCmd filename

writeCmd :: Bool -> ExCommand
writeCmd allFlag = Common.impureExCommand {
    cmdShow = "write" <> if allFlag then "all" else ""
  , cmdAction = YiA $ if allFlag then Common.forAllBuffers fwriteBufferE else viWrite
  }

writeAsCmd :: T.Text -> ExCommand
writeAsCmd filename = Common.impureExCommand {
    cmdShow = "write " <> filename
  , cmdAction = YiA $ viWriteTo filename
  }
