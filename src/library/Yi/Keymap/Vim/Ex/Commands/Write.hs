{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Write
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Write (parse) where

import           Control.Applicative              (Alternative ((<|>)), Applicative ((*>)), (<$>))
import           Control.Monad                    (void, when)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T (Text, pack)
import qualified Text.ParserCombinators.Parsec    as P (anyChar, many, many1, space, string, try)
import           Yi.Buffer                        (BufferRef)
import           Yi.Editor                        (printMsg)
import           Yi.File                          (fwriteBufferE, viWrite, viWriteTo)
import           Yi.Keymap                        (Action (YiA), YiM)
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (forAllBuffers, impureExCommand, needsSaving, parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $
               (P.try (P.string "write") <|> P.string "w")
            *> (parseWriteAs <|> parseWrite)
    where parseWrite = do
            alls <- P.many (P.try ( P.string "all") <|> P.string "a")
            return $! writeCmd $ not (null alls)

          parseWriteAs = do
            void $ P.many1 P.space
            filename <- T.pack <$> P.many1 P.anyChar
            return $! writeAsCmd filename

writeCmd :: Bool -> ExCommand
writeCmd allFlag = Common.impureExCommand {
    cmdShow = "write" <> if allFlag then "all" else ""
  , cmdAction = YiA $ if allFlag
      then Common.forAllBuffers tryWriteBuffer >> printMsg "All files written"
      else viWrite
  }

writeAsCmd :: T.Text -> ExCommand
writeAsCmd filename = Common.impureExCommand {
    cmdShow = "write " <> filename
  , cmdAction = YiA $ viWriteTo filename
  }

tryWriteBuffer :: BufferRef -> YiM ()
tryWriteBuffer buf = do
    ns <- Common.needsSaving buf
    when ns . void $ fwriteBufferE buf
