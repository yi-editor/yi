{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Buffer
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- :buffer ex command to switch to named or numbered buffer.

module Yi.Keymap.Vim.Ex.Commands.Buffer (parse) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Buffer.Basic
import           Yi.Buffer.Misc
import           Yi.Editor
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types

parse :: EventString -> Maybe ExCommand
parse = Common.parseWithBangAndCount nameParser $ \ _ bang mcount -> do
  bufIdent <- P.try ( P.many1 P.digit <|> bufferSymbol) <|>
              P.many1 P.space *> P.many P.anyChar <|>
              P.eof *> return ""
  return $ Common.pureExCommand {
      cmdShow = "buffer"
    , cmdAction = EditorA $ do
        unchanged <- withBuffer0 $ gets isUnchangedBuffer
        if bang || unchanged
          then case mcount of
            Nothing -> switchToBuffer bufIdent
            Just i  -> switchByRef $ BufferRef i
          else Common.errorNoWrite
    }
  where
    bufferSymbol = P.string "%" <|> P.string "#"


nameParser :: P.GenParser Char () ()
nameParser = do
    void $ P.try ( P.string "buffer") <|>
           P.try ( P.string "buf")    <|>
           P.try ( P.string "bu")     <|>
           P.try ( P.string "b")


switchToBuffer :: String -> EditorM ()
switchToBuffer s =
    case P.parse bufferRef "" s of
        Right ref -> switchByRef ref
        Left _e   -> switchByName s
  where
    bufferRef = BufferRef . read <$> P.many1 P.digit


switchByName :: String -> EditorM ()
switchByName ""      = return ()
switchByName "%"     = return ()
switchByName "#"     = switchToBufferWithNameE ""
switchByName bufName = switchToBufferWithNameE (T.pack bufName)


switchByRef :: BufferRef -> EditorM ()
switchByRef ref = do
    mBuf <- findBuffer ref
    maybe (return ()) (switchToBufferE . bkey) mBuf
