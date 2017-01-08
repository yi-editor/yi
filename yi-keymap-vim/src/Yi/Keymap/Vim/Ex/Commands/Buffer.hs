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

import           Control.Applicative              (Alternative ((<|>)))
import           Control.Monad                    (void)
import           Control.Monad.State              (gets)
import qualified Data.Attoparsec.Text             as P (Parser, anyChar, choice,
                                                        digit, endOfInput, many', many1,
                                                        parseOnly, space, string)
import           Yi.Buffer.Basic                  (BufferRef (..))
import qualified Data.Text                        as T (Text, pack, unpack)
import           Yi.Buffer.Misc                   (bkey, isUnchangedBuffer)
import           Yi.Editor
import           Yi.Keymap                        (Action (EditorA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (errorNoWrite, parseWithBangAndCount, pureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))

parse :: EventString -> Maybe ExCommand
parse = Common.parseWithBangAndCount nameParser $ \ _ bang mcount -> do
  bufIdent <- (T.pack <$> P.many1 P.digit) <|> bufferSymbol <|>
              (T.pack <$> P.many1 P.space) *> (T.pack <$> P.many' P.anyChar) <|>
              P.endOfInput *> return ""
  return $ Common.pureExCommand {
      cmdShow = "buffer"
    , cmdAction = EditorA $ do
        unchanged <- withCurrentBuffer $ gets isUnchangedBuffer
        if bang || unchanged
          then case mcount of
            Nothing -> switchToBuffer bufIdent
            Just i  -> switchByRef $ BufferRef i
          else Common.errorNoWrite
    }
  where
    bufferSymbol = P.string "%" <|> P.string "#"


nameParser :: P.Parser ()
nameParser = void . P.choice . fmap P.string $ ["buffer", "buf", "bu", "b"]


switchToBuffer :: T.Text -> EditorM ()
switchToBuffer s =
    case P.parseOnly bufferRef s of
        Right ref -> switchByRef ref
        Left _e   -> switchByName $ T.unpack s
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
