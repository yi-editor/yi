{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.BufferDelete
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.BufferDelete (parse) where

import           Control.Applicative              (Alternative ((<|>),some))
import           Control.Monad                    (void)
import           Control.Monad.State              (gets)
import qualified Data.Text                        as T (null)
import qualified Data.Attoparsec.Text             as P (Parser, char, choice, digit, endOfInput, parseOnly, string, try)
import           Yi.Buffer.Basic                  (BufferRef (..))
import           Yi.Editor                        (closeBufferAndWindowE,deleteBuffer,getBufferWithName)
import           Yi.Keymap                        (Action (EditorA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, pureExCommand)
import           Yi.Keymap.Vim.Ex.Commands.Buffer (bufferIdentifier)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    nameParser <* ((void $ some (P.char ' ')) <|> P.endOfInput)
    bufIdent <- bufferIdentifier
    if T.null bufIdent
        then return $ Common.pureExCommand {
            cmdShow = "bdelete"
          , cmdAction = EditorA closeBufferAndWindowE
          }
        -- If a different buffer is specified: the user probably doesn't
        -- want to close the current window. Also keep the current window
        -- open if the current buffer is specified because otherwise there's
        -- no way to do it.
        else return $ Common.pureExCommand {
            cmdShow = "bdelete"
          , cmdAction = EditorA $ do
                buffer <- case P.parseOnly bufferRef bufIdent of
                    Right ref -> return ref
                    Left _ -> getBufferWithName bufIdent
                deleteBuffer buffer
          }
  where
    bufferRef = BufferRef . read <$> some P.digit

nameParser :: P.Parser ()
nameParser = void . P.choice . fmap P.string $ ["bdelete","bdel","bd"]
