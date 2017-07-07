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
import           Control.Monad                    (void, when)
import qualified Data.Text                        as T (null)
import qualified Data.Attoparsec.Text             as P (Parser, char, choice, digit, endOfInput, parseOnly, string)
import           Lens.Micro.Platform              (use)
import           Yi.Buffer.Basic                  (BufferRef (..))
import           Yi.Core                          (closeWindow, errorEditor)
import           Yi.Editor                        (currentWindowA, deleteBuffer, getBufferWithName, withEditor)
import           Yi.Keymap                        (Action (YiA))
import           Yi.Keymap.Vim.Common             (EventString)
import           Yi.Keymap.Vim.Ex.Commands.Buffer (bufferIdentifier)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (needsSaving, parse, impureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.Window                        (bufkey)

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    nameParser
    bang <- (True <$ P.char '!') <|> pure False
    ((void $ some (P.char ' ')) <|> P.endOfInput)
    bufIdent <- bufferIdentifier
    return $ Common.impureExCommand {
        cmdShow = "bdelete"
      , cmdAction = YiA $ do
            buffer <- case P.parseOnly bufferRef bufIdent of
                _ | T.null bufIdent -> withEditor $ bufkey <$> use currentWindowA
                Right ref -> return ref
                Left _ -> getBufferWithName bufIdent
            q <- if bang then pure True else not <$> Common.needsSaving buffer
            if q
                then do
                    deleteBuffer buffer
                    when (T.null bufIdent) $ closeWindow -- Because this function closed the window before I started altering it
                else errorEditor "No write since last change (add ! to override)"
      }
  where
    bufferRef = BufferRef . read <$> some P.digit

nameParser :: P.Parser ()
nameParser = void . P.choice . fmap P.string $ ["bdelete","bdel","bd"]
