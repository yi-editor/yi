{-# LANGUAGE OverloadedStrings, UnboxedTuples #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.BufferDelete
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.BufferDelete (parse) where

import           Control.Applicative              (Alternative (some))
import           Control.Monad                    (void, when)
import qualified Data.Text                        as T (null)
import qualified Data.Attoparsec.Text             as P (Parser, choice, digit, parseOnly, string)
import           Lens.Micro.Platform              (use)
import           Yi.Buffer.Basic                  (BufferRef (..))
import           Yi.Core                          (closeWindow, errorEditor)
import           Yi.Editor                        (currentWindowA, deleteBuffer, getBufferWithName, withEditor)
import           Yi.Keymap                        (Action (YiA))
import           Yi.Keymap.Vim.Common             (EventString)
import           Yi.Keymap.Vim.Ex.Commands.Buffer (bufferIdentifier)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (needsSaving, parseWithBangAndCount, impureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.Window                        (bufkey)

parse :: EventString -> Maybe ExCommand
parse = Common.parseWithBangAndCount nameParser $ \ _ bang mcount -> do
    bufIdent <- bufferIdentifier
    return $ Common.impureExCommand {
        cmdShow = "bdelete"
      , cmdAction = YiA $ do
            buffer <- case (# mcount, P.parseOnly bufferRef bufIdent #) of
                (# Just i, _ #) -> return $ BufferRef i
                _ | T.null bufIdent -> withEditor $ bufkey <$> use currentWindowA
                (# _, Right ref #) -> return ref
                (# _, Left _ #) -> getBufferWithName bufIdent
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
