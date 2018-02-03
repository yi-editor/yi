{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Number
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Toggles line numbers.

module Yi.Keymap.Vim.Ex.Commands.Number (parse) where

import qualified Data.Attoparsec.Text             as P (string)
import           Data.Monoid                      ((<>))
import           Yi.Buffer                        (getBufferDyn, putBufferDyn)
import           Yi.Editor                        (getEditorDyn, printMsg, putEditorDyn, withCurrentBuffer)
import           Yi.Keymap                        (Action (BufferA, EditorA))
import           Yi.Keymap.Vim.Common             (EventString)
import           Yi.Keymap.Vim.Ex.Commands.Common (BoolOptionAction (..), parseBoolOption, pureExCommand)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Ex (parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (..), evStringToExCommand)
import           Yi.String                        (showT)
import           Yi.UI.LineNumbers                (DisplayLineNumbers (..), DisplayLineNumbersLocal (..))

-- | Defines the following commands:
-- - :set [no]number        (toggle buffer-local line numbers)
-- - :set [no]globalnumber  (toggle global line numbers)
-- - :unset number          (make the current buffer use the global setting)
parse :: EventString -> Maybe ExCommand
parse = evStringToExCommand
  [ parseBoolOption "number" boolLocal
  , parseBoolOption "globalnumber" boolGlobal
  , parseUnset
  ]

boolLocal :: BoolOptionAction -> Action
boolLocal BoolOptionAsk = EditorA $ do
  mb <- withCurrentBuffer (getDisplayLineNumbersLocal <$> getBufferDyn)
  printMsg $ "number = " <> case mb of
    Nothing -> "<unset>"
    Just b  -> showT b
boolLocal (BoolOptionSet b) = BufferA $
  putBufferDyn (DisplayLineNumbersLocal (Just b))
boolLocal BoolOptionInvert = BufferA $ do
  b <- getDisplayLineNumbersLocal <$> getBufferDyn
  putBufferDyn (DisplayLineNumbersLocal (fmap not b))

boolGlobal :: BoolOptionAction -> Action
boolGlobal BoolOptionAsk = EditorA $ do
  b <- getDisplayLineNumbers <$> getEditorDyn
  printMsg $ "globalnumber = " <> showT b
boolGlobal (BoolOptionSet b) = EditorA $
  putEditorDyn (DisplayLineNumbers b)
boolGlobal BoolOptionInvert = EditorA $ do
  b <- getDisplayLineNumbers <$> getEditorDyn
  putEditorDyn (DisplayLineNumbers (not b))

parseUnset :: EventString -> Maybe ExCommand
parseUnset = Ex.parse $ do
  _ <- P.string "unset number"
  return $ pureExCommand
    { cmdShow = "unset number"
    , cmdAction = BufferA $ putBufferDyn (DisplayLineNumbersLocal Nothing)
    }
