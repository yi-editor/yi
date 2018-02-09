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
import           Yi.Editor                        (printMsg, withCurrentBuffer)
import           Yi.Keymap                        (Action (BufferA, EditorA))
import           Yi.Keymap.Vim.Common             (EventString)
import           Yi.Keymap.Vim.Ex.Commands.Common (BoolOptionAction (..), parseBoolOption, pureExCommand)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Ex (parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (..), evStringToExCommand)
import           Yi.String                        (showT)
import           Yi.UI.LineNumbers                (getDisplayLineNumbersLocal, setDisplayLineNumbersLocal)

-- | Defines the following commands:
-- - :set [no]number        (toggle buffer-local line numbers)
-- - :unset number          (make the current buffer use the global setting)
parse :: EventString -> Maybe ExCommand
parse = evStringToExCommand
  [ parseBoolOption "number" actionSet
  , parseUnset
  ]

actionSet :: BoolOptionAction -> Action
actionSet BoolOptionAsk = EditorA $ do
  mb <- withCurrentBuffer getDisplayLineNumbersLocal
  printMsg $ "number = " <> case mb of
    Nothing -> "<unset>"
    Just b  -> showT b
actionSet (BoolOptionSet b) = BufferA $ setDisplayLineNumbersLocal (Just b)
actionSet BoolOptionInvert = BufferA $ do
  b <- getDisplayLineNumbersLocal
  setDisplayLineNumbersLocal (fmap not b)

parseUnset :: EventString -> Maybe ExCommand
parseUnset = Ex.parse $ do
  _ <- P.string "unset number"
  return $ pureExCommand
    { cmdShow = "unset number"
    , cmdAction = BufferA $ setDisplayLineNumbersLocal Nothing
    }
