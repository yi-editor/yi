{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Paste
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implements quit commands.

module Yi.Keymap.Vim.Ex.Commands.Paste (parse) where

import           Control.Applicative              ((<$>))
import           Data.Monoid                      ((<>))
import           Yi.Editor                        (getEditorDyn, printMsg)
import           Yi.Keymap                        (Action (EditorA))
import           Yi.Keymap.Vim.Common             (EventString, VimState (vsPaste))
import           Yi.Keymap.Vim.Ex.Commands.Common (BoolOptionAction (..), parseBoolOption)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand)
import           Yi.Keymap.Vim.StateUtils         (modifyStateE)
import           Yi.String                        (showT)

parse :: EventString -> Maybe ExCommand
parse = parseBoolOption "paste" action

action :: BoolOptionAction -> Action
action BoolOptionAsk = EditorA $ do
    value <- vsPaste <$> getEditorDyn
    printMsg $ "paste = " <> showT value
action (BoolOptionSet b) = modPaste $ const b
action BoolOptionInvert = modPaste not

modPaste :: (Bool -> Bool) -> Action
modPaste f = EditorA . modifyStateE $ \s -> s { vsPaste = f (vsPaste s) }
