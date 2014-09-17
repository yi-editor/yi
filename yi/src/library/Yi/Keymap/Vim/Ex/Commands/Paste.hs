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

import Control.Applicative
import Data.Monoid
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.Ex.Commands.Common hiding (parse)
import Yi.Keymap.Vim.Ex.Types
import Yi.Keymap.Vim.StateUtils
import Yi.String (showT)

parse :: EventString -> Maybe ExCommand
parse = parseOption "paste" action

action :: OptionAction -> Action
action Ask = EditorA $ do
    value <- vsPaste <$> getDynamic
    printMsg $ "paste = " <> showT value
action (Set b) = modPaste $ const b
action Invert = modPaste not

modPaste :: (Bool -> Bool) -> Action
modPaste f = EditorA . modifyStateE $ \s -> s { vsPaste = f (vsPaste s) }
