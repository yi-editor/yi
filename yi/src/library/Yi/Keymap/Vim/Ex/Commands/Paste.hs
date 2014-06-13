module Yi.Keymap.Vim.Ex.Commands.Paste
    ( parse
    ) where

import Control.Applicative

import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.Ex.Types
import Yi.Keymap.Vim.StateUtils
import Yi.Keymap.Vim.Ex.Commands.Common hiding (parse)

parse :: String -> Maybe ExCommand
parse = parseOption "paste" action

action :: OptionAction -> Action
action Ask = EditorA $ do
    value <- vsPaste <$> getDynamic
    printMsg $ "paste = " ++ show value
action (Set b) = modPaste $ const b
action Invert = modPaste not

modPaste :: (Bool -> Bool) -> Action
modPaste f = EditorA . modifyStateE $ \s -> s { vsPaste = f (vsPaste s) }
