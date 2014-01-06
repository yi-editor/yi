module Yi.Keymap.Vim2.Ex.Commands.Paste
    ( parse
    ) where

import Control.Applicative

import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.Ex.Commands.Common hiding (parse)

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
