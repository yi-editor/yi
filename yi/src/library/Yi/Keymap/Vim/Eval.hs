module Yi.Keymap.Vim.Eval
    ( scheduleActionStringForEval
    ) where

-- This module doesn't contains actual eval, see Yi.Keymap.Vim.vimEval comment

import Yi.Editor
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.StateUtils

scheduleActionStringForEval :: String -> EditorM ()
scheduleActionStringForEval s = modifyStateE $
    \state -> state { vsStringToEval = s }
