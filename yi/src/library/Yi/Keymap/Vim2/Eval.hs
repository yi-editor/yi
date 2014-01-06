module Yi.Keymap.Vim2.Eval
    ( scheduleActionStringForEval
    ) where

-- This module doesn't contains actual eval, see Yi.Keymap.Vim2.vimEval comment

import Yi.Editor
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StateUtils

scheduleActionStringForEval :: String -> EditorM ()
scheduleActionStringForEval s = modifyStateE $
    \state -> state { vsStringToEval = s }
