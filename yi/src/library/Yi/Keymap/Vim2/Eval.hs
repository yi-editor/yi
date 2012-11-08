module Yi.Keymap.Vim2.Eval
    ( scheduleActionStringForEval
    ) where

-- This module doesn't contains actual eval, see Yi.Keymap.Vim2.vimEval comment

import Yi.Prelude
import Prelude ()

import Yi.Editor
import Yi.Keymap.Vim2.Common

scheduleActionStringForEval :: String -> EditorM ()
scheduleActionStringForEval s = do
        state <- getDynamic
        setDynamic $ state { vsStringToEval = s }
