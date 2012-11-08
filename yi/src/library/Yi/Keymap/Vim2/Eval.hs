module Yi.Keymap.Vim2.Eval
    ( vimEval ) where

import Prelude ()
import Yi.Prelude

import Yi.Editor

vimEval :: String -> EditorM ()
vimEval = const $ return ()