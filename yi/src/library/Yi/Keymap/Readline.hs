module Yi.Keymap.Readline
    ( standardMovementBindings
    ) where

import Yi.Buffer
import Yi.Keymap
import Yi.Keymap.Keys

-- | Readline-like movement bindings intended for minibuffer keymaps
standardMovementBindings :: Keymap
standardMovementBindings =
    choice [ ctrlCh 'b' ?>>! moveXorSol 1
           , ctrlCh 'f' ?>>! moveXorEol 1
           , metaCh 'b' ?>>! moveB unitWord Backward
           , metaCh 'f' ?>>! moveB unitWord Forward
           , spec KLeft  ?>>! moveXorSol 1
           , spec KRight ?>>! moveXorEol 1
           , ctrlCh 'a'  ?>>! moveToSol
           , ctrlCh 'e'  ?>>! moveToEol
           ]

