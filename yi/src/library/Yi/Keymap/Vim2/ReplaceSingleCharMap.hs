module Yi.Keymap.Vim2.ReplaceSingleCharMap
    ( defReplaceSingleMap
    ) where

import Yi.Buffer
import Yi.Editor
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Utils

defReplaceSingleMap :: [VimBinding]
defReplaceSingleMap = [escBinding, printable]

escBinding :: VimBinding
escBinding = mkBindingE ReplaceSingleChar (spec KEsc, return (), switchMode Normal)

printable :: VimBinding
printable = VimBindingE prereq action
    where prereq _ s = ReplaceSingleChar == vsMode s
          action (Event (KASCII c) []) = do
              withBuffer0 $ replaceCharB c
              currentState <- getDynamic 
              setDynamic $ currentState { vsMode = Normal }
          action _ = return ()
