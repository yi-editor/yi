module Yi.Keymap.Vim2.InsertMap
  ( defInsertMap
  ) where

import Yi.Prelude
import Prelude ()

import Yi.Buffer hiding (Insert)
import Yi.Buffer.Misc
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Utils

defInsertMap :: [VimBinding]
defInsertMap = specials ++ [printable]

specials = fmap (mkBindingE Insert)
             [ (spec KEsc, vimMoveE (VMChar Backward), switchMode Normal)
             ]

printable :: VimBinding
printable = VimBindingE prereq action
    where prereq _ s = Insert == vsMode s
          action (Event (KASCII c) []) = do
              withBuffer0 $ insertB c
          action _ = return ()
