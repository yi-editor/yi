module Yi.Keymap.Vim2.InsertMap
  ( defInsertMap
  ) where

import Yi.Prelude
import Prelude ()

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Utils

defInsertMap :: [VimBinding]
defInsertMap = specials ++ [printable]

specials :: [VimBinding]
specials = fmap (mkBindingE Insert Finish)
             [ (spec KEsc, vimMoveE (VMChar Backward), switchMode Normal)
             , (ctrlCh 'c', vimMoveE (VMChar Backward), switchMode Normal)
             ]

printable :: VimBinding
printable = VimBindingE prereq action
    where prereq _ s = Insert == vsMode s
          action (Event (KASCII c) []) = do
              withBuffer0 $ insertB c
              return Continue
          action _ = return Drop
