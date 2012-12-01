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
import Yi.Keymap.Vim2.StateUtils

defInsertMap :: [VimBinding]
defInsertMap = specials ++ [printable]

specials :: [VimBinding]
specials = fmap (mkBindingE Insert Finish)
             [ (spec KEsc, withBuffer0 $ moveXorSol 1, switchMode Normal)
             , (ctrlCh 'c',withBuffer0 $ moveXorSol 1, switchMode Normal)
             ]

printable :: VimBinding
printable = VimBindingE prereq action
    where prereq _ s = Insert == vsMode s
          action (Event (KASCII c) []) = do
              withBuffer0 $ insertB c
              return Continue
          action (Event KEnter []) = do
              withBuffer0 $ insertB '\n'
              return Continue
          action (Event KTab []) = do
              -- For testing purposes assume noexpandtab, tw=4
              withBuffer0 $ insertN $ replicate 4 ' '
              return Continue
          action _ = return Drop
