module Yi.Keymap.Vim2.InsertMap
  ( defInsertMap
  ) where

import Yi.Prelude
import Prelude ()

import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Utils

defInsertMap :: [VimBinding]
defInsertMap = fmap (mkBinding Insert)
                 [ (spec KEsc, return (), switchMode Normal)
                 ]
