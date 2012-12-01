module Yi.Keymap.Vim2.ReplaceMap
    ( defReplaceMap
    ) where

import Control.Monad (replicateM_)

import Yi.Buffer
import Yi.Editor
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.Utils

defReplaceMap :: [VimBinding]
defReplaceMap = [escBinding, printable]

escBinding :: VimBinding
escBinding = mkBindingE Replace Finish (spec KEsc,
                                        withBuffer0 $ moveXorSol 1,
                                        resetCount . switchMode Normal)

printable :: VimBinding
printable = VimBindingE prereq action
    where prereq _ s = Replace == vsMode s
          action (Event (KASCII c) []) = withBuffer0 $ do
              insertOrReplaceB c
              return Continue
          action (Event KEnter []) = do
              withBuffer0 $ insertOrReplaceB '\n'
              return Continue
          action (Event KTab []) = do
              -- For testing purposes assume noexpandtab, tw=4
              withBuffer0 $ replicateM_ 4 $ insertOrReplaceB ' '
              return Continue
          action _ = return Drop

insertOrReplaceB :: Char -> BufferM ()
insertOrReplaceB c = do
    currentChar <- readB
    if currentChar == '\n'
    then insertB c
    else replaceCharB c
    rightB
