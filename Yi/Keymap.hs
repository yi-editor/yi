module Yi.Keymap ( module Yi.Editor, write ) where

import Yi.Event
import Yi.Editor ( Keymap, Action, EditorM, Interact )
import qualified Yi.Interact as I
import Control.Monad.Writer

write x = I.write (tell [x])
