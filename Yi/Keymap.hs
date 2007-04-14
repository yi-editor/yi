module Yi.Keymap ( module Yi.Editor, Interact, runProcess, write ) where

import Yi.Event
import Yi.Editor ( Keymap, Action, EditorM )
import qualified Yi.Interact as I
import Control.Monad.Writer

type Interact ev a = I.Interact ev (Writer [Action]) a

runProcess :: Interact ev () -> [ev] -> [Action]
runProcess p evs = snd $ runWriter (I.runProcess p evs)

write x = I.write (tell [x])