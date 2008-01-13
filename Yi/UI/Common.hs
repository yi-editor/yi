module Yi.UI.Common where

import Yi.Editor
import Yi.Event
import Control.Concurrent.Chan
import Yi.WindowSet

data UI = UI
    {
     main                  :: IO (),           -- ^ Main loop
     end                   :: IO (),           -- ^ Clean up
     suspend               :: IO (),           -- ^ Suspend (or minimize) the program
     refresh               :: Editor -> IO (WindowSet Window), -- ^ a full refresh of the with the given state, and returns the updated windowset.
     prepareAction         :: IO (EditorM ())  -- ^ Ran before an action is executed
    }

type UIBoot = forall action. (Chan Event -> Chan action ->  Editor -> (EditorM () -> action) -> IO UI)
