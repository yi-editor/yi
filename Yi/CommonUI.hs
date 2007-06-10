module Yi.CommonUI where

import Control.Monad.Trans
import Data.IORef
import Yi.Buffer
import Yi.Editor
import Yi.Window as Window
import Yi.WindowSet

data UI = UI
    {
     -- UI initialisation
     main                  :: IORef Editor -> IO (),                 -- ^ Main loop
     end                   :: IO (),                                 -- ^ Clean up
     suspend               :: EditorM (),                            -- ^ Suspend the program


     -- Refresh/Sync
     refreshAll            :: EditorM (),                            -- ^ Reset the heights and widths of all the windows; 
                                                                     -- refresh the display.

     scheduleRefresh       :: EditorM (),                            -- ^ Schedule a refresh of the UI.
     prepareAction         :: EditorM (),                            -- ^ Ran before an action is executed


     -- Window manipulation
     newWindow             :: Bool -> FBuffer -> EditorM Window,     -- ^ Create a new window onto this buffer.
     enlargeWindow         :: Window -> EditorM (),
     shrinkWindow          :: Window -> EditorM (),
     deleteWindow          :: Window -> EditorM (),                  -- ^ Delete a window. 
                                                                     --   Note that the buffer that was connected to this window is
                                                                     --   still open.
     hasRoomForExtraWindow :: EditorM Bool,                          -- ^ Has the display enough room for an extra window.
     setWindowBuffer       :: FBuffer -> Window -> EditorM (),       -- ^ Display the given buffer in the given window.
     setWindow             :: Window -> EditorM (),                  -- ^ set the focused window

     withWindow0           :: forall m a. MonadIO m => (Window -> a) -> m a,
     getWindows            :: forall m. MonadIO m => m (WindowSet Window),
     getWindow             :: forall m. MonadIO m => m Window,

     -- Command line
     setCmdLine            :: String -> IO ()
    }

