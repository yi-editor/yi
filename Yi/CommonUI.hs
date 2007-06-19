module Yi.CommonUI where

import Control.Monad.Trans
import Data.IORef
import Yi.Buffer
import Yi.Editor
import Yi.WindowSet
import Data.Unique

-- | A window onto a buffer.

data Window = Window {
                      isMini :: !Bool   -- ^ regular or mini window?
                     ,bufkey :: !Unique -- ^ the buffer this window opens to
                     ,tospnt :: !Int    -- ^ the buffer point of the top of screen
                     ,bospnt :: !Int    -- ^ the buffer point of the bottom of screen
                     ,height :: !Int    -- ^ height of the window (in number of lines displayed)
                     }

winkey w = (isMini w, bufkey w)

instance Show Window where
    show Window { bufkey = u } = "Window to " ++ show (hashUnique u)

pointInWindow :: Point -> Window -> Bool
pointInWindow point win = tospnt win <= point && point <= bospnt win

data UI = UI
    {
     -- UI initialisation
     main                  :: IORef Editor -> IO (),                 -- ^ Main loop
     end                   :: IO (),                                 -- ^ Clean up
     suspend               :: EditorM (),                            -- ^ Suspend the program


     -- Refresh/Sync
     refreshAll            :: EditorM (),                            -- ^ Reset the heights and widths of all the windows; -- refresh the display.
        -- FIXME: remove one of those.
        

     scheduleRefresh       :: EditorM (),                            -- ^ Schedule a refresh of the UI.
     prepareAction         :: EditorM (),                            -- ^ Ran before an action is executed



     -- Command line
     setCmdLine            :: String -> IO ()
    }

