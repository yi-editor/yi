module Yi.CommonUI where

import Yi.Buffer
import Yi.Editor

-- | A window onto a buffer.

data Window = Window {
                      isMini :: !Bool   -- ^ regular or mini window?
                     ,bufkey :: !BufferRef -- ^ the buffer this window opens to
                     ,tospnt :: !Int    -- ^ the buffer point of the top of screen
                     ,bospnt :: !Int    -- ^ the buffer point of the bottom of screen
                     ,height :: !Int    -- ^ height of the window (in number of lines displayed)
                     }

-- | Get the identification of a window.
winkey :: Window -> (Bool, BufferRef)
winkey w = (isMini w, bufkey w)

instance Show Window where
    show Window { bufkey = u } = "Window to " ++ show u

pointInWindow :: Point -> Window -> Bool
pointInWindow point win = tospnt win <= point && point <= bospnt win

data UI = UI
    {
     main                  :: IO (),           -- ^ Main loop
     end                   :: IO (),           -- ^ Clean up
     suspend               :: IO (),           -- ^ Suspend the program
     scheduleRefresh       :: Editor -> IO (), -- ^ Schedule a full refresh of the with the given state.
     prepareAction         :: IO (EditorM ())  -- ^ Ran before an action is executed
    }

