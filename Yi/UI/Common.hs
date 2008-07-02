{-# LANGUAGE Rank2Types #-}

module Yi.UI.Common where

import Yi.Editor
import Yi.Event
import {-# SOURCE #-} Yi.Keymap
import Yi.Style


data UI = UI
    {
     main                  :: IO (),             -- ^ Main loop
     end                   :: IO (),             -- ^ Clean up
     suspend               :: IO (),             -- ^ Suspend (or minimize) the program
     refresh               :: Editor -> IO (),   -- ^ Refresh the UI with the given state
     prepareAction         :: IO (EditorM ()),   -- ^ Ran before an action is executed
     reloadProject         :: FilePath -> IO ()  -- ^ Reload cabal project views
    }


