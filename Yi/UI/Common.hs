{-# LANGUAGE Rank2Types #-}

module Yi.UI.Common where

import Yi.Editor

data UI = UI
    {
     main                  :: IO (),             -- ^ Main loop
     end                   :: IO (),             -- ^ Clean up
     suspend               :: IO (),             -- ^ Suspend (or minimize) the program
     refresh               :: Editor -> IO (),   -- ^ Refresh the UI with the given state
     userForceRefresh      :: IO (),             -- ^ User force-refresh (in case the screen has been messed up from outside)
     prepareAction         :: IO (EditorM ()),   -- ^ Ran before an action is executed
     reloadProject         :: FilePath -> IO ()  -- ^ Reload cabal project views
    }

dummyUI :: UI
dummyUI = UI
  {
    main             = return (),
    end              = return (),
    suspend          = return (),
    refresh          = const (return ()),
    userForceRefresh = return (),
    prepareAction    = return (return ()),
    reloadProject    = const (return ())
  }
