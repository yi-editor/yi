{-# LANGUAGE Rank2Types #-}

module Yi.UI.Common where

import Yi.Editor

data UI = UI
    {
     main                  :: IO (),             -- ^ Main loop
     end                   :: Bool -> IO (),     -- ^ Clean up, and also terminate if given 'true'
     suspend               :: IO (),             -- ^ Suspend (or minimize) the program
     refresh               :: Editor -> IO Editor,   -- ^ Refresh the UI with the given state, return the new editor state.
     userForceRefresh      :: IO (),             -- ^ User force-refresh (in case the screen has been messed up from outside)
     prepareAction         :: IO (EditorM ()),   -- ^ Ran before an action is executed
     reloadProject         :: FilePath -> IO ()  -- ^ Reload cabal project views
    }

dummyUI :: UI
dummyUI = UI
  {
    main             = return (),
    end              = const (return ()),
    suspend          = return (),
    refresh          = return,
    userForceRefresh = return (),
    prepareAction    = return (return ()),
    reloadProject    = const (return ())
  }
