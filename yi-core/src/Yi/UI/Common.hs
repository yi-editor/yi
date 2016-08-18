{-# LANGUAGE Rank2Types #-}

module Yi.UI.Common where

{- | Record presenting a frontend's interface.

The functions 'layout' and 'refresh' are both run by the editor's main loop,
in response to user actions and so on. Their relation is a little subtle, and
is discussed here:

  * to see some code, look at the function @refreshEditor@ in "Yi.Core".
    This is the only place where 'layout' and 'refresh' are used.

  * the function 'layout' is responsible for updating the 'Editor' with the
    width and height of the windows. Some frontends, such as Pango, need to
    modify their internal state to do this, and will consequently change
    their display. This is expected.

  * the function 'refresh' should cause the UI to update its display with
    the information given in the 'Editor'.

  * the functionalities of 'layout' and 'refresh' overlap to some extent, in
    the sense that both may cause the frontend to update its display. The Yi core
    provides the following guarantees which the frontend may take advantage of:

      * in the main editor loop (i.e. in the @refreshEditor@ function),
        'layout' will be run (possibly multiple times) and then 'refresh' will
        be run. This guarantee will hold even in the case of threading (the
        function @refreshEditor@ will always be run atomically, using @MVar@s).

      * between the last run of 'layout' and the run of 'refresh', some changes
        may be made to the 'Editor'. However, the text, text attributes, and
        (displayed) window region of all windows will remain the same. However,
        the cursor location may change.

        This guarantee allows frontends which calculate rendering of the text
        during the 'layout' stage to avoid recalculating the render again during
        'refresh'. Pango is an example of such a frontend.

The Yi core provides no guarantee about the OS thread from which the functions
'layout' and 'refresh' are called from. In particular, subprocesses (e.g. compilation,
ghci) will run 'layout' and 'refresh' from new OS threads (see @startSubprocessWatchers@
in "Yi.Core"). The frontend must be preparaed for this: for instance, Gtk-based frontends
should wrap GUI updates in @postGUIAsync@.
-}
data UI e = UI
    { main                  :: IO ()               -- ^ Main loop
    , end                   :: Bool -> IO ()       -- ^ Clean up, and also terminate if given 'true'
    , suspend               :: IO ()               -- ^ Suspend (or minimize) the program
    , refresh               :: e -> IO ()     -- ^ Refresh the UI with the given state
    , userForceRefresh      :: IO ()               -- ^ User force-refresh (in case the screen has been messed up from outside)
    , layout                :: e -> IO e -- ^ Set window width and height
    , reloadProject         :: FilePath -> IO ()   -- ^ Reload cabal project views
    }

dummyUI :: UI e
dummyUI = UI
  { main             = return ()
  , end              = const (return ())
  , suspend          = return ()
  , refresh          = const (return ())
  , userForceRefresh = return ()
  , layout           = return
  , reloadProject    = const (return ())
  }
