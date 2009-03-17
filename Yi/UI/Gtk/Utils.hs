--
-- Copyright (c) Krasimir Angelov 2008.
--
-- Random GTK utils
--

module Yi.UI.Gtk.Utils where

import Paths_yi
import System.FilePath
import Graphics.UI.Gtk
import System.Glib.GError

loadIcon :: FilePath -> IO Pixbuf
loadIcon fpath = do
  datadir <- getDataDir
  icoProject <- catchGError (pixbufNewFromFile (datadir </> "art" </> fpath))
                            (\(GError dom code msg) -> throwGError $ GError dom code $
                             msg ++ " -- use the yi_datadir environment variable to specify an alternate location")
  pixbufAddAlpha icoProject (Just (0,255,0))
