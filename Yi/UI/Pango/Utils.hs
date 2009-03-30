--
-- Copyright (c) Krasimir Angelov 2008.
--
-- Random GTK utils
--

module Yi.UI.Pango.Utils where

import Paths_yi
import System.FilePath
import Graphics.UI.Gtk
import System.Glib.GError

loadIcon :: FilePath -> IO Pixbuf
loadIcon fpath = do
  iconfile <- getDataFileName $ "art" </> fpath
  icoProject <- catchGError (pixbufNewFromFile iconfile)
                            (\(GError dom code msg) -> throwGError $ GError dom code $
                             msg ++ " -- use the yi_datadir environment variable to specify an alternate location")
  pixbufAddAlpha icoProject (Just (0,255,0))
