--
-- Copyright (c) Krasimir Angelov 2008.
--
-- Random GTK utils
--

module Yi.UI.Gtk.Utils where

import Paths_yi
import System.FilePath
import Graphics.UI.Gtk

loadIcon :: FilePath -> IO Pixbuf
loadIcon fpath = do
  datadir <- getDataDir
  icoProject <- pixbufNewFromFile (datadir </> "art" </> fpath)
  pixbufAddAlpha icoProject (Just (0,255,0))
