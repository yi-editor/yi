module Yi.UI.Gtk.Utils where

import Paths_yi
import System.FilePath
import Graphics.UI.Gtk

loadIcon fpath = do
  datadir <- getDataDir
  icoProject <- pixbufNewFromFile (datadir </> "art" </> fpath)
  pixbufAddAlpha icoProject (Just (0,255,0))
