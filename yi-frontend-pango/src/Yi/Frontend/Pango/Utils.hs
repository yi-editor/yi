{-# LANGUAGE OverloadedStrings #-}

-- | Random GTK utils
module Yi.UI.Pango.Utils where

import Control.Exception (catch, throw)

import Data.Text (append)
import Paths_yi
import System.FilePath
import Graphics.UI.Gtk
import System.Glib.GError

loadIcon :: FilePath -> IO Pixbuf
loadIcon fpath = do
  iconfile <- getDataFileName $ "art" </> fpath
  icoProject <-
    catch (pixbufNewFromFile iconfile)
    (\(GError dom code msg) ->
      throw $ GError dom code $
        msg `append` " -- use the yi_datadir environment variable to"
            `append` " specify an alternate location")
  pixbufAddAlpha icoProject (Just (0,255,0))
