-- | Gnome integration
module Yi.UI.Pango.Gnome(watchSystemFont) where

import Control.Monad
import Graphics.UI.Gtk
import System.Gnome.GConf

monospaceFontKey :: String
monospaceFontKey = "/desktop/gnome/interface/monospace_font_name"

watchSystemFont :: (FontDescription -> IO ()) -> IO ()
watchSystemFont cb = do
  gconf <- gconfGetDefault
  gconfAddDir gconf "/desktop/gnome/interface"
  watch gconf monospaceFontKey (cb <=< fontDescriptionFromString)

watch :: GConfValueClass value => GConf -> String -> (value -> IO ()) -> IO ()
watch gconf key cb = do
  cb =<< gconfGet gconf key
  gconfNotifyAdd gconf key $ \key' val -> when (key == key') (cb val)
  return ()
