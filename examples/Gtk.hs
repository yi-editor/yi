module Gtk where

import Yi.Yi
import Yi.Kernel
import Yi.Editor
import Data.Char
import Yi.Keymap.Emacs
import Data.List
import Yi.Buffer
import Control.Monad.Trans
import System.FilePath
import System.Directory
import Yi.UI
import Graphics.UI.Gtk

yiConfig = do
    window <- withUI (return . uiWindow)
    lift $ windowResize window 640 480 

  