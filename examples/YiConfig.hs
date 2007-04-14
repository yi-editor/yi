-- This is jyp's YiConfig.

-- Don't run with --as=..., that will overwrite the keymap set here.

module YiConfig (yiMain) where

import Yi.Yi  
import Yi.Editor
import Yi.Keymap.Emacs
import Control.Monad.Trans
import Yi.Buffer

yiMain :: EditorM ()
yiMain = do
  changeKeymapE myKeymap  

  -- The following will /dynamically/ fail with the vty frontend,
  -- or if the Gtk module cannot be found in yiConfig.
  loadE "Gtk" >> execE "Gtk.yiConfig"
  -- However, the rest will continue running:

  msgE "User configuration successful."


-- I prefer to use Emacs keymap, with haskell hilight in every buffer.
myKeymap = do write $ setSynE "haskell" 
              keymap

