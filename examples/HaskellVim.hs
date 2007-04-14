module YiConfig ( yiMain ) where

import Yi.Yi
import Yi.Editor
import Yi.Keymap.Vim
import Control.Monad.Trans

yiMain :: EditorM ()
yiMain = do
  changeKeymapE myKeymap
  msgE "User configuration finished."


myKeymap = do
  write $ setSynE "haskell" 
  Yi.Keymap.Vim.keymap