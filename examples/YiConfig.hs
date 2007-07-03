-- This is jyp's YiConfig.

-- Don't run with --as=..., that will overwrite the keymap set here.

module YiConfig (yiMain) where

import Yi.Yi  
import Yi.Editor
import Yi.Keymap.Emacs
import Control.Monad.Trans
import Yi.Buffer

yiMain :: YiM ()
yiMain = do
  changeKeymapE myKeymap  
 
  msgE "User configuration successful."


-- I prefer to use Emacs keymap, with haskell hilight in every buffer.
myKeymap = do write $ setSyntaxB "haskell" 
              keymap

 