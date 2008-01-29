-- This is jyp's YiConfig.

-- Don't run with --as=..., that will overwrite the keymap set here.

module YiConfig (yiMain) where

import Yi.Yi  
import Yi.Editor
import Yi.Keymap.Emacs
-- import Yi.Keymap.Vim
import Control.Monad.Trans
import Yi.Buffer

yiMain :: YiM ()
yiMain = do
  -- I prefer to use Emacs keymap
  changeKeymap keymap
  msgEditor "User configuration successful."

