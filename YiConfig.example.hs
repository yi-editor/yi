module YiConfig ( yiMain ) where

import Yi.Yi
import Yi.Editor
import Yi.Keymap.Emacs

yiMain :: EditorM ()
yiMain = do
  modifyEditor_  $ \e ->
      return e { defaultKeymap = Yi.Keymap.Emacs.keymap }
  msgE "Emacs config set."
