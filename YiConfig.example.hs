module YiConfig ( yiMain ) where

import Yi.Yi
import Yi.Editor
import Yi.Keymap.Emacs
import Yi.Keymap.Vim
import Control.Monad.Trans

yiMain :: EditorM ()
yiMain = do
  lift $ initDebug ".yi.dbg.user"
  -- modifyEditor_  $ \e -> return e { defaultKeymap = Yi.Keymap.Vim.keymap }
  msgE "User configuration finished."
