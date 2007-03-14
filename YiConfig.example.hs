module YiConfig ( yiMain ) where

import Yi.Yi
import Yi.Editor
import Yi.Keymap.Emacs
import Yi.Keymap.Vim
import Control.Monad.Trans

yiMain :: EditorM ()
yiMain = do
  lift $ initDebug ".yi.dbg.user"
  msgE "User configuration finished."
