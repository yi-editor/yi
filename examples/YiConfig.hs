module YiConfig ( yiMain ) where

import Yi.Yi
import Yi.Editor
import Yi.Keymap.Emacs
import Yi.Keymap.Vim
import Control.Monad.Trans
-- You can import any number of packages that will be loaded at start.
-- They will have to be used fully qualified though.

yiMain :: EditorM ()
yiMain = do
  -- whatever code can be ran here.
  msgE "User configuration finished."
