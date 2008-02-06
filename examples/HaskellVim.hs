module YiConfig ( yiMain ) where

import Yi.Yi
import Yi.Editor
import Yi.Keymap.Vim
import Yi.Syntax.Table ( highlighters )
import qualified Data.Map as M
import Control.Monad.Trans

yiMain :: YiM ()
yiMain = do
  changeKeymap myKeymap
  msgEditor "User configuration finished."


myKeymap = do
  write $ setSyntaxB (highlighters M.! "haskell")
  Yi.Keymap.Vim.keymap
