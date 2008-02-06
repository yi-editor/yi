-- Don't run with --as=emacs (or any --as=) !
-- (Otherwise you would not see your nice new keymap)

module YiConfig where

import Yi.Yi
import Yi.Editor
import Yi.Keymap.Emacs
import Yi.Keymap.Emacs.Keys
import Control.Monad.Trans
-- You can import any number of packages that will be loaded at start.
-- They will have to be used fully qualified though.

yiMain :: YiM ()
yiMain = do
  reconfigure
  -- whatever code can be ran here.
  msgEditor "User configuration successful."

-- | reload YiConfig and run reconfigure.
myReconfigure = do
  reloadEditor
  execEditorAction "YiConfig.reconfigure" -- Must be dynamic (if we want to see the new reconfigure that has (hopefully) been just loaded)
  msgEditor "reconfiguration finished."

myKeymap = rebind [("C-h r", write $ myReconfigure),
                   ("C-h i", write $ help),
                   ("C-h t", write $ testAction)
                  ] keymap

help = msgEditor "Change ~/.yi/YiConfig.testAction, type C-h r to reload, then C-h t to test your changes. Errors are in the *messages* and *console* buffer"

testAction = msgEditor "Have fun changing this! :)"

reconfigure = do
  changeKeymap myKeymap
