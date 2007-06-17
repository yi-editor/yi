-- Don't run with --as=emacs (or any --as=) !
-- (Otherwise you would not see your nice new keymap)

module YiConfig where

import Yi.Yi
import Yi.Editor
import Yi.Keymap.Emacs
import Control.Monad.Trans
-- You can import any number of packages that will be loaded at start.
-- They will have to be used fully qualified though.

yiMain :: YiM ()
yiMain = do
  reconfigure
  -- whatever code can be ran here.
  msgE "User configuration successful."


-- | reload YiConfig and run reconfigure.
reconfigureE = do
  reloadE
  execE "YiConfig.reconfigure" -- Must be dynamic (if we want to see the new reconfigure that has (hopefully) been just loaded)
  msgE "reconfiguration finished." 

myKeymap = runKeymap (rebind [("C-h r", write $ reconfigureE),
                              ("C-h i", write $ helpE),
                              ("C-h t", write $ testActionE)
                             ] normalKeymap)

helpE = msgE "Change ~/.yi/YiConfig.testAction, type C-h r to reload, then C-h t to test your changes. Errors are in the *messages* buffer"

testActionE = msgE "Have fun changing this! :)"


reconfigure = do
  changeKeymapE myKeymap
