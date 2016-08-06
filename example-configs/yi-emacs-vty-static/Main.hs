
import Lens.Micro.Platform
import Control.Monad.State.Lazy
import Data.List
import System.Environment

import           Yi hiding (super)
import           Yi.Config.Simple.Types
import           Yi.Config.Default.Emacs (configureEmacs)
import           Yi.Config.Default.Vty (configureVty)

main :: IO ()
main = do
    files <- getArgs
    let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) files)
    cfg <- execStateT
        (runConfigM (myConfig >> (startActionsA .= openFileActions)))
        defaultConfig
    startEditor cfg Nothing

myConfig :: ConfigM ()
myConfig = do
    configureVty
    configureEmacs