-- This is a configuration example, that requires manual rebuild (as opposed to dynamic ones, using
-- Dyre, which you're, most probably, actually want). Here's a building example with "stack":
-- 1. Edit "stack.yaml" file so that "location: " would point to the root of the Yi source code
-- 2. Run "stack install"
-- 3. Run the built with "stack exec yi-vim-vty" (or just "yi-vim-vty" if you have $PATH
--    configured acc.)
-- The final name of the executable can be changed in the "package.yaml" file.

import Control.Monad.State.Lazy
import Data.List
import Lens.Micro.Platform
import System.Environment

import Yi
import Yi.Config.Simple.Types
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.MiscModes (configureMiscModes)
import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)

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
    configureVim
    configureHaskellMode
    configureMiscModes
