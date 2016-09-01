-- This version of config have to be rebuilt manually, as opposed to dynamic ones, where building is
-- handled by dyre. An example of building via stack:
--
-- 1. copy everything to ~/.config/yi/ directory
-- 2. change in lts.yaml and stack.yaml files the "location" so that it'd point to the root of the
--    Yi sources directory (by default its value meant to be built straight in the source)
-- 3. run "stack install"
-- After that you can run the binary with "stack exec yi" (or just "yi" if you configured $PATH acc.)

import Lens.Micro.Platform
import Control.Monad.State.Lazy
import Data.List
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
