-- | Boot process of Yi, as an instanciation of HConf
module Yi.Boot (driver, yi) where

import Control.Monad
import HConf
import Yi.Buffer.Basic
import Yi.Config
import Yi.Debug
import Yi.Editor (newBufferE)
import Yi.Keymap (makeAction)
import qualified Yi.Main

recoverState :: String -> IO ()
recoverState _stateName = return ()

saveState :: () -> IO String
saveState _ = return ""

realMain :: Config -> yiState -> IO ()
realMain staticConfig _state = do
          when (debugMode staticConfig) $ initDebug ".yi.dbg" 
          -- initialize here so we can see debug messages early, if
          -- the flag is set in the static configuration.
          Yi.Main.main staticConfig

initState :: () -- TODO: Should be Editor
initState = ()

driver :: IO ()
yi :: Config -> IO ()
HConf driver yi _ = getHConf Yi.Main.projectName initState recoverState saveState Yi.Main.defaultConfig showErrorsInConf realMain

showErrorsInConf :: String -> Config -> Config
showErrorsInConf errs conf 
    = conf {startActions = [makeAction $ newBufferE "*errors*" (fromString errs)]}

