-- | Boot process of Yi, as an instanciation of HConf
-- TODO: rename to Yi.Boot
module Yi (driver, yi, defaultConfig, module Yi.Yi) where

import Yi.Yi

import qualified Yi.Main
import HConf

import Yi.Config
import Yi.Main (defaultConfig)
import Yi.Debug
import Data.String

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
HConf driver yi _ = getHConf Yi.Main.projectName initState recoverState saveState defaultConfig showErrorsInConf realMain

showErrorsInConf :: String -> Config -> Config
showErrorsInConf errs conf 
    = conf {startActions = [makeAction $ newBufferE "*errors*" (fromString errs)]}

