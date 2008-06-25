module Yi (driver, yi, defaultConfig, module Yi.Yi) where

import Yi.Yi

import qualified Yi.Main
import HConf

import Yi.Core (Config (..))
import Yi.Main (defaultConfig)
import Yi.Debug
import Data.String

-- "real" main
--main0 :: Config -> IO ()

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
    = conf {startAction = withEditor $ newBufferE "*errors*" (fromString errs) >> return ()}

