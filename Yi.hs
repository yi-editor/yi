module Yi (driver, yi, defaultConfig, module Yi.Yi) where

import Yi.Yi

import qualified Yi.Main as Yi
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
realMain cfg _state = do
          initDebug ".yi.dbg"
          Yi.main cfg

projectName :: String
projectName = "yi"

initState :: () -- TODO: Should be Editor
initState = ()

driver :: IO ()
yi :: Config -> IO ()
HConf driver yi _ = getHConf projectName initState recoverState saveState defaultConfig showErrorsInConf realMain

showErrorsInConf :: String -> Config -> Config
showErrorsInConf errs conf 
    = conf {startAction = withEditor $ newBufferE "*errors*" (fromString errs) >> return ()}

