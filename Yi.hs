module Yi (driver, yi, defaultConfig, module Yi.Yi) where

import Yi.Yi

import qualified Yi.Main as Yi
import HConf

import Yi.Core (Config (..))
import Yi.Main (defaultConfig)
import Yi.Debug

-- "real" main
--main0 :: Config -> IO ()

recoverState :: String -> IO ()
recoverState _stateName = return ()

realMain :: Config -> yiState -> IO ()
realMain cfg _state = do
          initDebug ".yi.dbg"
          Yi.main cfg

projectName :: String
projectName = "yi"

initState :: () -- TODO: Should be Editor
initState = ()

HConf driver yi = getHConf projectName () recoverState defaultConfig showErrorsInConf realMain

showErrorsInConf errs conf 
    = conf {startAction = withEditor $ newBufferE "*errors*" errs >> return ()}

