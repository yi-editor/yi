module Yi (driver, yi, defaultConfig, module Yi.Yi) where

import Yi.Yi

import qualified Yi.Main as Yi
import HConf

import Yi.Core (Config (..))
import Yi.Main (defaultConfig)
import Yi.Debug

main0 :: Config -> IO ()
main0 cfg = do
  initDebug ".yi.dbg"
  Yi.main cfg

recoverState :: String -> IO ()
recoverState _stateName = return ()

realMain :: Config -> yiState -> IO ()
realMain cfg _state = main0 cfg

projectName :: String
projectName = "yi"

initState :: () -- TODO: Should be Editor
initState = ()

driver :: IO ()
driver = mainMaster projectName initState (realMain defaultConfig)

yi :: Config -> IO ()
yi cfg = mainSlave recoverState () (realMain cfg)
