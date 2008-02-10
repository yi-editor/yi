module Yi (driver, yi, module Yi.Yi) where

import Yi.Yi



import qualified Yi.Main as Yi
import HConf

import Yi.Core (Config (..), defaultConfig)
import Yi.Debug
import Yi.Kernel
#ifdef DYNAMIC
import Yi.Boot

{-
main :: IO ()
main = do
  initDebug ".yi-static.dbg"
  kernel <- initialize
  Yi.Boot.startYi kernel -- call Yi.main dynamically

-- TODO: also init the debug system

-}

import Control.Monad

main0 :: Config -> IO ()
main0 cfg = do
  initDebug ".yi-static.dbg"
  kernel <- initialize

  -- Setup the debug module in the dynamic session (this needs to be done only because
  -- debug uses "global" variables.
  debugMod <- guessTarget kernel "Yi.Debug" Nothing
  setTargets kernel [debugMod]
  loadAllTargets kernel
  initDebug' <- join $ evalMono kernel "Yi.Debug.initDebug \".yi.dbg\""
  initDebug' :: IO ()

  -- Fire up Yi
  Yi.main cfg kernel

#else
main0 :: Config -> IO ()
main0 cfg = do
  initDebug ".yi-static.dbg"
  Yi.main cfg Kernel
#endif





recoverState _stateName = return ()

realMain cfg _state = main0 cfg

projectName = "yi"

initState = ()

driver = mainMaster projectName initState (realMain defaultConfig)

yi cfg = mainSlave recoverState () (realMain cfg)
