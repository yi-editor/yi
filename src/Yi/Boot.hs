-- | Boot process of Yi, as an instanciation of Dyre
module Yi.Boot (yi, yiDriver, reload) where

import qualified Config.Dyre as Dyre
import Config.Dyre.Relaunch
import Control.Monad.State
import qualified Data.Rope as R
import System.Directory

import Yi.Config
import Yi.Editor
import Yi.Keymap
import qualified Yi.Main
import qualified Yi.UI.Common as UI

realMain :: Config -> IO ()
realMain config = do
    editor <- restoreBinaryState Nothing
    Yi.Main.main config editor

showErrorsInConf :: Config -> String -> Config
showErrorsInConf conf errs
    = conf { initialActions = (makeAction $ newBufferE (Left "*errors*") (R.fromString errs)) : initialActions conf }

yi, yiDriver :: Config -> IO ()

(yiDriver, yi) = (Dyre.wrapMain yiParams, Dyre.wrapMain yiParams)
  where yiParams = Dyre.defaultParams
             { Dyre.projectName  = "yi"
             , Dyre.realMain     = realMain
             , Dyre.showError    = showErrorsInConf
             , Dyre.configDir    = Just . getAppUserDataDirectory $ "yi"
             , Dyre.hidePackages = ["mtl"]
             , Dyre.ghcOpts = ["-threaded", "-O2"]
             }

reload :: YiM ()
reload = do
    editor <- withEditor get
    withUI (\ui -> UI.end ui False)
    liftIO $ relaunchWithBinaryState (Just editor) Nothing

