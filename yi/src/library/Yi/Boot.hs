-- | Boot process of Yi, as an instanciation of Dyre
module Yi.Boot (yi, yiDriver, reload) where

import qualified Config.Dyre as Dyre
import qualified Config.Dyre.Options as Dyre
import Config.Dyre.Relaunch
import Control.Monad.State
import qualified Data.Rope as R
import System.Directory
import System.Environment
import System.Exit

import Yi.Config
import Yi.Editor
import Yi.Keymap
import Yi.Main
import qualified Yi.UI.Common as UI

realMain :: (Config, ConsoleConfig) -> IO ()
realMain configs = do
    editor <- restoreBinaryState Nothing
    main configs editor

showErrorsInConf :: (Config, ConsoleConfig) -> String -> (Config, ConsoleConfig)
showErrorsInConf (conf, confcon) errs
    = (conf { initialActions = (makeAction $ splitE >> newBufferE (Left "*errors*") (R.fromString errs)) : initialActions conf }
      , confcon)

yi, yiDriver :: Config -> IO ()
yi = yiDriver

yiDriver cfg = do
    args <- Dyre.withDyreOptions Dyre.defaultParams getArgs 
    -- we do the arg processing before dyre, so we can extract '--ghc-option=' and '--help' and so on.
    case do_args cfg args of
        Left (Err err code) ->
          do putStrLn err
             exitWith code
        Right (finalCfg, cfgcon) -> 
            let yiParams = Dyre.defaultParams
                            { Dyre.projectName  = "yi"
                            , Dyre.realMain     = realMain
                            , Dyre.showError    = showErrorsInConf
                            , Dyre.configDir    = Just . getAppUserDataDirectory $ "yi"
                            , Dyre.hidePackages = ["mtl"]
                            , Dyre.ghcOpts      = ["-threaded", "-O2"] ++ ghcOptions cfgcon
                            }
            in Dyre.wrapMain yiParams (finalCfg, cfgcon)

reload :: YiM ()
reload = do
    editor <- withEditor get
    withUI (\ui -> UI.end ui False)
    liftIO $ relaunchWithBinaryState (Just editor) Nothing

