{-# LANGUAGE CPP #-}
-- | Boot process of Yi.
-- Uses Dyre to implement the XMonad-style dynamic reconfiguration.
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

-- | once the custom yi is compiled this restores the editor state (if requested) then proceeds to
-- run the editor.
realMain :: (Config, ConsoleConfig) -> IO ()
realMain configs = do
    editor <- restoreBinaryState Nothing
    main configs editor

-- | If the custom yi compile produces errors or warnings then the messages are presented as a
-- separate activity in the editor. 
--
-- The use of a separate activity prevents any other initial actions from immediately masking the
-- output. 
showErrorsInConf :: (Config, ConsoleConfig) -> String -> (Config, ConsoleConfig)
showErrorsInConf (conf, confcon) errs
    = (conf { initialActions = (makeAction $ splitE >> newBufferE (Left "*errors*") (R.fromString errs)) : initialActions conf }
      , confcon)

yi, yiDriver :: Config -> IO ()
yi = yiDriver

-- | Used by both the yi executable and the custom yi that is built from the user's configuration.
-- The yi executable uses a default config.
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
                            , Dyre.ghcOpts      = (["-threaded", "-O2"] ++
#ifdef PROFILING
                                                   ["-prof", "-auto-all", "-rtsopts"] ++
#endif
                                                   ghcOptions cfgcon)
                            }
            in Dyre.wrapMain yiParams (finalCfg, cfgcon)

-- | "reloads" the configuration
--
-- Serializes the editor state and relaunches Yi using the serialized state.
-- The launch of Yi will result in recompilation of the user's custom yi. This, in effect, "reloads"
-- the configuration.
reload :: YiM ()
reload = do
    editor <- withEditor get
    withUI (\ui -> UI.end ui False)
    liftIO $ relaunchWithBinaryState (Just editor) Nothing

