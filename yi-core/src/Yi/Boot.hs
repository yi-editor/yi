{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Boot
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Boot process of Yi.
--
-- Uses Dyre to implement the XMonad-style dynamic reconfiguration.

module Yi.Boot (yi, yiDriver, reload) where

import qualified Config.Dyre as Dyre
import qualified Config.Dyre.Options as Dyre
import qualified Config.Dyre.Params as Dyre
import           Config.Dyre.Relaunch
import           Control.Lens
import           Data.Text ()
import           System.Environment
import           System.Exit
import           Yi.Boot.Internal
import           Yi.Buffer.Misc (BufferId(..))
import           Yi.Config
import           Yi.Editor
import           Yi.Keymap
import           Yi.Main
import           Yi.Paths (getCustomConfigPath)
import           Yi.Rope (fromString)

-- | Once the custom yi is compiled this restores the editor state (if
-- requested) then proceeds to run the editor.
realMain :: (Config, ConsoleConfig) -> IO ()
realMain configs = restoreBinaryState Nothing >>= main configs

-- | If the custom yi compile produces errors or warnings then the
-- messages are presented as a separate activity in the editor.
--
-- The use of a separate activity prevents any other initial actions
-- from immediately masking the output.
showErrorsInConf :: (Config, ConsoleConfig) -> String -> (Config, ConsoleConfig)
showErrorsInConf c errs = c & _1 . initialActionsA %~ (makeAction openErrBuf :)
  where
    openErrBuf = splitE >> newBufferE (MemBuffer "*errors*") (fromString errs)

-- | Handy alias for 'yiDriver'.
yi :: Config -> IO ()
yi = yiDriver

-- | Used by both the yi executable and the custom yi that is built
-- from the user's configuration. The yi executable uses a default
-- config.
yiDriver :: Config -> IO ()
yiDriver cfg = do
  args <- Dyre.withDyreOptions Dyre.defaultParams getArgs
  -- we do the arg processing before dyre, so we can extract
  -- '--ghc-option=' and '--help' and so on.
  case do_args cfg args of
    Left (Err err code) -> putStrLn err >> exitWith code
    Right (finalCfg, cfgcon) -> do
      modules <- getCustomConfigPath (userConfigDir cfgcon) "modules"
      let yiParams = Dyre.defaultParams
                      { Dyre.projectName  = "yi"
                      , Dyre.realMain     = realMain
                      , Dyre.showError    = showErrorsInConf
                      , Dyre.configDir    = Just $ userConfigDir cfgcon
                      , Dyre.ghcOpts      = ["-threaded", "-O2", "-rtsopts"]
                                            ++ ["-i" ++ modules]
                                            ++ profilingParams
                                            ++ ghcOptions cfgcon
                      , Dyre.includeCurrentDirectory = False
                      , Dyre.rtsOptsHandling = Dyre.RTSAppend ["-I5"]
                      }
      Dyre.wrapMain yiParams (finalCfg, cfgcon)

-- | CPP-guarded profiling params.
profilingParams :: [String]
profilingParams =
#ifdef EVENTLOG
  ["-eventlog", "-rtsopts"] ++
#endif
#ifdef PROFILING
  ["-prof", "-auto-all", "-rtsopts"
  , "-osuf=p_o", "-hisuf=p_hi"] ++
#endif
  []
