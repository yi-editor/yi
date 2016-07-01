{-# LANGUAGE CPP #-}

-- | This is the main module of Yi, called with configuration from the user.
-- Here we mainly process command line arguments.

module Yi.Main (
                -- * Static main
                main,
                -- * Command line processing
                do_args,
                ConsoleConfig(..),
                Err(..),
               ) where

import Control.Monad.Error
import Data.Char
import Data.List (intercalate)
import Distribution.Text (display)
import System.Console.GetOpt
import System.Exit
#ifndef HLINT
#include "ghcconfig.h"
#endif

import Yi.Buffer
import Yi.Config
import Yi.Config.Default
import Yi.Core (startEditor)
import Yi.Debug
import Yi.Editor
import Yi.File
import Yi.Keymap
import Yi.Paths (getConfigDir)
import Paths_yi

frontendNames :: [String]
frontendNames = fmap fst' availableFrontends
  where fst' :: (a,UIBoot) -> a
        fst' (x,_) = x

data Err = Err String ExitCode

instance Error Err where
    strMsg s = Err s (ExitFailure 1)

-- | Configuration information which can be set in the command-line, but not
-- in the user's configuration file.
data ConsoleConfig =
  ConsoleConfig {
     ghcOptions :: [String],
     selfCheck :: Bool,
     userConfigDir :: IO FilePath
  }

defaultConsoleConfig :: ConsoleConfig
defaultConsoleConfig =
  ConsoleConfig {
                  ghcOptions = [],
                  selfCheck = False,
                  userConfigDir = Yi.Paths.getConfigDir
                }

-- ---------------------------------------------------------------------
-- | Argument parsing. Pretty standard.

data Opts = Help
          | Version
          | LineNo String
          | EditorNm String
          | File String
          | Frontend String
          | ConfigFile String
          | SelfCheck
          | GhcOption String
          | Debug
          | OpenInTabs

-- | List of editors for which we provide an emulation.
editors :: [(String,Config -> Config)]
editors = [("emacs", toEmacsStyleConfig),
           ("vim",  toVimStyleConfig),
           ("cua",   toCuaStyleConfig)]

options :: [OptDescr Opts]
options =
  [ Option []     ["self-check"]  (NoArg  SelfCheck)             "Run self-checks"
  , Option ['f']  ["frontend"]    (ReqArg Frontend   "FRONTEND") frontendHelp
  , Option ['y']  ["config-file"] (ReqArg ConfigFile "PATH")     "Specify a folder containing a configuration yi.hs file"
  , Option ['V']  ["version"]     (NoArg  Version)               "Show version information"
  , Option ['h']  ["help"]        (NoArg  Help)                  "Show this help"
  , Option []     ["debug"]       (NoArg  Debug)                 "Write debug information in a log file"
  , Option ['l']  ["line"]        (ReqArg LineNo     "NUM")      "Start on line number"
  , Option []     ["as"]          (ReqArg EditorNm   "EDITOR")   editorHelp
  , Option []     ["ghc-option"]  (ReqArg GhcOption  "OPTION")   "Specify option to pass to ghc when compiling configuration file"
  , Option [openInTabsShort] [openInTabsLong] (NoArg  OpenInTabs)  "Open files in tabs"
  ] where frontendHelp = "Select frontend, which can be one of:\n" ++ intercalate ", " frontendNames
          editorHelp   = "Start with editor keymap, where editor is one of:\n" ++ (intercalate ", " . fmap fst) editors

openInTabsShort :: Char
openInTabsShort = 'p'

openInTabsLong :: String
openInTabsLong = "open-in-tabs"

-- | usage string.
usage, versinfo :: String
usage = usageInfo "Usage: yi [option...] [file]" options

versinfo = "yi " ++ display version

-- | Transform the config with options
do_args :: Config -> [String] -> Either Err (Config, ConsoleConfig)
do_args cfg args =
    case getOpt (ReturnInOrder File) options args of
        (os, [], []) -> foldM (getConfig shouldOpenInTabs) (cfg, defaultConsoleConfig) (reverse os)
        (_, _, errs) -> fail (concat errs)
    where
        shouldOpenInTabs = ("--" ++ openInTabsLong) `elem` args
                         || ('-':[openInTabsShort]) `elem` args

-- | Update the default configuration based on a command-line option.
getConfig :: Bool -> (Config, ConsoleConfig) -> Opts -> Either Err (Config, ConsoleConfig)
getConfig shouldOpenInTabs (cfg, cfgcon) opt =
    case opt of
      Frontend f -> case lookup f availableFrontends of
                      Just frontEnd -> return (cfg { startFrontEnd = frontEnd }, cfgcon)
                      Nothing       -> fail "Panic: frontend not found"
      Help          -> throwError $ Err usage ExitSuccess
      Version       -> throwError $ Err versinfo ExitSuccess
      Debug         -> return (cfg { debugMode = True }, cfgcon)
      LineNo l      -> case startActions cfg of
                         x : xs -> return (cfg { startActions = x:makeAction (gotoLn (read l)):xs }, cfgcon)
                         []     -> fail "The `-l' option must come after a file argument"

      File filename -> if shouldOpenInTabs && not (null (startActions cfg)) then
                         prependActions [YiA $ openNewFile filename, EditorA newTabE]
                       else
                         prependAction $ openNewFile filename

      EditorNm emul -> case lookup (fmap toLower emul) editors of
             Just modifyCfg -> return (modifyCfg cfg, cfgcon)
             Nothing -> fail $ "Unknown emulation: " ++ show emul
      GhcOption ghcOpt -> return (cfg, cfgcon { ghcOptions = ghcOptions cfgcon ++ [ghcOpt] })
      ConfigFile f -> return (cfg, cfgcon { userConfigDir = return f })
      _ -> return (cfg, cfgcon)
  where
    prependActions as = return (cfg { startActions = fmap makeAction as ++ startActions cfg }, cfgcon)
    prependAction a = return (cfg { startActions = makeAction a : startActions cfg}, cfgcon)

-- ---------------------------------------------------------------------
-- | Static main. This is the front end to the statically linked
-- application, and the real front end, in a sense. 'dynamic_main' calls
-- this after setting preferences passed from the boot loader.
--
main :: (Config, ConsoleConfig) -> Maybe Editor -> IO ()
main (cfg, _cfgcon) state = do
  when (debugMode cfg) $ initDebug ".yi.dbg"
  startEditor cfg state
