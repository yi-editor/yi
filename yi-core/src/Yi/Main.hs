{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This is the main module of Yi, called with configuration from the user.
-- Here we mainly process command line arguments.

module Yi.Main (
                -- * Static main
                main,
                -- * Command line processing
                do_args,
                ConsoleConfig(..),
               ) where

import Control.Monad
import Data.Char
import Data.Monoid
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Version (showVersion)
import Lens.Micro.Platform (view)
import System.Console.GetOpt
import System.Exit
#ifndef HLINT
#include "ghcconfig.h"
#endif

import Yi.Buffer
import Yi.Config
import Yi.Core (startEditor)
import Yi.Debug
import Yi.Editor
import Yi.File
import Yi.Keymap
import Yi.Option (YiOption, OptionError(..), yiCustomOptions)
import Yi.Paths (getConfigDir)
import Paths_yi_core

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
          | CustomNoArg YiOption
          | CustomReqArg (String -> YiOption) String
          | CustomOptArg (Maybe String -> YiOption) (Maybe String)

-- | List of editors for which we provide an emulation.
editors :: [(String,Config -> Config)]
editors = []

builtinOptions :: [OptDescr Opts]
builtinOptions =
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
  ] where frontendHelp = "Select frontend"
          editorHelp   = "Start with editor keymap, where editor is one of:\n" ++ (intercalate ", " . fmap fst) editors

convertCustomOption :: OptDescr YiOption -> OptDescr Opts
convertCustomOption (Option short long desc help) = Option short long desc' help
    where desc' = convertCustomArgDesc desc

convertCustomArgDesc :: ArgDescr YiOption -> ArgDescr Opts
convertCustomArgDesc (NoArg o) = NoArg (CustomNoArg o)
convertCustomArgDesc (ReqArg f s) = ReqArg (CustomReqArg f) s
convertCustomArgDesc (OptArg f s) = OptArg (CustomOptArg f) s

customOptions :: Config -> [OptDescr Opts]
customOptions = fmap convertCustomOption . view yiCustomOptions

openInTabsShort :: Char
openInTabsShort = 'p'

openInTabsLong :: String
openInTabsLong = "open-in-tabs"

-- | usage string.
usage :: [OptDescr Opts] -> T.Text
usage opts = T.pack $ usageInfo "Usage: yi [option...] [file]" opts

versinfo :: T.Text
versinfo = "yi " <> T.pack (showVersion version)

-- | Transform the config with options
do_args :: Bool -> Config -> [String] -> Either OptionError (Config, ConsoleConfig)
do_args ignoreUnknown cfg args = let options = customOptions cfg ++ builtinOptions in
    case getOpt' (ReturnInOrder File) options args of
        (os, [], [], []) -> handle options os
        (os, _, u:us, []) -> if ignoreUnknown
                                    then handle options os
                                    else fail $ "unknown arguments: " ++ intercalate ", " (u:us)
        (_os, _ex, _ey, errs) -> fail (concat errs)
    where
        shouldOpenInTabs = ("--" ++ openInTabsLong) `elem` args
                         || ('-':[openInTabsShort]) `elem` args
        handle options os = foldM (getConfig options shouldOpenInTabs) (cfg, defaultConsoleConfig) (reverse os)

-- | Update the default configuration based on a command-line option.
getConfig :: [OptDescr Opts] -> Bool -> (Config, ConsoleConfig) -> Opts -> Either OptionError (Config, ConsoleConfig)
getConfig options shouldOpenInTabs (cfg, cfgcon) opt =
    case opt of
      Frontend _    -> fail "Panic: frontend not found"
      Help          -> Left $ OptionError (usage options) ExitSuccess
      Version       -> Left $ OptionError versinfo ExitSuccess
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
      CustomNoArg o -> do
          cfg' <- o cfg
          return (cfg', cfgcon)
      CustomReqArg f s -> do
          cfg' <- f s cfg
          return (cfg', cfgcon)
      CustomOptArg f s -> do
          cfg' <- f s cfg
          return (cfg', cfgcon)
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
