--
-- riot/Main.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

--
-- | This is the real main module of Yi, and is shared between
-- Main.hs (the static binary), and dynamically loaded by Boot.hs.
-- We take any config arguments from the boot loader (if that is how we
-- are being invoked) parse command line args, initialise the ui, before
-- jumping into an event loop.
--

module Yi (static_main, dynamic_main) where

import Yi.Locale                        ( setupLocale )
import Yi.Version                       ( package, version )
import qualified Yi.Editor  as Editor
import qualified Yi.Config  as Config
import qualified Yi.Core    as Core 

import Control.Exception        ( bracket_ )
import Data.IORef               ( IORef, readIORef, newIORef, writeIORef )
import System.Console.GetOpt
import System.Environment       ( getArgs )
import System.Exit              ( exitWith, ExitCode(ExitSuccess) )
import System.IO.Unsafe         ( unsafePerformIO )
import System.Posix.Signals as Signals

import GHC.Base                 ( unsafeCoerce# )

-- ---------------------------------------------------------------------
-- | Argument parsing. Pretty standard, except for the trick with -B.
-- The -B flag is needed, and used by Boot.hs to find the runtime
-- libraries. We still parse it here, but ignore it.

data Opts = Help | Version | Libdir String

options :: [OptDescr Opts]
options = [
    Option ['V']  ["version"] (NoArg Version) "Show version information",
    Option ['B']  ["libdir"]  (ReqArg Libdir "libdir") "Path to runtime libraries",
    Option ['h']  ["help"]    (NoArg Help)    "Show this help"
    ]

--
-- usage string.
--
usage, versinfo :: IO ()
usage    = putStr   $ usageInfo "Usage: yi [option...] [file]" options
versinfo = putStrLn $ package++" "++version

--
-- deal with real options
--
do_opts :: [Opts] -> IO ()
do_opts (o:oo) = case o of
    Help     -> usage    >> exitWith ExitSuccess
    Version  -> versinfo >> exitWith ExitSuccess
    Libdir _ -> do_opts oo  -- ignore -B flag. already handled in Boot.hs
do_opts [] = return ()

--
-- everything that is left over
--
do_args :: [String] -> IO (Maybe [FilePath])
do_args args =
    case (getOpt Permute options args) of
        (o, n, []) -> do
            do_opts o
            case n of
                []   -> return Nothing
                fs   -> return $ Just fs
        (_, _, errs) -> error (concat errs)

-- ---------------------------------------------------------------------
-- Set up the signal handlers

--
-- Notes on setStoppedChildFlag:
--      If this bit is set when installing a catching function for the SIGCHLD
--      signal, the SIGCHLD signal will be generated only when a child process
--      exits, not when a child process stops.
--
initSignals :: IO Handler
initSignals = do 
    -- Signals.setStoppedChildFlag True -- crashes rts on openbsd
    Signals.installHandler Signals.sigCHLD Signals.Default Nothing
    Signals.installHandler Signals.sigINT  Signals.Ignore Nothing
    Signals.installHandler Signals.sigPIPE Signals.Ignore Nothing

releaseSignals :: IO Handler
releaseSignals = do 
    Signals.installHandler Signals.sigINT Signals.Default Nothing
    Signals.installHandler Signals.sigPIPE Signals.Default Nothing

-- ---------------------------------------------------------------------
-- | The "g_settings" var stores the user-configurable information. It is
-- initialised with the default settings, so it works even if the user
-- doesnt provide a ~/.yi/Config.hs, or stuffs up Config.hs in some
-- way.
--
g_settings :: IORef Editor.Config
g_settings = unsafePerformIO $ newIORef (Config.settings)
{-# NOINLINE g_settings #-}

-- ---------------------------------------------------------------------
-- | Static main. This is the front end to the statically linked
-- application, and the real front end, in a sense. 'dynamic_main' calls
-- this after setting preferences passed from the boot loader.
--
-- Initialise the ui getting an initial editor state, set signal
-- handlers, then jump to ui event loop with the state.
--
static_main :: IO ()
static_main = do
    setupLocale
    args   <- getArgs
    mfiles <- do_args args
    config <- readIORef g_settings
    bracket_ (initSignals    >> Core.startE config mfiles)
             (Core.endE      >> releaseSignals)
             (Core.refreshE  >> Core.eventLoop)

-- ---------------------------------------------------------------------
-- | Dynamic main. This is jumped to from from Boot.hs, after dynamically
-- loading HSyi.o. It takes in user preferences, sets a global
-- variable if any settings were received, then jumps to static main.
--
dynamic_main :: YiMainType
dynamic_main v = putStrLn "done." >> dynamic_main' v

dynamic_main' :: YiMainType

dynamic_main' Nothing = static_main     -- No prefs found, use defaults

--
-- Prefs found by the boot loader, so write them into the global
-- settings. Anyone got an idea of how to unwrap this value without
-- using the coerce?
--
dynamic_main' (Just (CD cfg)) = do 
    case unsafeCoerce# cfg of   -- MAGIC: to unwrap the config value
        (cfg_ :: Editor.Config) -> do
                writeIORef g_settings cfg_
                static_main

-- ---------------------------------------------------------------------
-- | Magic type: encapsulates user prefs determined by the boot loader.
-- Also identically defined in Boot.hs. Why so magic -- the type is
-- defined here, but the boot loader generates a value of this type. Yet
-- the boot loader *is not* statically linked against this module. So
-- the type is defined in the boot loader too.
--
data ConfigData = forall a. CD a {- has Config type -}

--
-- | Maybe a set of user preferences, if the boot loader found
-- ~/.yi/Config.hs
--
type YiMainType = (Maybe ConfigData) -> IO ()

-- vim: sw=4 ts=4
