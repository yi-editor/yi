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
-- This is the real main module of HEmacs, and is shared between Main.hs
-- (the static binary), and dynamically loaded by Boot.hs. It is linked
-- into -package hemacs, though.
--

module HEmacs (static_main, dynamic_main) where

import HEmacs.Entry             ( EntryTree )
import HEmacs.Locale            ( setupLocale )
import HEmacs.MBox
import HEmacs.Version           ( package, version )
import qualified HEmacs.UI        as UI
import qualified HEmacs.ConfigAPI

import Control.Exception        ( bracket )
import Data.IORef               ( readIORef, newIORef, writeIORef )
import System.Console.GetOpt
import System.Environment       ( getArgs )
import System.Exit              ( exitWith, ExitCode(ExitSuccess) )
import System.IO.Unsafe         ( unsafePerformIO )
import System.Posix.Signals as Signals

import GHC.Base                 ( unsafeCoerce# )

-- ---------------------------------------------------------------------
-- Argument parsing. Pretty standard, except for the trick with -B.
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
usage    = putStr   $ usageInfo "Usage: hemacs [option...] [file]" options
versinfo = putStrLn $ package++" "++version

--
-- deal with real options
--
do_opts :: [Opts] -> IO ()
do_opts [] = return ()
do_opts (o:oo) =
    case o of
        Help     -> usage    >> exitWith(ExitSuccess)
        Version  -> versinfo >> exitWith(ExitSuccess)
        Libdir _ -> do_opts oo  -- ignore -B flag. already handled in Boot.hs

--
-- everything that is left over
--
do_args :: [String] -> IO ([EntryTree MBoxEntry], String)
do_args args =
    case (getOpt Permute options args) of
        (o, n, []) -> do
            do_opts o
            case n of
                []    -> return ([], "<null>")
                (_f:_) -> return ([], "<null>")
        (_, _, errs) -> error (concat errs)

-- ---------------------------------------------------------------------
-- Initialise the gui

initui :: [EntryTree MBoxEntry] -> String -> IO (UI.Status MBoxEntry)
initui tt fname = do
    styles'  <- get_global HEmacs.ConfigAPI.styles
    topinfo' <- get_global HEmacs.ConfigAPI.topinfo_text
    s   <- UI.init styles'
    s'  <- return $ UI.set_topinfo s topinfo'
    s'' <- return $ UI.set_entries s' tt (Just fname)
    return s''

-- ---------------------------------------------------------------------
-- Set up the signal handlers

init_sighandlers s = do 
    -- Signals.setStoppedChildFlag True -- crashes rts on openbsd
    Signals.installHandler Signals.sigCHLD Signals.Default Nothing
    Signals.installHandler Signals.sigINT  Signals.Ignore Nothing
    Signals.installHandler Signals.sigPIPE Signals.Ignore Nothing

release_sighandlers = do 
    Signals.installHandler Signals.sigINT Signals.Default Nothing
    Signals.installHandler Signals.sigPIPE Signals.Default Nothing

-- ---------------------------------------------------------------------
-- The "g_settings" var stores the user-configurable information. It is
-- initialised with the default settings, so it works even if the user
-- doesnt provide a ~/.hemacs/Config.hs, or stuffs up Config.hs in some
-- way.
--
g_settings = unsafePerformIO $ newIORef (HEmacs.ConfigAPI.settings)
{-# NOINLINE g_settings #-}

get_global selector = readIORef g_settings >>= \v -> return $ selector v

-- ---------------------------------------------------------------------
--
-- Static main. This is the front end to the statically linked
-- application.
--
static_main :: IO ()
static_main = do
    setupLocale
    args <- getArgs
    (tt, fname) <- do_args args

    hk <- get_global HEmacs.ConfigAPI.handle_key
    
    tt <- return $! tt -- Force load before initialising UI
    bracket (initui tt fname >>= \s -> init_sighandlers s >> return s)
            (\_ -> UI.deinit >> release_sighandlers)
            (\s -> UI.refresh s >> UI.event_loop s hk)


-- ---------------------------------------------------------------------
--
-- Ddynamic main. This is jumped to from from Boot.hs, after dynamically
-- loading HShemacs.o
--
dynamic_main :: HEmacsMainType
dynamic_main v = putStrLn "done." >> dynamic_main' v

dynamic_main' :: HEmacsMainType
dynamic_main' Nothing = static_main

dynamic_main' (Just (CD cfg)) = do 
    case unsafeCoerce# cfg of   -- MAGIC: to unwrap the config value
        (cfg_ :: HEmacs.ConfigAPI.Config) -> do
                writeIORef g_settings cfg_
                static_main

-- ---------------------------------------------------------------------
-- MAGIC type. Also identically defined in Boot.hs. See there for
-- details
--

data ConfigData = forall a. CD a {- has Config type -}

type HEmacsMainType = (Maybe ConfigData) -> IO ()

-- vim: sw=4 ts=4
