{-# OPTIONS -cpp -fglasgow-exts #-}
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

module Main where

import HEmacs.Entry                  (EntryTree)
import HEmacs.MBox
import HEmacs.Version                (package, version)
import HEmacs.Locale                 (setupLocale)
import qualified HEmacs.UI        as UI
import qualified HEmacs.ConfigAPI as ConfigAPI

import qualified BootAPI

import Control.Exception      (catchJust, ioErrors, throw, bracket)
import Control.Monad          (liftM, when)
import Data.IORef
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment     (getArgs, getEnv)
import System.Exit
import System.IO              (readFile, writeFile)
import System.IO.Error        (isDoesNotExistError)
import System.IO.Unsafe       (unsafePerformIO)
import System.Posix.User      (getUserEntryForID, getRealUserID, homeDirectory)
import System.Posix.Signals as PosixSig

import GHC.Base (unsafeCoerce#)

default_file = ".riot"

get_home :: IO (String)
get_home =
    catch (getRealUserID >>= getUserEntryForID >>= (return . homeDirectory))
          (\_ -> getEnv "HOME")

get_default_file :: IO (String)
get_default_file = do
    home <- get_home
    return $ home ++ "/" ++ default_file

do_load :: String -> IO [EntryTree MBoxEntry]
do_load fname = 
     liftM read_mbox_entrytree $ readFile fname

do_save :: String -> [EntryTree MBoxEntry] -> IO ()
do_save fname et = 
    writeFile fname $! show_mbox_entrytree et

load_initial :: String -> IO ([EntryTree MBoxEntry], String)
load_initial fname = do
    et <- catchJust ioErrors (do_load fname) exh
    return (et, fname)
    where
        exh e = if isDoesNotExistError e then
                    return []
                else
                    ioError e

load_default :: IO ([EntryTree MBoxEntry], String)
load_default =  get_default_file >>= load_initial


initui :: [EntryTree MBoxEntry] -> String -> IO (UI.Status MBoxEntry)
initui tt fname = do
    styles  <- readIORef settings >>= \v -> return $ ConfigAPI.styles v
    topinfo <- readIORef settings >>= \v -> return $ ConfigAPI.topinfo_text v
    s <- UI.init styles
    s <- return $ UI.set_topinfo s topinfo
    s <- return $ UI.set_entries s tt (Just fname)
    s <- return $ UI.set_callbacks s do_save new_mboxentry
    return s

init_sighandlers s = do 
    -- PosixSig.setStoppedChildFlag True
    PosixSig.installHandler PosixSig.sigCHLD PosixSig.Default Nothing
    PosixSig.installHandler PosixSig.sigINT PosixSig.Ignore Nothing
    PosixSig.installHandler PosixSig.sigPIPE PosixSig.Ignore Nothing

release_sighandlers = do 
    PosixSig.installHandler PosixSig.sigINT PosixSig.Default Nothing
    PosixSig.installHandler PosixSig.sigPIPE PosixSig.Default Nothing

data Opts = Help | Version | Libdir String

options :: [OptDescr Opts]
options = [
    Option ['V']  ["version"] (NoArg Version) "Show version information",
    Option ['B']  ["libdir"]  (ReqArg Libdir "libdir") "Path to runtime libraries",
    Option ['h']  ["help"]    (NoArg Help)    "Show this help"
    ]

usage = putStr $ usageInfo "Usage: riot [option...] [file]" options
versinfo = putStr $ package++" "++version++"\n"

do_opts :: [Opts] -> IO ()
do_opts [] = return ()
do_opts (o:oo) =
    case o of
        Help     -> usage    >> exitWith(ExitSuccess)
        Version  -> versinfo >> exitWith(ExitSuccess)
        Libdir s -> do_opts oo

do_args :: [String] -> IO ([EntryTree MBoxEntry], String)
do_args args =
    case (getOpt Permute options args) of
        (o, n, []) -> do
            do_opts o
            case n of
                [] -> load_default
                (f:[]) -> load_initial f
                otherwise -> error "Too many file parameters."
        (_, _, errs) -> error (concat errs)

--
-- grab the default settings, so it works even if they stuff up Config.hs
-- this is where we store the config values supplied by the user
--
settings = unsafePerformIO $ newIORef (ConfigAPI.settings)
{-# NOINLINE settings #-}

--
-- static main. no plugins
--
main = do
    setupLocale
    args <- getArgs
    (tt, fname) <- do_args args

    hk <- readIORef settings >>= \v -> return $ ConfigAPI.handle_key v
    
    tt <- return $! tt -- Force load before initialising UI
    bracket (initui tt fname >>= \s -> init_sighandlers s >> return s)
            (\_ -> UI.deinit >> release_sighandlers)
            (\s -> UI.refresh s >> UI.event_loop s hk)

--
-- dynamic main. called from Boot.hs
--
dynamic_main :: BootAPI.HEmacsMainType
dynamic_main (Just (BootAPI.CD cfg)) = do 
    putStrLn "done."
    case unsafeCoerce# cfg of -- MAGIC: to unwrap the existentially-passed config value
        (cfg_ :: ConfigAPI.Config) -> writeIORef settings cfg_ >> main

dynamic_main Nothing = putStrLn "done." >> main

