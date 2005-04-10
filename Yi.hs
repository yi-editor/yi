{-# OPTIONS -fglasgow-exts #-}
-- gla-exts required for unsafeCoerce#

--
-- riot/Main.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) Don Stewarti 2004.
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
import qualified Yi.Core    as Core 
import qualified Yi.Style   as Style

import qualified Yi.Map as M

import qualified Yi.Keymap.Vi    as Vi
import qualified Yi.Keymap.Vim   as Vim
import qualified Yi.Keymap.Nano  as Nano
import qualified Yi.Keymap.Emacs as Emacs

import qualified Yi.Curses.UI as UI

import Data.IORef

import Control.Monad            ( liftM, when )
import Control.Concurrent       ( myThreadId, throwTo )
import Control.Exception        ( catch, throw )

import System.Console.GetOpt
import System.Environment       ( getArgs )
import System.Exit
import System.IO.Unsafe         ( unsafePerformIO )
import System.Posix.Signals

import GHC.Base                 ( unsafeCoerce# )
import GHC.Exception            ( Exception(ExitException) )

-- ---------------------------------------------------------------------
-- | Argument parsing. Pretty standard, except for the trick with -B.
-- The -B flag is needed, and used by Boot.hs to find the runtime
-- libraries. We still parse it here, but ignore it.

data Opts = Help 
          | Version 
          | Libdir String 
          | LineNo String
          | EditorNm String

--
-- In case the user wants to start with a certain editor
--
editorFM :: M.Map [Char] ([Char] -> [Editor.Action])
editorFM = M.fromList $
    [("vi",     Vi.keymap)
    ,("vim",    Vim.keymap)
    ,("nano",   Nano.keymap)
    ,("emacs",  Emacs.keymap)
    ]

options :: [OptDescr Opts]
options = [
    Option ['V']  ["version"] (NoArg Version) "Show version information",
    Option ['B']  ["libdir"]  (ReqArg Libdir "libdir") "Path to runtime libraries",
    Option ['h']  ["help"]    (NoArg Help)    "Show this help",
    Option ['l']  ["line"]    (ReqArg LineNo "[num]") "Start on line number",
    Option []     ["as"]      (ReqArg EditorNm "[editor]") "Start with editor keymap"
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
    LineNo l -> writeIORef g_lineno ((read l) :: Int) >> do_opts oo

    EditorNm e -> case M.lookup e editorFM of
                    Just km -> do
                        (k,f,g) <- readIORef g_settings
                        writeIORef g_settings (k { Editor.keymap = km }, f, g)
                        do_opts oo
                    Nothing -> do
                        putStrLn $ "Unknown editor: "++show e++". Ignoring."
                        do_opts oo

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
-- | Set up the signal handlers

--
-- Notes on setStoppedChildFlag:
--      If this bit is set when installing a catching function for the SIGCHLD
--      signal, the SIGCHLD signal will be generated only when a child process
--      exits, not when a child process stops.
--
-- setStoppedChildFlag True
--
initSignals :: IO ()
initSignals = do 

    tid <- myThreadId

    -- to pass \^C to keymap (think emacs mode)
    installHandler sigINT (Catch (return ())) Nothing

    -- ignore
    sequence_ $ flip map [sigPIPE, sigALRM]
                          (\sig -> installHandler sig Ignore Nothing)

    -- and exit if we get the following:
    -- we have to do our own quitE here.
    sequence_ $ flip map [sigHUP, sigABRT, sigTERM] $ \sig -> do
            installHandler sig (CatchOnce $ do 
                    releaseSignals
                    UI.end
                    Editor.shutdown
                    throwTo tid (ExitException (ExitFailure 2))) Nothing

releaseSignals :: IO ()
releaseSignals =
    sequence_ $ flip map [sigINT, sigPIPE, sigHUP, sigABRT, sigTERM] 
               (\sig -> installHandler sig Default Nothing)

-- ---------------------------------------------------------------------
-- | The "g_settings" var stores the user-configurable information. It is
-- initialised with the default settings, so it works even if the user
-- doesnt provide a ~/.yi/x.hs, or stuffs up their config scripts in
-- some way. The second component is our reboot function, passed in from
-- Boot.hs, otherwise return ().
--
g_settings :: IORef (Editor.Config, IO (), IO Editor.Config )
g_settings = unsafePerformIO $ 
                newIORef (dflt_config
                         ,static_main 
                         ,return dflt_config)
{-# NOINLINE g_settings #-}

--
-- | default values to use if no ~/.yi/Config.hs is found
--
dflt_config :: Editor.Config
dflt_config = Editor.Config { 
        Editor.keymap = Vim.keymap,
        Editor.style  = Style.ui 
    }

--
-- | The line number to start on
--
g_lineno :: IORef Int
g_lineno = unsafePerformIO $ newIORef (1 :: Int)
{-# NOINLINE g_lineno #-}

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
    lineno <- readIORef g_lineno

    --
    -- The only way out is by throwing an exception, or an external
    -- signal. Is this alright?
    --
    -- catch any exception thrown by the main loop, clean up and quit
    -- (catching an ExitException), then re throw the original
    -- exception. catch that and print it, if it wasn't exit.
    --
    -- If we've rebooted, then we have two levels of catch wrapped
    -- around.
    --
    Control.Exception.catch
        (initSignals >> Core.startE config lineno mfiles >> Core.eventLoop)
        (\e -> do releaseSignals
                  Editor.shutdown
                  UI.end
                  when (not $ isExitCall e) $ print e
                  throw e
        )

    where
      isExitCall (ExitException _) = True
      isExitCall _ = False


-- ---------------------------------------------------------------------
-- | Dynamic main. This is jumped to from from Boot.hs, after dynamically
-- loading HSyi.o. It takes in user preferences, sets a global
-- variable if any settings were received, then jumps to static main.
--
dynamic_main :: YiMainType
dynamic_main v = dynamic_main' v

dynamic_main' :: YiMainType
dynamic_main' (Nothing, fn1, fn2) = do
    modifyIORef g_settings $ \(dflt,_,_) -> (dflt,fn1, liftM unwrap fn2)
    static_main             -- No prefs found, use defaults

--
-- Prefs found by the boot loader, so write them into the global
-- settings. Anyone got an idea of how to unwrap this value without
-- using the coerce?
--
dynamic_main' (cfg, fn1, fn2) = do 
        let cfg_ = unwrap cfg
        writeIORef g_settings (cfg_, fn1, liftM unwrap fn2)
        static_main

------------------------------------------------------------------------
--
-- unwrap a ConfigData value
--
unwrap :: Maybe ConfigData -> Editor.Config
unwrap Nothing         = dflt_config
unwrap (Just (CD cfg)) = unsafeCoerce# cfg :: Editor.Config

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
type YiMainType = (Maybe ConfigData, 
                   IO (), 
                   IO (Maybe ConfigData)) 
                -> IO ()

-- vim: sw=4 ts=4
