--
-- riot/Main.hs
--
-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) Don Stewarti 2004-5.
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

import Prelude hiding (error)

import Yi.Version                       ( package, version )
import qualified Yi.Editor  as Editor
import qualified Yi.Core    as Core
import qualified Yi.Style   as Style

import Yi.Debug

{- All the standard editor front ends -}
import qualified Yi.Keymap.Vi     as Vi
import qualified Yi.Keymap.Vim    as Vim
import qualified Yi.Keymap.Nano   as Nano
import qualified Yi.Keymap.Emacs  as Emacs
import qualified Yi.Keymap.Emacs2 as Emacs2
import qualified Yi.Keymap.Vimacs as Vimacs
import qualified Yi.Keymap.Joe    as Joe
import qualified Yi.Keymap.Ee     as Ee
import qualified Yi.Keymap.Mg     as Mg

import qualified Yi.UI as UI

import Data.Char
import Data.IORef
import Data.List                ( intersperse )
import qualified Data.Map as M

import Control.Monad            ( when )
import Control.Concurrent       ( myThreadId, throwTo )
import Control.Exception        ( catch, throw )

import System.Console.GetOpt
import System.Environment       ( getArgs )
import System.Exit
import System.IO.Unsafe         ( unsafePerformIO )
import System.Posix.Signals

import GHC.Exception            ( Exception(ExitException) )

#include "ghcconfig.h"

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
editorFM :: M.Map [Char] (Editor.Keymap)
editorFM = M.fromList $
    [ ("vi"      ,      Vi.keymap)
    , ("vim"     ,     Vim.keymap)
    , ("nano"    ,    Nano.keymap)
    , ("emacs"   ,   Emacs.keymap)
    , ("emacs2"  ,  Emacs2.keymap)
    , ("vimacs"  ,  Vimacs.keymap)
    , ("joe"     ,     Joe.keymap)
    , ("ee"      ,      Ee.keymap)
    , ("mg"      ,      Mg.keymap)
    ]

options :: [OptDescr Opts]
options = [
    Option ['V']  ["version"] (NoArg Version) "Show version information",
    Option ['B']  ["libdir"]  (ReqArg Libdir "libdir") "Path to runtime libraries",
    Option ['h']  ["help"]    (NoArg Help)    "Show this help",
    Option ['l']  ["line"]    (ReqArg LineNo "[num]") "Start on line number",
    Option []     ["as"]      (ReqArg EditorNm "[editor]")
        ("Start with editor keymap, where editor is one of:\n" ++
                (concat . intersperse ", ") (M.keys editorFM))
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

    EditorNm e -> case M.lookup (map toLower e) editorFM of
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

    -- ignore
    flip mapM_ [sigPIPE, sigALRM]
               (\sig -> installHandler sig Ignore Nothing)

    -- and exit if we get the following:
    -- we have to do our own quitE here.
    flip mapM_ [sigINT, sigHUP, sigABRT, sigTERM] $ \sig -> do
            installHandler sig (CatchOnce $ do
                    releaseSignals
                    UI.end =<< Editor.readEditor Editor.ui
                    Editor.shutdown
                    throwTo tid (ExitException (ExitFailure 2))) Nothing

releaseSignals :: IO ()
releaseSignals =
    flip mapM_ [sigINT, sigPIPE, sigHUP, sigABRT, sigTERM]
               (\sig -> installHandler sig Default Nothing)

-- ---------------------------------------------------------------------
-- | The "g_settings" var stores the user-configurable information. It is
-- initialised with the default settings, so it works even if the user
-- doesnt provide a ~/.yi/x.hs, or stuffs up their config scripts in
-- some way. The second component is our reboot function, passed in from
-- Boot.hs, otherwise return ().
--
g_settings :: IORef (Editor.Config
                    ,Maybe Editor.Editor -> IO ()
                    ,IO (Maybe Editor.Config))

g_settings = unsafePerformIO $
                newIORef (dflt_config
                         ,static_main
                         ,return (Just dflt_config))
{-# NOINLINE g_settings #-}

--
-- | default values to use if no ~/.yi/Config.hs is found
--
dflt_config :: Editor.Config
dflt_config = Editor.Config {
        Editor.keymap = Vim.keymap,
        Editor.style  = Style.uiStyle
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
static_main :: (Maybe Editor.Editor) -> IO ()
static_main st = do
    args    <- getArgs
    mfiles  <- do_args args
    config  <- readIORef g_settings
    lineno  <- readIORef g_lineno

    --
    -- The only way out is by throwing an exception, or an external
    -- signal. Is this alright?
    --
    -- catch any exception thrown by the main loop, clean up and quit
    -- (catching an ExitException), then re throw the original
    -- exception. catch that and print it, if it wasn't exit.
    --
    -- If we've rebooted, then we have two levels of catch wrapped
    -- around. (is this still true? -- 04/05)
    --
    Control.Exception.catch
        (initSignals >> initDebug ".yi.dbg" >> Core.startE st config lineno mfiles )
        (\e -> do releaseSignals
                  Editor.shutdown
                  when (not $ isExitCall e) $ print e
                  throw e)

    where
      isExitCall (ExitException _) = True
      isExitCall _ = False

--
-- | Dynamic main. This is jumped to from from Boot.hs, after
-- dynamically loading HSyi.o. It takes in user preferences, an old
-- state, and some dynamic loading funcitons, updates a global variable
-- if any settings were received, then jumps to main.
--
type DynamicT = (Maybe Editor.Editor,          -- old state
                 Maybe Editor.Config,          -- config data
                 Maybe Editor.Editor -> IO (), -- reboot function
                 IO (Maybe Editor.Config))     -- reload function

dynamic_main :: DynamicT -> IO ()
dynamic_main (st, Nothing, fn1, fn2) = do
    modifyIORef g_settings $ \(kb,_,_) -> (kb,fn1,fn2)
    static_main st          -- No prefs found, use defaults

dynamic_main (st, Just cfg, fn1, fn2) = do
    writeIORef g_settings (cfg, fn1, fn2)
    static_main st
