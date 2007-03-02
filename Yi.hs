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

module Yi (static_main) where

import Prelude hiding (error)

import Yi.Version                       ( package, version )
import qualified Yi.Editor  as Editor
import qualified Yi.Core    as Core
import qualified Yi.Style   as Style
import qualified Yi.Keymap  as Keymap

import Yi.Debug

{- All the standard editor front ends -}
import qualified Yi.Keymap.Vi     as Vi
import qualified Yi.Keymap.Vim    as Vim
import qualified Yi.Keymap.Nano   as Nano
import qualified Yi.Keymap.Emacs  as Emacs
import qualified Yi.Keymap.Joe    as Joe
import qualified Yi.Keymap.Ee     as Ee
import qualified Yi.Keymap.Mg     as Mg

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
editorFM :: M.Map [Char] (Keymap.Keymap)
editorFM = M.fromList $
    [ ("vi"      ,      Vi.keymap)
    , ("vim"     ,     Vim.keymap)
    , ("nano"    ,    Nano.keymap)
    , ("emacs"   ,   Emacs.keymap)
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
do_opt :: Opts -> IO ()
do_opt o = case o of
    Help     -> usage    >> exitWith ExitSuccess
    Version  -> versinfo >> exitWith ExitSuccess
    Libdir _ -> return ()  -- FIXME: don't ignore -B flag. 
    LineNo l -> writeIORef g_lineno ((read l) :: Int)

    EditorNm e -> case M.lookup (map toLower e) editorFM of
                    Just km -> return () -- FIXME
                    Nothing -> do
                        putStrLn $ "Unknown editor: "++show e++". Ignoring."
--
-- everything that is left over
--
do_args :: [String] -> IO (Maybe [FilePath])
do_args args =
    case (getOpt Permute options args) of
        (o, n, []) -> do
            mapM do_opt o
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
#ifdef mingw32_HOST_OS
-- Stubs, no signals in win32.
initSignals :: IO ()
initSignals = return ()
releaseSignals :: IO ()
releaseSignals = return ()

#else
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
                    -- FIXME: We should do this, but that's impossible with no access to the editor state:
                    -- UI.end =<< Editor.readEditor Editor.ui
                    -- Editor.shutdown
                    throwTo tid (ExitException (ExitFailure 2))) Nothing

releaseSignals :: IO ()
releaseSignals =
    flip mapM_ [sigINT, sigPIPE, sigHUP, sigABRT, sigTERM]
               (\sig -> installHandler sig Default Nothing)
#endif


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
        (initSignals >> initDebug ".yi.dbg" >> Core.startE st lineno mfiles )
        (\e -> do releaseSignals
                  -- FIXME: We should do this, but that's impossible with no access to the editor state:
                  -- Editor.shutdown
                  when (not $ isExitCall e) $ print e
                  throw e)

    where
      isExitCall (ExitException _) = True
      isExitCall _ = False

