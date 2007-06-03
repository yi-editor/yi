
-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) Don Stewart 2004-5.
-- Copyright (c) Jean-Philippe Bernardy 2006,2007.
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

module Yi.Main (main, Kernel) where

import Prelude hiding (error)

import qualified Yi.Buffer  as Buffer
import qualified Yi.Core    as Core
import qualified Yi.Keymap  as Keymap
import qualified Yi.Eval    as Eval
import Yi.Kernel
import Yi.Debug
import Yi.String

import Yi.Interact hiding (Interact, runProcess, write)

import Data.Char
import Data.List                ( intersperse )

import Control.Monad            ( when )
import Control.Monad.Trans      ( lift )
import Control.Concurrent       ( myThreadId, throwTo )
import Control.Exception        ( catch, throw )

import System.Console.GetOpt
import System.Environment       ( getArgs )
import System.Exit
import System.Posix.Signals

import GHC.Exception            ( Exception(ExitException) )

#include "ghcconfig.h"

-- ---------------------------------------------------------------------
-- | Argument parsing. Pretty standard.

data Opts = Help
          | Version
          | OptIgnore String
          | LineNo String
          | EditorNm String
          | File String

-- | List of editors for which we provide an emulation.
editors :: [String]
editors = ["vi", "vim", "nano", "emacs", "joe", "ee", "mg"]

editorToKeymap :: String -> String
editorToKeymap (c:cs) = "Yi.Keymap." ++ toUpper c : map toLower cs ++ ".keymap"
editorToKeymap [] = []

moduleNameOf :: String -> String
moduleNameOf s = concat $ intersperse "." $ init $ split "." $ s

options :: [OptDescr Opts]
options = [
    Option ['f']  ["flavour"] (ReqArg OptIgnore "gtk|vty") "Select flavour",
    Option ['V']  ["version"] (NoArg Version) "Show version information",
    Option ['B']  ["libdir"]  (ReqArg OptIgnore "libdir") "Path to runtime libraries",
    Option ['h']  ["help"]    (NoArg Help)    "Show this help",
    Option ['l']  ["line"]    (ReqArg LineNo "[num]") "Start on line number",
    Option []     ["as"]      (ReqArg EditorNm "[editor]")
        ("Start with editor keymap, where editor is one of:\n" ++
                (concat . intersperse ", ") editors)
    ]

--
-- usage string.
--
usage, versinfo :: IO ()
usage    = putStr   $ usageInfo "Usage: yi [option...] [file]" options

versinfo = putStrLn $ "yi 0.3.0"

--
-- deal with real options
--
do_opt :: Opts -> IO (Keymap.Action)
do_opt o = case o of
    Help     -> usage    >> exitWith ExitSuccess
    Version  -> versinfo >> exitWith ExitSuccess
    OptIgnore _ -> return Core.nopE
    LineNo l -> return (Core.gotoLnE (read l))
    File file -> return (Core.fnewE file)
    EditorNm emul -> case map toLower emul `elem` editors of
                       True -> let km = editorToKeymap emul in return $ do                                  
                                 Core.execE ("loadE " ++ show (moduleNameOf km)) 
                                 Core.execE ("changeKeymapE " ++ km)
                       False -> putStrLn ("Unknown emulation: " ++ show emul) >> exitWith (ExitFailure 1)
--
-- everything that is left over
--
do_args :: [String] -> IO ([Keymap.Action])
do_args args =
    case (getOpt (ReturnInOrder File) options args) of
        (o, [], []) -> do
            mapM do_opt o
        (_, _, errs) -> do putStrLn (concat errs)
                           exitWith (ExitFailure 1)

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

startConsole :: Keymap.Action
startConsole = do 
  console <- Core.getBufferWithName "*console*"
  lift $ Buffer.setBufferKeymap console (Eval.consoleKeymap <++)

openScratchBuffer :: Keymap.Action
openScratchBuffer = do     -- emacs-like behaviour
      Core.newBufferE "*scratch*" 
                   ("-- This buffer is for notes you don't want to save, and for haskell evaluation\n" ++
                    "-- If you want to create a file, open that file,\n" ++
                    "-- then enter the text in that file's own buffer.\n\n")
      return ()

-- ---------------------------------------------------------------------
-- | Static main. This is the front end to the statically linked
-- application, and the real front end, in a sense. 'dynamic_main' calls
-- this after setting preferences passed from the boot loader.
--
-- Initialise the ui getting an initial editor state, set signal
-- handlers, then jump to ui event loop with the state.
--
main :: Kernel -> IO ()
main kernel = do
    mopts <- do_args =<< getArgs

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
        (do initSignals
            initDebug ".yi.dbg"
            Core.startE kernel Nothing (startConsole : openScratchBuffer : mopts))
        (\e -> do releaseSignals
                  -- FIXME: We should do this, but that's impossible with no access to the editor state:
                  -- Editor.shutdown
                  when (not $ isExitCall e) $ print e
                  throw e)

    where
      isExitCall (ExitException _) = True
      isExitCall _ = False

