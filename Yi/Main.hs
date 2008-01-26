
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

import qualified Yi.Core    as Core
import qualified Yi.Buffer  as Buffer
import qualified Yi.Keymap  as Keymap
import qualified Yi.Eval    as Eval
import Yi.UI.Common (UIBoot)
import Yi.Kernel
import Yi.Debug
import Yi.String

import Yi.Interact hiding (write)

#ifdef FRONTEND_COCOA
import qualified Yi.UI.Cocoa
import Foundation (withAutoreleasePool)
#endif
#ifdef FRONTEND_GTK
import qualified Yi.UI.Gtk
#endif
#ifdef FRONTEND_VTY
import qualified Yi.UI.Vty
#endif

import Data.Char
import Data.List                ( intersperse )

import System.Console.GetOpt
import System.Environment       ( getArgs )
import System.Exit

#include "ghcconfig.h"


frontends :: [(String,UIBoot)]
frontends =
#ifdef FRONTEND_COCOA
   ("cocoa", Yi.UI.Cocoa.start) :
#endif
#ifdef FRONTEND_GTK
   ("gtk", Yi.UI.Gtk.start) :
#endif
#ifdef FRONTEND_VTY
   ("vty", Yi.UI.Vty.start) :
#endif
   []

frontendNames :: [String]
frontendNames = map fst' frontends
  where fst' :: (a,UIBoot) -> a
        fst' (x,_) = x

-- ---------------------------------------------------------------------
-- | Argument parsing. Pretty standard.

data Opts = Help
          | Version
          | OptIgnore String
          | LineNo String
          | EditorNm String
          | File String
          | Frontend String
          | ConfigFile String

-- | List of editors for which we provide an emulation.
editors :: [String]
editors = ["vi", "vim", "nano", "emacs", "joe", "ee", "mg"]

editorToKeymap :: String -> String
editorToKeymap s = "Yi.Keymap." ++ capitalize s ++ ".keymap"

moduleNameOf :: String -> String
moduleNameOf s = concat $ intersperse "." $ init $ split "." $ s

options :: [OptDescr Opts]
options = [
    Option ['f']  ["frontend"]    (ReqArg Frontend "[frontend]")
        ("Select frontend, which can be one of:\n" ++
         (concat . intersperse ", ") frontendNames),
    Option ['y']  ["config-file"] (ReqArg ConfigFile  "path") "Specify a configuration file",
    Option ['V']  ["version"]     (NoArg Version) "Show version information",
    Option ['B']  ["libdir"]      (ReqArg OptIgnore "libdir") "Path to runtime libraries",
    Option ['b']  ["bindir"]      (ReqArg OptIgnore "bindir") "Path to runtime library binaries\n(default: libdir)",
    Option ['h']  ["help"]        (NoArg Help)    "Show this help",
    Option ['l']  ["line"]        (ReqArg LineNo "[num]") "Start on line number",
    Option []     ["as"]          (ReqArg EditorNm "[editor]")
        ("Start with editor keymap, where editor is one of:\n" ++
                (concat . intersperse ", ") editors)
    ]

-- | usage string.
usage, versinfo :: IO ()
usage    = putStr   $ usageInfo "Usage: yi [option...] [file]" options

versinfo = putStrLn $ "yi 0.3.0"
-- TODO: pull this out of the cabal configuration

-- | deal with real options
do_opt :: Opts -> IO (Keymap.YiM ())
do_opt o = case o of
    Frontend f     -> case map toLower f `elem` frontendNames of
                        False -> do putStrLn ("Unknown frontend: " ++ show f)
                                    exitWith (ExitFailure 1)
                        _     -> return (return ()) -- Processed differently
    OptIgnore _   -> return (return ())
    ConfigFile _  -> return (return ())
    Help          -> usage    >> exitWith ExitSuccess
    Version       -> versinfo >> exitWith ExitSuccess
    LineNo l      -> return (Keymap.withBuffer (Buffer.gotoLn (read l)) >> return ())
    File file     -> return (Core.fnewE file)
    EditorNm emul -> case map toLower emul `elem` editors of
                       True -> let km = editorToKeymap emul in return $ do
                                 Core.execE ("loadE " ++ show (moduleNameOf km))
                                 Core.execE ("changeKeymapE " ++ km)
                       False -> do putStrLn ("Unknown emulation: " ++ show emul)
                                   exitWith (ExitFailure 1)

-- | everything that is left over
do_args :: [String] -> IO (Core.StartConfig, [Keymap.YiM ()])
do_args args =
    case (getOpt (ReturnInOrder File) options args) of
        (o, [], []) ->  do
            let frontend = head $ [f | Frontend   f <- o] ++ [head frontendNames]
                yiConfig = head $ [f | ConfigFile f <- o] ++ ["YiConfig"]
                config   = Core.StartConfig { Core.startFrontEnd   = case lookup frontend frontends of
                                                                       Nothing -> error "Panic: frontend not found"
                                                                       Just x -> x
                                            , Core.startConfigFile = yiConfig
                                            }
            actions <- mapM do_opt o
            return (config, actions)
        (_, _, errs) -> do putStrLn (concat errs)
                           exitWith (ExitFailure 1)

startConsole :: Keymap.YiM ()
startConsole = do
  console <- Core.getBufferWithName "*console*"
  Keymap.setBufferKeymap console (Eval.consoleKeymap <||)

openScratchBuffer :: Keymap.YiM ()
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
main :: Kernel -> IO ()
#ifdef FRONTEND_COCOA
main kernel = withAutoreleasePool $ do
#else
main kernel = do
#endif
    (config, mopts) <- do_args =<< getArgs
    Core.startE config kernel Nothing (startConsole : openScratchBuffer : mopts)
