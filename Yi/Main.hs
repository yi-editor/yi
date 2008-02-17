
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

module Yi.Main (main, defaultConfig) where

import Prelude hiding (error)

import qualified Yi.Keymap.Emacs  as Emacs
import qualified Yi.Keymap.Vim  as Vim
import qualified Yi.Keymap.Users.Ertai
import Yi.UI.Common (UIBoot)
import Yi.Debug
import Yi.Yi hiding (file, yiConfig)

import Yi.Interact hiding (write)
import qualified Yi.Interact as I

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

import Control.Monad.Error
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

data Err = Err String ExitCode

instance Error Err where
    strMsg s = Err s (ExitFailure 1)

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
editors :: [(String,Keymap)]
editors = [("emacs", Emacs.keymap),
           ("vim", Vim.keymap),
           ("users/ertai", Yi.Keymap.Users.Ertai.keymap)]

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
                (concat . intersperse ", " . map fst) editors)
    ]

-- | usage string.
usage, versinfo :: String
usage    = usageInfo "Usage: yi [option...] [file]" options

versinfo = "yi 0.4.0"
-- TODO: pull this out of the cabal configuration

nilKeymap :: Keymap
nilKeymap = do c <- anyEvent
               case eventToChar c of
                         'e' -> forever Emacs.keymap
                         'v' -> forever Vim.keymap
                         'q' -> write $ quitEditor
                         'r' -> write $ reloadEditor
                         'h' -> write $ configHelp
                         _   -> write $ errorEditor $ "Keymap not defined, 'q' to quit, 'h' for help."
    where configHelp = withEditor $ newBufferE "*configuration help*" $ unlines $
                         ["To get a standard reasonable keymap, you can run yi with either --as=vim or --as=emacs.",
                          "you can type 'e' or 'v' now to get a temporary emacs or vim keymap.",
                          "You should however create your own ~/.yi/yi.hs file: ",
                          "start by copying it from the examples directory and edit it."]
                         -- TODO: create the default file and open it in the editor.


defaultConfig :: Config
defaultConfig = 
  Config { startFrontEnd    = snd (head frontends)
         , defaultKm        = nilKeymap
         , startAction      = openScratchBuffer -- emacs-style behaviour
         , startQueuedActions = []
         , publishedActions = Yi.Yi.defaultPublishedActions
         }

openScratchBuffer :: YiM ()
openScratchBuffer = withEditor $ do     -- emacs-like behaviour
      newBufferE "*scratch*"
                   ("-- This buffer is for notes you don't want to save, and for haskell evaluation\n" ++
                    "-- If you want to create a file, open that file,\n" ++
                    "-- then enter the text in that file's own buffer.\n\n")
      return ()

startConsole :: YiM ()
startConsole = do
  console <- withEditor $ getBufferWithName "*console*"
  setBufferKeymap console (consoleKeymap <||)

-- | Transform the config with options
do_args :: Config -> [String] -> Either Err Config
do_args cfg args =
    case (getOpt (ReturnInOrder File) options args) of
        (o, [], []) -> foldM getConfig cfg o
        (_, _, errs) -> fail (concat errs)

-- | Update the default configuration based on a command-line option.
getConfig :: Config -> Opts -> Either Err Config
getConfig cfg opt =
    case opt of
      Frontend f -> case lookup f frontends of
                      Just frontEnd -> return cfg { startFrontEnd = frontEnd }
                      Nothing       -> fail "Panic: frontend not found"
      Help          -> throwError $ Err usage ExitSuccess
      Version       -> throwError $ Err versinfo ExitSuccess
      LineNo l      -> appendAction (withBuffer (gotoLn (read l)))
      File file     -> appendAction (fnewE file)
      EditorNm emul -> case lookup (map toLower emul) editors of
             Just km -> return cfg { defaultKm = km }
             Nothing -> fail $ "Unknown emulation: " ++ show emul
      _ -> return cfg
  where appendAction a = return $ cfg { startAction = startAction cfg >> a >> return ()}

-- ---------------------------------------------------------------------
-- | Static main. This is the front end to the statically linked
-- application, and the real front end, in a sense. 'dynamic_main' calls
-- this after setting preferences passed from the boot loader.
--
main :: Config -> IO ()
main cfg = do
#ifdef FRONTEND_COCOA
       withAutoreleasePool $ do
#endif
         args <- getArgs
         case do_args cfg args of
              Left (Err err code) -> do putStrLn err
                                        exitWith code
              Right finalCfg -> startEditor finalCfg Nothing
