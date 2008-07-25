
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

module Yi.Main (main, defaultConfig, projectName) where

import Prelude ()
import Yi.Prelude
import qualified Yi.Keymap.Emacs  as Emacs
import qualified Yi.Keymap.Vim  as Vim
import qualified Yi.Keymap.Cua  as Cua
import Yi.Modes (defaultModeMap, defaultFundamentalMode)
import Yi.Buffer hiding (file)
import Yi.Buffer.HighLevel
import Yi.Buffer.Normal
import Yi.Config
import Yi.Core
import Yi.Debug
import Yi.Dired
import Yi.Editor
import Yi.Keymap.Keys
import Yi.File
import Yi.Style
import Data.Dynamic
import Yi.Keymap.Emacs.Utils
import Yi.Keymap.Keys
import HConf (hconfOptions)
import Paths_yi
import Distribution.Text (display)
#ifdef TESTING
import qualified TestSuite
#endif

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
#ifdef FRONTEND_PANGO
import qualified Yi.UI.Pango
#endif

import Data.Char
import Data.List                ( intersperse, map )
import qualified Data.Map as M
import Control.Monad.Error
import Control.Applicative
import System.Console.GetOpt
import System.Directory
import System.Environment       ( getArgs )
import System.Exit
import System.FilePath
import System.IO (readFile)
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
#ifdef FRONTEND_PANGO
   ("pango", Yi.UI.Pango.start) :
#endif
   []

frontendNames :: [String]
frontendNames = fmap fst' frontends
  where fst' :: (a,UIBoot) -> a
        fst' (x,_) = x

data Err = Err String ExitCode

instance Error Err where
    strMsg s = Err s (ExitFailure 1)

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
          | Debug
          | HConfOption

-- | List of editors for which we provide an emulation.
editors :: [(String,Config -> Config)]
editors = [("emacs", \cfg -> cfg {defaultKm = Emacs.keymap, configKillringAccumulate = True}),
           ("vim",   \cfg -> cfg {defaultKm = Vim.keymap}),
           ("cua",   \cfg -> cfg {defaultKm = Cua.keymap})]

options :: [OptDescr Opts]
options = [
    Option []     ["self-check"]  (NoArg SelfCheck) "run self-checks",
    Option ['f']  ["frontend"]    (ReqArg Frontend "[frontend]")
        ("Select frontend, which can be one of:\n" ++
         (concat . intersperse ", ") frontendNames),
    Option ['y']  ["config-file"] (ReqArg ConfigFile  "path") "Specify a configuration file",
    Option ['V']  ["version"]     (NoArg Version) "Show version information",
    Option ['h']  ["help"]        (NoArg Help)    "Show this help",
    Option []     ["debug"]       (NoArg Debug)   "Write debug information in a log file",
    Option ['l']  ["line"]        (ReqArg LineNo "[num]") "Start on line number",
    Option []     ["as"]          (ReqArg EditorNm "[editor]")
        ("Start with editor keymap, where editor is one of:\n" ++
                (concat . intersperse ", " . fmap fst) editors)
    ] ++ (map (\(opt_name,desc,_) -> Option [] [opt_name] (NoArg HConfOption) desc) 
              (hconfOptions projectName)
         )

-- | usage string.
usage, versinfo :: String
usage    = usageInfo ("Usage: " ++ projectName ++ " [option...] [file]") options

projectName :: String
projectName = "yi"

versinfo = projectName ++ ' ' : display version

nilKeymap :: Keymap
nilKeymap = choice [
             char 'c' ?>>  openCfg Cua.keymap,
             char 'e' ?>>  openCfg Emacs.keymap,
             char 'v' ?>>  openCfg Vim.keymap,
             char 'q' ?>>! quitEditor,
             char 'r' ?>>! reloadEditor,
             char 'h' ?>>! configHelp
            ] 
            <|| (anyEvent >>! errorEditor "Keymap not defined, 'q' to quit, 'h' for help.")
    where configHelp = newBufferE "*configuration help*" $ fromString $ unlines $
                         ["This instance of Yi is not configured.",
                          "To get a standard reasonable keymap, you can run yi with either --as=cua, --as=vim or --as=emacs.",
                          "You should however create your own ~/.yi/yi.hs file: ",
                          "You can type 'c', 'e' or 'v' now to create and edit it using a temporary cua, emacs or vim keymap."]
          openCfg km = write $ do
            dataDir <- io $ getDataDir
            let exampleCfg = dataDir </> "examples" </> "yi.hs"
            homeDir <- io $ getHomeDirectory
            let cfgDir = homeDir </> ".yi"
                cfgFile = cfgDir </> "yi.hs"
            cfgExists <- io $ doesFileExist cfgFile
            fnewE cfgFile -- load config file
            -- locally override the keymap to the user choice
            withBuffer $ modifyMode (\m -> m {modeKeymap = const km})
            when (not cfgExists) $ do
                 -- file did not exist, load a reasonable default
                 io $ createDirectoryIfMissing True cfgDir -- so that the file can be saved.
                 defCfg <- io $ readFile exampleCfg
                 withBuffer $ insertN defCfg

defaultConfig :: Config
defaultConfig = 
  Config { startFrontEnd    = snd (head frontends)
         , configUI         =  UIConfig 
           { configFontSize = Nothing
           , configLineWrap = True
           , configLeftSideScrollBar = True
           , configAutoHideScrollBar = False
           , configWindowFill = ' '
           , configStyle = UIStyle 
             { window             = []
             , modeline           = [Foreground black,       Background darkcyan]
             , modeline_focused   = [Foreground brightwhite, Background darkcyan]
             , selected           = [Foreground Reverse,     Background Reverse]
             , eof                = [Foreground blue]
             }
           }
         , defaultKm        = nilKeymap
         , startActions     = [makeAction openScratchBuffer] -- emacs-style behaviour
         , publishedActions = defaultPublishedActions
         , modeTable = defaultModeMap
         , fundamentalMode = defaultFundamentalMode
         , debugMode = False
         , configKillringAccumulate = False
         }

-- | List of published Actions

-- THIS MUST BE OF THE FORM:
-- ("symbol", box symbol")
-- ... so we can hope getting rid of this someday.
-- Failing to conform to this rule exposes the code to instant deletion.

defaultPublishedActions :: M.Map String [Dynamic]
defaultPublishedActions = M.fromList $ 
    [ ("leftB"                  , box leftB) 
    , ("pointB"                 , box (fromPoint <$> pointB)) 
    , ("linePrefixSelectionB"  , box linePrefixSelectionB)
    , ("unLineCommentSelectionB", box unLineCommentSelectionB)
    , ("insertB"                , box insertB)
    , ("revertE"                , box revertE)
    , ("numberOfB"              , box numberOfB)
    , ("Character"              , box Character)
    , ("Line"                   , box Line)
    , ("Word"                   , box Word)
    , ("unitParagraph"          , box unitParagraph)
    , ("Document"               , box Document)
    , ("cabalConfigureE"        , box cabalConfigureE)
    , ("cabalBuildE"            , box cabalBuildE)
    , ("reloadProjectE"         , box reloadProjectE)
    , ("atBoundaryB"            , box atBoundaryB)
    , ("regionOfB"              , box regionOfB)
    , ("regionOfPartB"          , box regionOfPartB)
    , ("Forward"                , box Forward)
    , ("Backward"               , box Backward)
    , ("deleteBlankLinesB"      , box deleteBlankLinesB)
    , ("writeB"                 , box writeB)
    , ("getSelectRegionB"       , box getSelectRegionB)
    , ("closeBufferE"           , box closeBufferE)
    ]

  where box x = [toDyn x]


openScratchBuffer :: YiM ()
openScratchBuffer = withEditor $ do     -- emacs-like behaviour
      newBufferE "*scratch*" $ fromString
                   ("-- This buffer is for notes you don't want to save, and for haskell evaluation\n" ++
                    "-- If you want to create a file, open that file,\n" ++
                    "-- then enter the text in that file's own buffer.\n\n")
      return ()

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
      Debug         -> return cfg { debugMode = True }
      LineNo l      -> appendAction (withBuffer (gotoLn (read l)))
      File file     -> appendAction (fnewE file)
      EditorNm emul -> case lookup (fmap toLower emul) editors of
             Just modifyCfg -> return $ modifyCfg cfg
             Nothing -> fail $ "Unknown emulation: " ++ show emul
      _ -> return cfg
  where 
    appendAction a = return $ cfg { startActions = startActions cfg ++ [makeAction a]}

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
#ifdef TESTING
         when ("--self-check" `elem` args)
              TestSuite.main
#endif
         case do_args cfg args of
              Left (Err err code) -> do putStrLn err
                                        exitWith code
              Right finalCfg -> do when (debugMode finalCfg) $ initDebug ".yi.dbg" 
                                   startEditor finalCfg Nothing
