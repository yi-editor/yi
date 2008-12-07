{-# LANGUAGE CPP #-}
-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) Don Stewart 2004-5.
-- Copyright (c) Jean-Philippe Bernardy 2006,2007.

-- | This is the main module of Yi, called with configuration from the user.
-- Here we mainly process command line arguments.

module Yi.Main (main, defaultConfig, availableFrontends, projectName) where

import Prelude ()
import qualified Yi.Keymap.Emacs  as Emacs
import qualified Yi.Keymap.Vim  as Vim
import qualified Yi.Keymap.Cua  as Cua
import Yi.Modes
import qualified Yi.Mode.Haskell as Haskell
import Yi.Mode.IReader (ireaderMode, ireadMode)
import Yi.IReader (nextArticle, saveAsNewArticle)
import qualified Yi.Mode.Latex as Latex
import {-# source #-} Yi.Boot
import Yi.Config
import Yi.Core
import Yi.Dired
import Yi.File
import Yi.Misc
import Yi.Style.Library
import Data.Dynamic
import Yi.Keymap.Emacs.Utils
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
import System.Console.GetOpt
import System.Directory
import System.Environment       ( getArgs )
import System.Exit
import System.FilePath
import System.IO (readFile)
#include "ghcconfig.h"


availableFrontends :: [(String,UIBoot)]
availableFrontends =
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
frontendNames = fmap fst' availableFrontends
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
usage = usageInfo ("Usage: " ++ projectName ++ " [option...] [file]") options

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
  Config { startFrontEnd    = case availableFrontends of
             [] -> error "panic: no frontend compiled in! (configure with -fvty or another frontend.)"
             ((_,f):_) -> f
         , configUI         =  UIConfig 
           { configFontSize = Nothing
           , configFontName = Nothing
           , configLineWrap = True
           , configLeftSideScrollBar = True
           , configAutoHideScrollBar = False
           , configWindowFill = ' '
           , configTheme = defaultLightTheme
           }
         , defaultKm        = nilKeymap
         , startActions     = [makeAction openScratchBuffer] -- emacs-style behaviour
         , publishedActions = defaultPublishedActions
         , modeTable = [AnyMode Haskell.cleverMode,
                        AnyMode Latex.latexMode2,
                        AnyMode Latex.latexMode, -- available but the other one is preferred
                        AnyMode cppMode,
                        AnyMode Haskell.literateMode,
                        AnyMode cabalMode,
                        AnyMode gnuMakeMode,
                        AnyMode srmcMode,
                        AnyMode ocamlMode,
                        AnyMode ottMode,
                        AnyMode perlMode,
                        AnyMode pythonMode,
                        AnyMode ireaderMode,
                        AnyMode fundamentalMode]
         , debugMode = False
         , configKillringAccumulate = False
         }

-- | List of published Actions

-- THIS MUST BE OF THE FORM:
-- ("symbol", box symbol")
-- ... so we can hope getting rid of this someday.
-- Failing to conform to this rule exposes the code to instant deletion.

defaultPublishedActions :: M.Map String [Data.Dynamic.Dynamic]
defaultPublishedActions = M.fromList $ 
    [ ("Backward"               , box Backward)
    , ("Character"              , box Character)
    , ("Document"               , box Document)
    , ("Forward"                , box Forward)
    , ("Line"                   , box Line)
    , ("unitWord"               , box unitWord)
    , ("Point"                  , box Point)
    , ("atBoundaryB"            , box atBoundaryB)
    , ("cabalBuildE"            , box cabalBuildE)
    , ("cabalConfigureE"        , box cabalConfigureE)
    , ("closeBufferE"           , box closeBufferE)
    , ("deleteB"                , box deleteB)
    , ("deleteBlankLinesB"      , box deleteBlankLinesB)
    , ("getSelectRegionB"       , box getSelectRegionB)
    , ("grepFind"               , box grepFind)
    , ("insertB"                , box insertB)
    , ("iread"                  , box (withBuffer ireadMode >> nextArticle))
    , ("ireadSaveAsArticle"     , box saveAsNewArticle)
    , ("leftB"                  , box leftB)
    , ("linePrefixSelectionB"   , box linePrefixSelectionB)
    , ("mkRegion"               , box mkRegion)
    , ("moveB"                  , box moveB)
    , ("numberOfB"              , box numberOfB)
    , ("pointB"                 , box pointB) 
    , ("regionOfB"              , box regionOfB)
    , ("regionOfPartB"          , box regionOfPartB)
    , ("regionOfPartNonEmptyB"  , box regionOfPartNonEmptyB)
    , ("reloadEditor"           , box reloadEditor)
    , ("reloadProjectE"         , box reloadProjectE)
    , ("replaceString"          , box replaceString)
    , ("revertE"                , box revertE)
    , ("shell"                  , box shell)
    , ("setAnyMode"             , box setAnyMode)
    , ("sortLines"              , box sortLines)
    , ("unLineCommentSelectionB", box unLineCommentSelectionB)
    , ("unitParagraph"          , box unitParagraph)
    , ("unitViWord"             , box unitViWord)
    , ("writeB"                 , box writeB)
    , ("ghci"                   , box Haskell.ghciGet)
    ]

  where box x = [Data.Dynamic.toDyn x]


openScratchBuffer :: YiM ()
openScratchBuffer = withEditor $ do     -- emacs-like behaviour
      newBufferE "*scratch*" $ fromString $ unlines
                   ["This buffer is for notes you don't want to save.", --, and for haskell evaluation" -- maybe someday?
                    "If you want to create a file, open that file,",
                    "then enter the text in that file's own buffer."]
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
      Frontend f -> case lookup f availableFrontends of
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
main :: Config -> Maybe Editor -> IO ()
main cfg state = do
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
                                   startEditor finalCfg state
