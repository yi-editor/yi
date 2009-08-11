{-# LANGUAGE CPP #-}
-- Copyright (c) Jean-Philippe Bernardy 2006,2007,2008.

module Yi.Config.Default (defaultConfig, availableFrontends, 
                          defaultEmacsConfig, defaultVimConfig, defaultCuaConfig,
                          toVimStyleConfig, toEmacsStyleConfig, toCuaStyleConfig) where

import Control.Monad (forever)
import Data.Dynamic
import Data.Either (rights)
import Paths_yi
import Prelude ()
import System.Directory
import System.FilePath
import System.IO (readFile)
import Yi.Command (cabalBuildE, cabalConfigureE, grepFind, makeBuild, reloadProjectE, searchSources, shell)
import {-# source #-} Yi.Boot
import Yi.Config
import Yi.Core
import Yi.Dired
import Yi.File
import Yi.IReader (saveAsNewArticle)
import Yi.Mode.IReader (ireaderMode, ireadMode)
import Yi.Modes
#ifdef SCION
import Yi.Scion
#endif
import Yi.Search
import Yi.Style.Library
import qualified Data.Map as M
import qualified Yi.Keymap.Cua  as Cua
import qualified Yi.Keymap.Emacs  as Emacs
import qualified Yi.Keymap.Vim  as Vim
import qualified Yi.Mode.Abella as Abella
import qualified Yi.Mode.Haskell as Haskell
import qualified Yi.Mode.JavaScript as JS
import qualified Yi.Mode.Latex as Latex
import qualified Yi.Interact as I
import qualified Data.Rope as R

#ifdef FRONTEND_VTY
import qualified Yi.UI.Vty
#endif
#ifdef FRONTEND_COCOA
import qualified Yi.UI.Cocoa
#endif
#ifdef FRONTEND_PANGO
import qualified Yi.UI.Pango
#endif
#ifdef FRONTEND_GTK
import qualified Yi.UI.Gtk
#endif
import qualified Yi.UI.Batch

availableFrontends :: [(String, UIBoot)]
availableFrontends =
#ifdef FRONTEND_VTY
   ("vty", Yi.UI.Vty.start) :
#endif
#ifdef FRONTEND_COCOA
   ("cocoa", Yi.UI.Cocoa.start) :
#endif
#ifdef FRONTEND_PANGO
   ("pango", Yi.UI.Pango.start) :
#endif
#ifdef FRONTEND_GTK
   ("gtk", Yi.UI.Gtk.start) :
#endif
   ("batch", Yi.UI.Batch.start) :
   []

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
    , ("iread"                  , box ireadMode)
    , ("ireadSaveAsArticle"     , box saveAsNewArticle)
    , ("leftB"                  , box leftB)
    , ("linePrefixSelectionB"   , box linePrefixSelectionB)
    , ("lineStreamB"            , box lineStreamB)
    , ("mkRegion"               , box mkRegion)
    , ("makeBuild"              , box makeBuild)
    , ("moveB"                  , box moveB)
    , ("numberOfB"              , box numberOfB)
    , ("pointB"                 , box pointB) 
    , ("regionOfB"              , box regionOfB)
    , ("regionOfPartB"          , box regionOfPartB)
    , ("regionOfPartNonEmptyB"  , box regionOfPartNonEmptyB)
    , ("reloadEditor"           , box reload)
    , ("reloadProjectE"         , box reloadProjectE)
    , ("replaceString"          , box replaceString)
    , ("revertE"                , box revertE)
    , ("shell"                  , box shell)
    , ("searchSources"          , box searchSources)
    , ("setAnyMode"             , box setAnyMode)
    , ("sortLines"              , box sortLines)
    , ("unLineCommentSelectionB", box unLineCommentSelectionB)
    , ("unitParagraph"          , box unitParagraph)
    , ("unitViWord"             , box unitViWord)
    , ("writeB"                 , box writeB)
    , ("ghciGet"                , box Haskell.ghciGet)
    , ("abella"                 , box Abella.abella)
#ifdef SCION
    , ("scion"                  , box scion)
#endif
    ]

  where box x = [Data.Dynamic.toDyn x]


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
           , configAutoHideTabBar = True
           , configWindowFill = ' '
           , configTheme = defaultLightTheme
           , configVtyEscDelay = 0
           }
         , defaultKm        = modelessKeymapSet nilKeymap
         , startActions     = []
         , publishedActions = defaultPublishedActions
         , modeTable = [AnyMode Haskell.cleverMode,
                        AnyMode Haskell.preciseMode,
                        AnyMode Latex.latexMode3,
                        AnyMode Latex.fastMode,
                        AnyMode Abella.abellaModeEmacs,
                        AnyMode cMode,
                        AnyMode objectiveCMode,
                        AnyMode cppMode,
                        AnyMode Haskell.literateMode,
                        AnyMode cabalMode,
                        AnyMode gnuMakeMode,
                        AnyMode srmcMode,
                        AnyMode ocamlMode,
                        AnyMode ottMode,
                        AnyMode perlMode,
                        AnyMode (JS.hooks JS.javaScriptMode),
                        AnyMode pythonMode,
                        AnyMode ireaderMode,
                        AnyMode svnCommitMode,
                        AnyMode whitespaceMode,
                        AnyMode fundamentalMode]
         , debugMode = False
         , configKillringAccumulate = False
         , configRegionStyle = Exclusive
         , configInputPreprocess = I.idAutomaton
         , bufferUpdateHandler = []
         }

defaultEmacsConfig, defaultVimConfig, defaultCuaConfig :: Config
defaultEmacsConfig = toEmacsStyleConfig defaultConfig
defaultVimConfig = toVimStyleConfig defaultConfig
defaultCuaConfig = toCuaStyleConfig defaultConfig

toEmacsStyleConfig, toVimStyleConfig, toCuaStyleConfig :: Config -> Config
toEmacsStyleConfig cfg 
    = cfg {
            configUI = (configUI cfg) { configVtyEscDelay = 1000 },
            defaultKm = Emacs.keymap,
            startActions = makeAction openScratchBuffer : startActions cfg,
            configInputPreprocess = escToMeta,
            configKillringAccumulate = True
          }

-- | Input preprocessor: Transform Esc;Char into Meta-Char
-- Useful for emacs lovers ;)
escToMeta :: I.P Event Event
escToMeta = mkAutomaton $ forever $ (anyEvent >>= I.write) ||> do
    event (spec KEsc)
    c <- printableChar
    I.write (Event (KASCII c) [MMeta])

toVimStyleConfig cfg = cfg { defaultKm = Vim.keymapSet
                           , configRegionStyle = Inclusive
                           , modeTable = AnyMode Abella.abellaModeVim : modeTable cfg }
toCuaStyleConfig cfg = cfg {defaultKm = Cua.keymap}

-- | Open an emacs-like scratch buffer if no file is open.
openScratchBuffer :: YiM ()
openScratchBuffer = withEditor $ do 
      noFileBufOpen <- null . rights . fmap (getVal identA) . M.elems <$> getA buffersA
      when noFileBufOpen $ do
           newBufferE (Left "scratch") $ R.fromString $ unlines
                   ["This buffer is for notes you don't want to save.", --, and for haskell evaluation" -- maybe someday?
                    "If you want to create a file, open that file,",
                    "then enter the text in that file's own buffer."]
           return ()


nilKeymap :: Keymap
nilKeymap = choice [
             char 'c' ?>>  openCfg (extractTopKeymap Cua.keymap),
             char 'e' ?>>  openCfg (extractTopKeymap Emacs.keymap),
             char 'v' ?>>  openCfg (extractTopKeymap Vim.keymapSet),
             char 'q' ?>>! quitEditor,
             char 'r' ?>>! reload,
             char 'h' ?>>! configHelp
            ] 
            <|| (anyEvent >>! errorEditor "Keymap not defined, 'q' to quit, 'h' for help.")
    where configHelp = newBufferE (Left "configuration help") $ R.fromString $ unlines $
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
            io $ createDirectoryIfMissing True cfgDir -- so that the file can be saved.
            fnewE cfgFile -- load config file
            -- locally override the keymap to the user choice
            withBuffer $ modifyMode (\m -> m {modeKeymap = const km})
            when (not cfgExists) $ do
                 -- file did not exist, load a reasonable default
                 defCfg <- io $ readFile exampleCfg
                 withBuffer $ insertN defCfg

