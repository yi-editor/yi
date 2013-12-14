{-# LANGUAGE CPP #-}
-- Copyright (c) Jean-Philippe Bernardy 2006,2007,2008.

module Yi.Config.Default (defaultConfig, availableFrontends,
                          defaultEmacsConfig, defaultVimConfig, defaultCuaConfig,
                          toVimStyleConfig, toVim2StyleConfig, toEmacsStyleConfig, toCuaStyleConfig) where

import Control.Monad (forever)
import Data.Either (rights)
import Paths_yi
import Prelude ()
import System.Directory
import System.FilePath
import System.IO (readFile)
import Yi.Command (cabalBuildE, cabalConfigureE, grepFind, makeBuild, reloadProjectE, searchSources, shell)
import {-# source #-} Yi.Boot
import Yi.Config
import Yi.Config.Misc
import Yi.Paths(getConfigFilename)
import Yi.Core
import Yi.Eval(publishedActions)
import Yi.File
import Yi.IReader (saveAsNewArticle)
import Yi.Mode.IReader (ireaderMode, ireadMode)
import Yi.Layout
import Yi.Modes
#ifdef SCION
import Yi.Scion
#endif
import Yi.Search
import Yi.Style.Library
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Yi.Keymap.Cua  as Cua
import qualified Yi.Keymap.Emacs  as Emacs
import qualified Yi.Keymap.Vim  as Vim
import qualified Yi.Keymap.Vim2  as Vim2
import qualified Yi.Mode.Abella as Abella
import qualified Yi.Mode.Haskell as Haskell
import qualified Yi.Mode.JavaScript as JS
import qualified Yi.Mode.Latex as Latex
import qualified Yi.Interact as I
import qualified Data.Rope as R

#ifdef FRONTEND_VTE
import qualified Yi.UI.Vte
#endif
#ifdef FRONTEND_VTY
import qualified Yi.UI.Vty
#endif
#ifdef FRONTEND_PANGO
import qualified Yi.UI.Pango
#endif
#ifdef FRONTEND_COCOA
import qualified Yi.UI.Cocoa
#endif
import qualified Yi.UI.Batch

availableFrontends :: [(String, UIBoot)]
availableFrontends =
#ifdef FRONTEND_VTE
   ("vte", Yi.UI.Vte.start) :
#endif
#ifdef FRONTEND_VTY
   ("vty", Yi.UI.Vty.start) :
#endif
#ifdef FRONTEND_PANGO
   ("pango", Yi.UI.Pango.start) :
#endif
#ifdef FRONTEND_COCOA
   ("cocoa", Yi.UI.Cocoa.start) :
#endif
   ("batch", Yi.UI.Batch.start) :
   []

-- | List of published Actions

-- THIS MUST BE OF THE FORM:
-- ("symbol", box symbol")
-- ... so we can hope getting rid of this someday.
-- Failing to conform to this rule exposes the code to instant deletion.

defaultPublishedActions :: HM.HashMap String Action
defaultPublishedActions = HM.fromList $
    [
      ("atBoundaryB"            , box atBoundaryB)
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
--    , ("mkRegion"               , box mkRegion) -- can't make 'instance Promptable Region'
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
    , ("writeB"                 , box writeB)
    , ("ghciGet"                , box Haskell.ghciGet)
    , ("abella"                 , box Abella.abella)
#ifdef SCION
    , ("scion"                  , box scion)
#endif
    ]

  where
    box :: (Show x, YiAction a x) => a -> Action
    box = makeAction


defaultConfig :: Config
defaultConfig =
  publishedActions ^= defaultPublishedActions $
  Config { startFrontEnd    = case availableFrontends of
             [] -> error "panic: no frontend compiled in! (configure with -fvty or another frontend.)"
             ((_,f):_) -> f
         , configUI         =  UIConfig
           { configFontSize = Nothing
           , configFontName = Nothing
           , configScrollWheelAmount = 4
           , configScrollStyle = Nothing
           , configCursorStyle = FatWhenFocusedAndInserting
           , configLineWrap = True
           , configLeftSideScrollBar = True
           , configAutoHideScrollBar = False
           , configAutoHideTabBar = True
           , configWindowFill = ' '
           , configTheme = defaultTheme
           , configVtyEscDelay = 0
           }
         , defaultKm        = modelessKeymapSet nilKeymap
         , startActions     = []
         , initialActions   = []
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
                        AnyMode rubyMode,
                        AnyMode javaMode,
                        AnyMode jsonMode,
                        AnyMode ireaderMode,
                        AnyMode svnCommitMode,
                        AnyMode gitCommitMode,
                        AnyMode whitespaceMode,
                        AnyMode fundamentalMode]
         , debugMode = False
         , configKillringAccumulate = False
         , configCheckExternalChangesObsessively = True
         , configRegionStyle = Exclusive
         , configInputPreprocess = I.idAutomaton
         , bufferUpdateHandler = []
         , layoutManagers = [hPairNStack 1, vPairNStack 1, tall, wide]
         , configVars = initial
         }

defaultEmacsConfig, defaultVimConfig, defaultCuaConfig :: Config
defaultEmacsConfig = toEmacsStyleConfig defaultConfig
defaultVimConfig = toVimStyleConfig defaultConfig
defaultCuaConfig = toCuaStyleConfig defaultConfig

toEmacsStyleConfig, toVimStyleConfig, toVim2StyleConfig, toCuaStyleConfig :: Config -> Config
toEmacsStyleConfig cfg
    = cfg {
            configUI = (configUI cfg) { configVtyEscDelay = 1000 , configScrollStyle = Just SnapToCenter},
            defaultKm = Emacs.keymap,
            startActions = makeAction openScratchBuffer : startActions cfg,
            configInputPreprocess = escToMeta,
            configKillringAccumulate = True
          }

-- | Input preprocessor: Transform Esc;Char into Meta-Char
-- Useful for emacs lovers ;)
escToMeta :: I.P Event Event
escToMeta = mkAutomaton $ forever $ (anyEvent >>= I.write) ||> do
    discard $ event (spec KEsc)
    c <- printableChar
    I.write (Event (KASCII c) [MMeta])

toVimStyleConfig cfg = cfg { defaultKm = Vim.keymapSet
                           , configUI = (configUI cfg) { configScrollStyle = Just SingleLine}
                           , configRegionStyle = Inclusive
                           , modeTable = AnyMode Abella.abellaModeVim : modeTable cfg }

toVim2StyleConfig cfg = cfg { defaultKm = Vim2.keymapSet
                            , configUI = (configUI cfg) { configScrollStyle = Just SingleLine}
                            , configRegionStyle = Inclusive }

toCuaStyleConfig cfg = cfg {defaultKm = Cua.keymap}

-- | Open an emacs-like scratch buffer if no file is open.
openScratchBuffer :: YiM ()
openScratchBuffer = withEditor $ do
      noFileBufOpen <- null . rights . fmap (getVal identA) . M.elems <$> getA buffersA
      when noFileBufOpen $ do
           discard $ newBufferE (Left "scratch") $ R.fromString $ unlines
                   ["This buffer is for notes you don't want to save.", --, and for haskell evaluation" -- maybe someday?
                    "If you want to create a file, open that file,",
                    "then enter the text in that file's own buffer."]

nilKeymap :: Keymap
nilKeymap = choice [
             char 'c' ?>>  openCfg (Cua.keymap)    "yi-cua.hs",
             char 'e' ?>>  openCfg (Emacs.keymap)  "yi.hs",
             char 'v' ?>>  openCfg (Vim.keymapSet) "yi-vim.hs",
             char 'q' ?>>! quitEditor,
             char 'r' ?>>! reload,
             char 'h' ?>>! configHelp
            ]
            <|| (anyEvent >>! errorEditor "Keymap not defined, 'q' to quit, 'h' for help.")
    where configHelp = newBufferE (Left "configuration help") $ R.fromString $ unlines $
                         ["This instance of Yi is not configured.",
                          "To get a standard reasonable keymap, you can run yi with either --as=cua, --as=vim or --as=emacs.",
                          "You should however create your own ~/.config/yi/yi.hs file: ",
                          "You can type 'c', 'e' or 'v' now to create and edit it using a temporary cua, emacs or vim keymap."]
          openCfg km kmName = write $ do
            dataDir <- io getDataDir
            let exampleCfg = dataDir </> "example-configs" </> kmName
            cfgFile <- getConfigFilename -- automatically creates directory, if missing
            cfgExists <- io $ doesFileExist cfgFile
            discard $ editFile cfgFile -- load config file
            -- locally override the keymap to the user choice
            withBuffer $ modifyMode (\m -> m { modeKeymap = const km })
            when (not cfgExists) $ do
                -- file did not exist, load a reasonable default
                defCfg <- io $ readFile exampleCfg
                withBuffer $ insertN defCfg
--          openCfg km kmName = write $ do
--            dataDir <- io $ getDataDir
--            let exampleCfg = dataDir </> "example-configs" </> kmName
--            homeDir <- io $ getHomeDirectory
--            let cfgDir = homeDir </> ".yi"
--                cfgFile = cfgDir </> "yi.hs"
--            cfgExists <- io $ doesFileExist cfgFile
--            -- io $ print cfgExists
--            io $ createDirectoryIfMissing True cfgDir -- so that the file can be saved.
--            discard $ editFile cfgFile -- load config file
--            -- locally override the keymap to the user choice
--            withBuffer $ modifyMode (\m -> m {modeKeymap = const km})
--            when (not cfgExists) $ do
--                 -- file did not exist, load a reasonable default
--                 defCfg <- io $ readFile exampleCfg
--                 withBuffer $ insertN defCfg
