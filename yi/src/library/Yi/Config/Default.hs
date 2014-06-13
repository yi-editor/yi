{-# LANGUAGE CPP #-}
-- Copyright (c) Jean-Philippe Bernardy 2006,2007,2008.

module Yi.Config.Default (defaultConfig, availableFrontends,
                          defaultEmacsConfig, defaultVimConfig, defaultCuaConfig,
                          toVimStyleConfig, toEmacsStyleConfig, toCuaStyleConfig) where

import Control.Applicative
import Control.Monad
import Control.Lens hiding (Action)
import Data.Default
import Paths_yi
import System.FilePath
import Yi.Command (cabalBuildE, cabalConfigureE, grepFind, makeBuild, reloadProjectE, searchSources, shell)
import Yi.Config
import Yi.Config.Misc
import Yi.Core
import Yi.Utils
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
import qualified Yi.Mode.Abella as Abella
import qualified Yi.Mode.Haskell as Haskell
import qualified Yi.Mode.JavaScript as JS
import qualified Yi.Mode.Latex as Latex
import qualified Yi.Interact as I
import qualified Data.Rope as R

#ifdef FRONTEND_VTY
import qualified Yi.UI.Vty
import qualified Graphics.Vty.Config as Vty
#endif
#ifdef FRONTEND_PANGO
import qualified Yi.UI.Pango
#endif
import qualified Yi.UI.Batch

{-# ANN availableFrontends "HLint: ignore Use list literal" #-}
availableFrontends :: [(String, UIBoot)]
availableFrontends =
#ifdef FRONTEND_VTY
   ("vty", Yi.UI.Vty.start) :
#endif
#ifdef FRONTEND_PANGO
   ("pango", Yi.UI.Pango.start) :
#endif
   ("batch", Yi.UI.Batch.start) :
   []

-- | List of published Actions

-- THIS MUST BE OF THE FORM:
-- ("symbol", box symbol")
-- ... so we can hope getting rid of this someday.
-- Failing to conform to this rule exposes the code to instant deletion.

defaultPublishedActions :: HM.HashMap String Action
defaultPublishedActions = HM.fromList
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
  publishedActions .~ defaultPublishedActions $
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
#ifdef FRONTEND_VTY
           , configVty = def
#endif
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
         , configVars = def
         }

defaultEmacsConfig, defaultVimConfig, defaultCuaConfig :: Config
defaultEmacsConfig = toEmacsStyleConfig defaultConfig
defaultVimConfig = toVimStyleConfig defaultConfig
defaultCuaConfig = toCuaStyleConfig defaultConfig

toEmacsStyleConfig, toVimStyleConfig, toCuaStyleConfig :: Config -> Config
toEmacsStyleConfig cfg
    = cfg {
            configUI = (configUI cfg)
                       { configScrollStyle = Just SnapToCenter
#ifdef FRONTEND_VTY
                       -- corey: does this actually matter? escToMeta appears to perform all the
                       -- meta joining required. I'm not an emacs user and cannot evaluate feel. For
                       -- me these settings join esc;key to meta-key OK. The 100 millisecond lag in
                       -- ESC is terrible for me. Maybe that's just how it is under emacs...
                       , configVty = def { Vty.vtime = Just 100, Vty.vmin = Just 2 }
#endif
                       },
            defaultKm = Emacs.keymap,
            startActions = makeAction openScratchBuffer : startActions cfg,
            configInputPreprocess = escToMeta,
            configKillringAccumulate = True
          }

-- | Input preprocessor: Transform Esc;Char into Meta-Char
-- Useful for emacs lovers ;)
escToMeta :: I.P Event Event
escToMeta = mkAutomaton $ forever $ (anyEvent >>= I.write) ||> do
    void $ event (spec KEsc)
    c <- printableChar
    I.write (Event (KASCII c) [MMeta])

toVimStyleConfig cfg = cfg { defaultKm = Vim.keymapSet
                            , configUI = (configUI cfg) { configScrollStyle = Just SingleLine}
                            , configRegionStyle = Inclusive }

toCuaStyleConfig cfg = cfg {defaultKm = Cua.keymap}

-- | Open an emacs-like scratch buffer if no file is open.
openScratchBuffer :: YiM ()
openScratchBuffer = withEditor $ do
      fileBufOpen <- any isFileOrDir . M.elems <$> use buffersA
      unless fileBufOpen $
           void $ newBufferE (Left "scratch") $ R.fromString $ unlines
                   ["This buffer is for notes you don't want to save.", --, and for haskell evaluation" -- maybe someday?
                    "If you want to create a file, open that file,",
                    "then enter the text in that file's own buffer."]
      where
        isFileOrDir :: FBuffer -> Bool
        isFileOrDir attrs = either (const $ view directoryContentA attrs) (const True) (view identA attrs)

nilKeymap :: Keymap
nilKeymap = choice [
             char 'q' ?>>! quitEditor,
             char 'h' ?>>! configHelp
            ]
            <|| (anyEvent >>! errorEditor "Keymap not defined, 'q' to quit, 'h' for help.")
    where configHelp :: YiM ()
          configHelp = do
            dataDir <- io getDataDir
            let welcomeText = unlines
                         ["This instance of Yi is not configured.",
                          "",
                          "To get a standard reasonable keymap, you can run yi with",
                          "either --as=cua, --as=vim or --as=emacs.",
                          "",
                          "You should however create your own ~/.config/yi/yi.hs file.",
                          "As a starting point it's recommended to use one of the configs",
                          "from " ++ dataDir </> "example-configs/"]
            withEditor (void (newBufferE (Left "configuration help") (R.fromString welcomeText)))
