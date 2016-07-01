{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Yi.Config.Default (defaultConfig) where

import           Control.Lens        ((.~), (^.), use)
import           Control.Monad
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import           Data.Monoid
import           Paths_yi_core
import           System.FilePath

import           Yi.Buffer
import           Yi.Command          (cabalBuildE, cabalConfigureE, grepFind,
                                      makeBuild, reloadProjectE, searchSources,
                                      shell)
import           Yi.Config
import           Yi.Config.Misc
import           Yi.Core             (errorEditor, quitEditor)
import           Yi.Editor
import           Yi.Eval             (publishedActions)
import           Yi.File
import qualified Yi.Interact         as I
import           Yi.IReader          (saveAsNewArticle)
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.Layout
import qualified Yi.Mode.Abella      as Abella
import qualified Yi.Mode.Haskell     as Haskell
import           Yi.Mode.IReader     (ireadMode, ireaderMode)
import qualified Yi.Mode.JavaScript  as JS
import qualified Yi.Mode.Latex       as Latex
import           Yi.Modes
import qualified Yi.Rope             as R
import           Yi.Search
import           Yi.Style.Library
import           Yi.Utils

import           Yi.Types            ()

-- | List of published Actions

-- THIS MUST BE OF THE FORM:
-- ("symbol", box symbol")
-- ... so we can hope getting rid of this someday.
-- Failing to conform to this rule exposes the code to instant deletion.
--
-- TODO: String → Text/YiString
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
    ]

  where
    box :: (Show x, YiAction a x) => a -> Action
    box = makeAction


defaultConfig :: Config
defaultConfig =
  publishedActions .~ defaultPublishedActions $
  Config { startFrontEnd    = error "panic: no frontend compiled in! (configure with -fvty or another frontend.)"
         , configUI         =  UIConfig
           { configFontSize = Just 10
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
                        AnyMode clojureMode,
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
         , configVars = mempty
         }

nilKeymap :: Keymap
nilKeymap = choice [
             char 'q' ?>>! quitEditor,
             char 'h' ?>>! configHelp
            ]
            <|| (anyEvent >>! errorEditor "Keymap not defined, 'q' to quit, 'h' for help.")
    where
      configHelp :: YiM ()
      configHelp = do
        dataDir <- io getDataDir
        let x <//> y = R.fromString (x </> y)
            welcomeText = R.unlines
              [ "This instance of Yi is not configured."
              , ""
              , "To get a standard reasonable keymap, you can run yi with"
              , "either --as=cua, --as=vim or --as=emacs."
              , ""
              , "You should however create your own ~/.config/yi/yi.hs file."
              , "As a starting point it's recommended to use one of the configs"
              , "from " <> (dataDir <//> "example-configs/")
              , ""
              ]
        withEditor_ $ newBufferE (MemBuffer "configuration help") welcomeText
