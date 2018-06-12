{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Yi.Config.Default (defaultConfig) where


import           Lens.Micro.Platform ((.~))
import qualified Data.HashMap.Strict as HM
import           Data.Monoid
import           Paths_yi_core
import           System.FilePath

import           Yi.Buffer
import           Yi.Command          (cabalBuildE, cabalConfigureE, grepFind,
                                      makeBuild, reloadProjectE, searchSources,
                                      shell)
import           Yi.Config
import           Yi.Core             (errorEditor, quitEditor)
import           Yi.Editor
import           Yi.Eval             (publishedActions)
import           Yi.File
import qualified Yi.Interact         as I
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.Layout
import           Yi.Mode.Common      (fundamentalMode)
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
           , configLineNumbers = False
           }
         , defaultKm        = modelessKeymapSet nilKeymap
         , startActions     = mempty
         , initialActions   = mempty
         , modeTable = [AnyMode fundamentalMode]
         , debugMode = False
         , configKillringAccumulate = False
         , configCheckExternalChangesObsessively = True
         , configRegionStyle = Exclusive
         , configInputPreprocess = I.idAutomaton
         , bufferUpdateHandler = mempty
         , layoutManagers = [hPairNStack 1, vPairNStack 1, tall, wide]
         , configVars = mempty
         , configDisableSystemClipboard = False
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
