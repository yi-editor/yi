module Yi.Config where

import qualified Data.Map as M
import Data.Prototype

import Yi.Buffer
import Yi.Layout
import Yi.Config.Misc
import {-# source #-} Yi.Keymap
import {-# source #-} Yi.Editor
import Data.Dynamic
import Yi.Event
import Yi.Style
import Yi.Style.Library
import {-# source #-} Yi.UI.Common
import qualified Yi.Interact as I

data UIConfig = UIConfig {
   configVtyEscDelay :: Int,       
   configFontName :: Maybe String,  -- ^ Font name, for the UI that support it.
   configFontSize :: Maybe Int,     -- ^ Font size, for the UI that support it.
   configScrollStyle ::Maybe ScrollStyle,
   -- ^ Style of scroll
   configLeftSideScrollBar :: Bool, -- ^ Should the scrollbar be shown on the left side?
   configAutoHideScrollBar :: Bool, -- ^ Hide scrollbar automatically if text fits on one page.
   configAutoHideTabBar :: Bool,    -- ^ Hide the tabbar automatically if only one tab is present
   configLineWrap :: Bool,          -- ^ Wrap lines at the edge of the window if too long to display.
   configWindowFill :: Char,
   -- ^ The char with which to fill empty window space.  Usually '~' for vi-like
   -- editors, ' ' for everything else.
   configTheme :: Theme             -- ^ UI colours
  }

configStyle :: UIConfig -> UIStyle
configStyle = extractValue . configTheme


-- | Configuration record. All Yi hooks can be set here.
data Config = Config {startFrontEnd :: UIBoot,
                      -- ^ UI to use.
                      configUI :: UIConfig,
                      -- ^ UI-specific configuration.
                      startActions :: [Action],
                      -- ^ Actions to run when the editor is started.
                      initialActions :: [Action],
                      -- ^ Actions to run after startup (after startActions) or reload.
                      defaultKm :: KeymapSet,
                      -- ^ Default keymap to use.
                      configInputPreprocess :: I.P Event Event,
                      modeTable :: [AnyMode],
                      -- ^ List modes by order of preference.
                      publishedActions :: M.Map String [Data.Dynamic.Dynamic],
                      -- ^ Actions available in the "interpreter" (akin to M-x in emacs)
                      debugMode :: Bool,
                      -- ^ Produce a .yi.dbg file with a lot of debug information.
                      configRegionStyle :: RegionStyle,
                      -- ^ Set to 'Exclusive' for an emacs-like behaviour.
                      configKillringAccumulate :: Bool,
                      -- ^ Set to 'True' for an emacs-like behaviour, where 
                      -- all deleted text is accumulated in a killring.
                      bufferUpdateHandler :: [([Update] -> BufferM ())],
                      layoutManagers :: [AnyLayoutManager]
                      -- ^ List of layout managers for 'cycleLayoutManagersNext'
                     }

configFundamentalMode :: Config -> AnyMode
configFundamentalMode = last . modeTable

configTopLevelKeymap :: Config -> Keymap
configTopLevelKeymap = extractTopKeymap . defaultKm

type UIBoot = Config -> (Event -> IO ()) -> ([Action] -> IO ()) ->  Editor -> IO UI
