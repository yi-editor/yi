module Yi.Config where

import qualified Data.Map as M
import Data.Prototype

import Yi.Buffer
import {-# source #-} Yi.Keymap
import {-# source #-} Yi.Editor
import Data.Dynamic
import Yi.Event
import Yi.Style
import Yi.Style.Library
import {-# source #-} Yi.UI.Common

data UIConfig = UIConfig {
   configFontName :: Maybe String,  -- ^ Font name, for the UI that support it.
   configFontSize :: Maybe Int,     -- ^ Font size, for the UI that support it.
   configLeftSideScrollBar :: Bool, -- ^ Should the scrollbar be shown on the left side?
   configAutoHideScrollBar :: Bool, -- ^ Hide scrollbar automatically if text fits on one page.
   configLineWrap :: Bool,          -- ^ Wrap lines at the edge of the window if too long to display.
   configWindowFill :: !Char,
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
                      defaultKm :: Keymap,
                      -- ^ Default keymap to use.
                      modeTable :: [AnyMode],
                      -- ^ List modes by order of preference.
                      publishedActions :: M.Map String [Data.Dynamic.Dynamic],
                      -- ^ Actions available in the "interpreter" (akin to M-x in emacs)
                      debugMode :: Bool,
                      -- ^ Produce a .yi.dbg file with a lot of debug information.
                      configKillringAccumulate :: !Bool
                      -- ^ Set to 'True' for an emacs-like behaviour, where 
                      -- all deleted text is accumulated in a killring.
                     }

configFundamentalMode :: Config -> AnyMode
configFundamentalMode = last . modeTable

type UIBoot = Config -> (Event -> IO ()) -> ([Action] -> IO ()) ->  Editor -> IO UI

