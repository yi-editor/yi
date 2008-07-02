module Yi.Config where

import Control.Monad.Reader
import Data.Dynamic
import Yi.Buffer
import qualified Data.Map as M
import {-# source #-} Yi.Keymap
import {-# source #-} Yi.Editor
import Yi.Style
import Yi.Event
import {-# source #-} Yi.UI.Common

data UIConfig = UIConfig {
   configFontSize :: Maybe Int,
   configLeftSideScrollBar :: Bool,
   configAutoHideScrollBar :: Bool,
   configLineWrap :: Bool,
   configWindowFill :: !Char,       
   -- ^ char to fill empty window space with.  Usually '~' for vi-like
   -- editors, ' ' for everything else
   configStyle :: UIStyle                        -- ^ ui colours

  }

type UIBoot = UIConfig -> (Event -> IO ()) -> (Action -> IO ()) ->  Editor -> IO UI

-- | Configuration record. All Yi hooks can be set here.
data Config = Config {startFrontEnd :: UIBoot,
                      configUI :: UIConfig,
                      startActions :: [Action],
                      defaultKm :: Keymap,                      
                      modeTable :: ReaderT String Maybe AnyMode,
                      fundamentalMode :: Mode (),
                      publishedActions :: M.Map String [Dynamic],
                      debugMode :: Bool,
                      configKillringAccumulate :: !Bool 
                      -- ^ accumulate cuts automatically in killring
                     }
