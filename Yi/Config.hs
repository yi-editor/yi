module Yi.Config where

import Control.Monad.Reader
import Data.Dynamic
import Yi.Buffer
import Yi.UI.Common
import qualified Data.Map as M
import {-# source #-} Yi.Keymap

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
