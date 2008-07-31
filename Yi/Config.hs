module Yi.Config where

import Control.Monad.Reader
import Data.Dynamic
import qualified Data.Map as M
import {-# source #-} Yi.Buffer
import {-# source #-} Yi.Keymap
import {-# source #-} Yi.Editor
import Yi.Dynamic
import Yi.Event
import Yi.Style
import {-# source #-} Yi.UI.Common

data UIConfig = UIConfig {
   configFontSize :: Maybe Int,
   configLeftSideScrollBar :: Bool,
   configAutoHideScrollBar :: Bool,
   configLineWrap :: Bool,
   configWindowFill :: !Char,       
   -- ^ char to fill empty window space with.  Usually '~' for vi-like
   -- editors, ' ' for everything else
   configStyle :: UIStyle -- ^ ui colours
  }

{- | Currently duplicates some of Vim's indent settings. Allowing a buffer to
 - specify settings that are more dynamic, perhaps via closures, could be
 - useful.
 -}
data IndentSettings = IndentSettings { expandTabs :: Bool -- ^ Insert spaces instead of tabs as possible
                                     , tabSize    :: Int  -- ^ Size of a Tab
                                     , shiftWidth :: Int  -- ^ Indent by so many columns 
                                     }
                      deriving (Eq, Show, Typeable)

{- 
  The default indent settings should likely be initializable
  from a global preference.
 -}
instance Initializable IndentSettings where
    initial = error "IndentSettings should be initialized from Config."


-- | Configuration record. All Yi hooks can be set here.
data Config = Config {startFrontEnd :: UIBoot,
                      configUI :: UIConfig,
                      startActions :: [Action],
                      defaultKm :: Keymap,                      
                      modeTable :: ReaderT String Maybe AnyMode,
                      fundamentalMode :: Mode (),
                      publishedActions :: M.Map String [Dynamic],
                      debugMode :: Bool,
                      configKillringAccumulate :: !Bool,
                      -- ^ accumulate cuts automatically in killring
                      configIndentSettings :: IndentSettings
                      -- IndentSettings should perhaps be in Mode?
                     }

type UIBoot = Config -> (Event -> IO ()) -> ([Action] -> IO ()) ->  Editor -> IO UI

