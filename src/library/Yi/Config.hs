{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Config
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module exposing common user settings. Users most likely want to be starting
-- with "Yi.Config.Default".

module Yi.Config ( Config(..), UIConfig(..), UIBoot, CursorStyle(..)
                 , module Yi.Config.Lens
                 , configStyle, configFundamentalMode, configTopLevelKeymap
                 ) where

import Data.Prototype
import Yi.Style
import Yi.Types (Config(..), UIConfig(..), UIBoot, CursorStyle(..),
                 Keymap, extractTopKeymap, AnyMode)
import Yi.Config.Lens

configStyle :: UIConfig -> UIStyle
configStyle = extractValue . configTheme

configFundamentalMode :: Config -> AnyMode
configFundamentalMode = last . modeTable

configTopLevelKeymap :: Config -> Keymap
configTopLevelKeymap = extractTopKeymap . defaultKm
