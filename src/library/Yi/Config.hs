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

import Data.Prototype ( extractValue )
import Yi.Style ( UIStyle )
import Yi.Types
    ( Config(..),
      UIConfig(..),
      UIBoot,
      CursorStyle(..),
      Keymap,
      extractTopKeymap,
      AnyMode )
import Yi.Config.Lens
    ( bufferUpdateHandlerA,
      configCheckExternalChangesObsessivelyA,
      configInputPreprocessA,
      configKillringAccumulateA,
      configRegionStyleA,
      configUIA,
      configVarsA,
      debugModeA,
      defaultKmA,
      initialActionsA,
      layoutManagersA,
      modeTableA,
      startActionsA,
      startFrontEndA,
      configAutoHideScrollBarA,
      configAutoHideTabBarA,
      configCursorStyleA,
      configFontNameA,
      configFontSizeA,
      configLeftSideScrollBarA,
      configLineWrapA,
      configScrollStyleA,
      configScrollWheelAmountA,
      configThemeA,
      configVtyA,
      configWindowFillA,
      configVariable )

configStyle :: UIConfig -> UIStyle
configStyle = extractValue . configTheme

configFundamentalMode :: Config -> AnyMode
configFundamentalMode = last . modeTable

configTopLevelKeymap :: Config -> Keymap
configTopLevelKeymap = extractTopKeymap . defaultKm
