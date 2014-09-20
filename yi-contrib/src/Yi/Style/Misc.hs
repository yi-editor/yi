{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Style.Misc
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A couple of themes.

module Yi.Style.Misc (happyDeluxe,  textExMachina) where

import Data.Monoid
import Yi.Style
import Yi.Style.Library
import Data.Prototype

-- TextMate themes are available on the TM wiki:
-- http://wiki.macromates.com/Themes/UserSubmittedThemes

-- | Theme originally designed by Joseph Andrew Magnani for TextMate, and
-- redistributed with explicit permission. It is not usable in the vty UI.
happyDeluxe :: Theme
happyDeluxe = defaultTheme `override` \super _ -> super
  { modelineAttributes = emptyAttributes
  , tabBarAttributes   = emptyAttributes { foreground = RGB 255 255 255 }
  , baseAttributes     = emptyAttributes { foreground = RGB 255 255 255, background = RGB 14 19 30 }

  , selectedStyle      = withBg (RGB 21 40 90)

  , commentStyle       = withFg (RGB 53 73 124)
  , keywordStyle       = withFg (RGB 254 144 6)
  , numberStyle        = withFg (RGB 20 222 209)
  , stringStyle        = withFg (RGB 253 102 249)
  , typeStyle          = mempty
  , operatorStyle      = mempty
  , errorStyle         = withFg (RGB 252 45 7)
  }

-- | Theme originally developed by Matthew Ratzloff for TextMate, and
-- redistributed with explicit permission. It is not usable in the vty UI.
textExMachina :: Theme
textExMachina = defaultTheme `override` \super _ -> super
  { modelineAttributes = emptyAttributes { foreground = black }
  , tabBarAttributes   = emptyAttributes { foreground = black }
  , baseAttributes     = emptyAttributes { foreground = RGB 230 230 230, background = RGB 21 21 21 }

  , selectedStyle      = withBg (RGB 102 102 102)

  , commentStyle       = withFg (RGB 51 51 51)
  , keywordStyle       = withFg (RGB 119 124 178)
  , numberStyle        = withFg (RGB 174 129 255)
  , stringStyle        = withFg (RGB 102 204 255)
  , typeStyle          = withFg (RGB 174 129 255)
  , variableStyle      = withFg (RGB 255 255 255)
  , operatorStyle      = withFg (RGB 151 255 127)
  }
