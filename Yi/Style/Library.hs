-- A collection of Themes.
module Yi.Style.Library where

import Yi.Style
import Data.Prototype
import Data.Monoid

type Theme = Proto UIStyle

-- | Abstract theme that provides useful defaults.
defaultTheme :: Theme
defaultTheme = Proto $ const $ UIStyle
  { modelineAttributes = error "modeline attributes must be redefined!"
  , modelineFocusStyle = withFg brightwhite

  , tabBarAttributes   = error "tabbar attributes must be redefined!"
  , tabInFocusStyle    = withFg black `mappend` withBg brightwhite
  , tabNotFocusedStyle = withFg grey `mappend` withBg white

  , baseAttributes     = error "base attributes must be redefined!"

  , selectedStyle      = withFg black `mappend` withBg cyan
  , eofStyle           = withFg blue
  , errorStyle         = withBg red
  , hintStyle          = withFg black `mappend` withBg cyan
  , strongHintStyle    = withFg black `mappend` withBg magenta

  , commentStyle       = withFg purple
  , blockCommentStyle  = mempty
  , keywordStyle       = withFg darkblue
  , numberStyle        = withFg darkred
  , preprocessorStyle  = withFg red
  , stringStyle        = withFg darkcyan
  , longStringStyle    = mempty
  , typeStyle          = withFg darkgreen
  , dataConstructorStyle
                       = withBd True `mappend` withFg darkgreen
  , variableStyle      = mempty
  , operatorStyle      = withFg brown
  , makeFileRuleHead   = withFg blue
  , makeFileAction     = withFg grey
  , quoteStyle         = withFg grey
  }


-- | The default Theme
defaultLightTheme :: Theme
defaultLightTheme = defaultTheme `override` \super _ -> super
  { modelineAttributes = emptyAttributes { foreground = black,    background = darkcyan }
  , tabBarAttributes   = emptyAttributes { foreground = white,    background = black }
  , baseAttributes     = emptyAttributes
  }

-- | A Theme inspired by the darkblue colorscheme of Vim.
darkBlueTheme :: Theme
darkBlueTheme = defaultTheme `override` \super _ -> super
  { modelineAttributes = emptyAttributes { foreground = darkblue, background = white }
  , modelineFocusStyle = withBg brightwhite

  , tabBarAttributes   = emptyAttributes { foreground = darkblue, background = brightwhite }
  , tabInFocusStyle    = withFg grey `mappend` withBg white
  , tabNotFocusedStyle = withFg lightGrey `mappend` withBg white

  , baseAttributes     = emptyAttributes { foreground = white,    background = black }

  , selectedStyle      = withFg white `mappend` withBg blue
  , eofStyle           = withFg red
  , hintStyle          = withBg darkblue
  , strongHintStyle    = withBg blue

  , commentStyle       = withFg darkred
  , keywordStyle       = withFg brown
  , stringStyle        = withFg purple
  , variableStyle      = withFg cyan
  , operatorStyle      = withFg brown
  }
  
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
