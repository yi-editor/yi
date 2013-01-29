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
  , modelineFocusStyle = withFg bright_white

  , tabBarAttributes   = error "tabbar attributes must be redefined!"
  , tabInFocusStyle    = withFg black <> withBg bright_white
  , tabNotFocusedStyle = withFg bright_black <> withBg white

  , baseAttributes     = error "base attributes must be redefined!"

  , selectedStyle      = withFg black <> withBg bright_magenta
  , eofStyle           = withFg bright_blue
  , errorStyle         = withBg bright_red
  , hintStyle          = withFg black <> withBg bright_magenta
  , strongHintStyle    = withFg black <> withBg magenta

  , commentStyle       = withFg white
  , blockCommentStyle  = withFg white
  , keywordStyle       = withFg bright_blue
  , numberStyle        = withFg red
  , preprocessorStyle  = withFg bright_red
  , stringStyle        = withFg magenta
  , longStringStyle    = mempty
  , typeStyle          = withFg green
  , dataConstructorStyle
                       = withBd True <> withFg green
  , importStyle        = withFg bright_blue
  , builtinStyle       = withFg bright_blue
  , regexStyle         = withFg bright_red
  , variableStyle      = mempty
  , operatorStyle      = withFg yellow
  , makeFileRuleHead   = withFg bright_blue
  , makeFileAction     = withFg bright_black
  , quoteStyle         = withFg bright_black
  }


-- | The default Theme
defaultLightTheme :: Theme
defaultLightTheme = defaultTheme `override` \super _ -> super
  { modelineAttributes = emptyAttributes { foreground = bright_black,    background = white }
  , modelineFocusStyle = withFg black <> withBg white
  , tabBarAttributes   = emptyAttributes { foreground = white,    background = black }
  , baseAttributes     = emptyAttributes
  }

-- | A Theme inspired by the blue colorscheme of Vim.
darkBlueTheme :: Theme
darkBlueTheme = defaultTheme `override` \super _ -> super
  { modelineAttributes = emptyAttributes { foreground = blue, background = white }
  , modelineFocusStyle = withBg bright_white

  , tabBarAttributes   = emptyAttributes { foreground = blue, background = bright_white }
  , tabInFocusStyle    = withFg bright_black <> withBg white
  , tabNotFocusedStyle = withFg white <> withBg white

  , baseAttributes     = emptyAttributes { foreground = white,    background = black }

  , selectedStyle      = withFg white <> withBg bright_blue
  , eofStyle           = withFg bright_red
  , hintStyle          = withBg blue
  , strongHintStyle    = withBg bright_blue

  , commentStyle       = withFg red
  , keywordStyle       = withFg yellow
  , stringStyle        = withFg magenta
  , variableStyle      = withFg bright_magenta
  , operatorStyle      = withFg yellow
  }
