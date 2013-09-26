-- A collection of Themes.
module Yi.Style.Library where

import Yi.Style
import Data.Prototype
import Data.Monoid

type Theme = Proto UIStyle

-- | Abstract theme that provides useful defaults.
defaultTheme :: Theme
defaultTheme = Proto $ const UIStyle
  { modelineAttributes = emptyAttributes { foreground = white, background = grey }
  , modelineFocusStyle = withFg brightwhite

  , tabBarAttributes   = emptyAttributes
  , tabInFocusStyle    = withFg black `mappend` withBg white
  , tabNotFocusedStyle = mempty

  , baseAttributes     = emptyAttributes

  , selectedStyle      = withFg white `mappend` withBg purple
  , eofStyle           = withFg blue
  , errorStyle         = withBg red
  , hintStyle          = withFg black `mappend` withBg cyan
  , strongHintStyle    = withFg black `mappend` withBg magenta

  , commentStyle       = withFg purple
  , blockCommentStyle  = withFg purple
  , keywordStyle       = withFg darkblue
  , numberStyle        = withFg darkred
  , preprocessorStyle  = withFg red
  , stringStyle        = withFg darkcyan
  , longStringStyle    = mempty
  , typeStyle          = withFg darkgreen
  , dataConstructorStyle
                       = withBd True `mappend` withFg darkgreen
  , importStyle        = withFg blue
  , builtinStyle       = withFg blue
  , regexStyle         = withFg red
  , variableStyle      = mempty
  , operatorStyle      = withFg brown
  , makeFileRuleHead   = withFg blue
  , makeFileAction     = withFg grey
  , quoteStyle         = withFg grey
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
