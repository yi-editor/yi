-- A collection of UIStyles.
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

  , selectedStyle      = Endo $ \a -> a {reverseAttr = True}
  , eofStyle           = withFg blue
  , errorStyle         = withBg red
  , hintStyle          = withBg cyan
  , strongHintStyle    = withBg magenta

  , commentStyle       = withFg purple
  , blockCommentStyle  = mempty
  , keywordStyle       = withFg darkblue
  , numberStyle        = withFg darkred
  , preprocessorStyle  = withFg red
  , stringStyle        = withFg darkcyan
  , longStringStyle    = mempty
  , typeStyle          = withFg darkgreen
  , variableStyle      = mempty
  , operatorStyle      = withFg brown
  , makeFileRuleHead   = withFg blue
  , makeFileAction     = withFg grey
  , quoteStyle         = withFg grey
  }


-- | The default UIStyle
defaultLightTheme :: Theme
defaultLightTheme = defaultTheme `override` \super _ -> super
  { modelineAttributes = emptyAttributes { foreground = black,    background = darkcyan }
  , tabBarAttributes   = emptyAttributes { foreground = white,    background = black }
  , baseAttributes     = emptyAttributes
  }

-- | A UIStyle inspired by the darkblue colorscheme of Vim.
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
