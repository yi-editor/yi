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

  , baseAttributes     = error "base attributes must be redefined!"

  , selectedStyle      = withFg Reverse `mappend` withBg Reverse
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
  }


-- | The default UIStyle
defaultLightTheme :: Theme
defaultLightTheme = defaultTheme `override` \super _ -> super
  { modelineAttributes = Attributes { foreground = black,    background = darkcyan }
  , baseAttributes     = Attributes { foreground = Default,  background = Default }
  }

-- | A UIStyle inspired by the darkblue colorscheme of Vim.
darkBlueTheme :: Theme
darkBlueTheme = defaultTheme `override` \super _ -> super
  { modelineAttributes = Attributes { foreground = darkblue, background = white }
  , modelineFocusStyle = withBg brightwhite

  , baseAttributes     = Attributes { foreground = white,    background = black }

  , selectedStyle      = withFg white `mappend` withBg blue
  , eofStyle           = withFg red
  , hintStyle          = withBg darkblue
  , strongHintStyle    = withBg blue

  , commentStyle       = withFg darkred
  , keywordStyle       = withFg brown
  , stringStyle        = withFg purple
  , operatorStyle      = withFg white
  }
