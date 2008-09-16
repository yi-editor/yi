-- A collection of UIStyles.
module Yi.Style.Library where
import Yi.Style
import Data.Prototype
import Data.Monoid

type Theme = Proto UIStyle

-- | Abstract theme that provides useful defaults.
defaultTheme :: Theme
defaultTheme = Proto $ \self -> UIStyle
    { window            = error "window must be redefined!"
    , modeline          = error "modeline must be redefined!"
    , modelineFocused   = withFg brightwhite `mappend` modeline self
    , selected          = withFg Reverse   `mappend` withBg Reverse
    , eof               = withFg blue      `mappend` window self

    , preprocessorStyle = withFg red       `mappend` window self
    , commentStyle      = withFg purple    `mappend` window self
    , blockCommentStyle = commentStyle self
    , keywordStyle      = withFg darkblue  `mappend` window self

    , operatorStyle     = withFg brown     `mappend` window self

    , variableStyle     =                            window self
    , typeStyle         = withFg darkgreen `mappend` window self

    , stringStyle       = withFg darkcyan  `mappend` window self
    , longStringStyle   = stringStyle self
    , numberStyle       = withFg darkred   `mappend` window self

    , errorStyle        = withBg red
    , hintStyle         = withBg cyan
    , strongHintStyle   = withBg magenta
    }


-- | The default UIStyle
defaultLightTheme :: Theme
defaultLightTheme = defaultTheme `override` \super _ -> super
    { window            = mempty
    , modeline          = withFg black `mappend` withBg darkcyan
    }

-- | A UIStyle inspired by the darkblue colorscheme of Vim.
darkBlueTheme :: Theme
darkBlueTheme = defaultTheme `override` \super _ -> super
    { window            = withFg white    `mappend` withBg black
    , modeline          = withFg darkblue `mappend` withBg white
    , modelineFocused   = withFg darkblue `mappend` withBg white
    , selected          = withFg white    `mappend` withBg blue
    , eof               = withFg red      `mappend` withBg black

    , preprocessorStyle = withFg red
    , commentStyle      = withFg darkred
    , keywordStyle      = withFg brown
    , operatorStyle     = withFg white
    , typeStyle         = withFg darkgreen
    , stringStyle       = withFg purple
    , numberStyle       = withFg darkred
    , errorStyle        = withFg green

    , hintStyle         = withBg darkblue
    , strongHintStyle   = withBg blue
    }

