-- A collection of UIStyles.
module Yi.Style.Library where
import Yi.Style
import Data.Prototype

type Theme = Proto UIStyle

-- | Abstract theme that provides useful defaults.
defaultTheme :: Theme
defaultTheme = Proto $ \self -> UIStyle
    { window             = defaultStyle self
    , modeline           = error "modeline must be redefined!"
    , modelineFocused    = modeline self `changeFg` brightwhite
    , selected           = reverseStyle self
    , eof                = [Foreground blue]
    , defaultStyle       = error "defaultStyle must be redefined!"
    , reverseStyle       = [Foreground Reverse, Background Reverse]
    , preprocessorStyle  = defaultStyle self `changeFg` red
    , commentStyle       = defaultStyle self `changeFg` purple
    , blockCommentStyle  = commentStyle self
    , keywordStyle       = defaultStyle self `changeFg` darkblue

    , operatorStyle      = defaultStyle self `changeFg` brown

    , variableStyle      = defaultStyle self
    , typeStyle          = variableStyle self `changeFg` darkgreen

    , stringStyle        = defaultStyle self `changeFg` darkcyan
    , longStringStyle    = stringStyle self
    , numberStyle        = defaultStyle self `changeFg` darkred

    , errorStyle         = defaultStyle self `changeBg` red
    , hintStyle          = defaultStyle self `changeBg` cyan
    , strongHintStyle    = defaultStyle self `changeBg` magenta
    }


-- | The default UIStyle
defaultLightTheme :: Theme
defaultLightTheme = defaultTheme `override` \super _ -> super
    { modeline           = [Foreground black,       Background darkcyan]
    , defaultStyle       = []
    }

-- | A UIStyle inspired by the darkblue colorscheme of Vim.
darkBlueTheme :: Theme
darkBlueTheme = defaultTheme `override` \super _ -> super
    {
      window            = [Background black, Foreground white]
    , modeline          = [Background white, Foreground darkblue]
    , modelineFocused   = [Background white, Foreground darkblue]
    , selected          = [Background blue,  Foreground white]
    , eof               = [Background black, Foreground red]
    , defaultStyle      = [Background black, Foreground white]

    , reverseStyle      = [Foreground green]
    , preprocessorStyle = [Foreground red]
    , commentStyle      = [Foreground darkred]
    , keywordStyle      = [Foreground brown]
    , operatorStyle     = [Foreground white]
    , typeStyle         = [Foreground darkgreen]
    , stringStyle       = [Foreground purple]
    , numberStyle       = [Foreground darkred]
    , errorStyle        = [Foreground green]

    , hintStyle         = [Background darkblue]
    , strongHintStyle   = [Background blue]
    }

