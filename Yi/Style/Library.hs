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
    , cppStyle           = defaultStyle self `changeFg` red
    , commentStyle       = defaultStyle self `changeFg` purple
    , keywordStyle       = defaultStyle self `changeFg` darkblue

    , operatorStyle      = defaultStyle self `changeFg` brown
    , consOperatorStyle  = operatorStyle self

    , idStyle            = defaultStyle self
    , upperIdStyle       = idStyle self `changeFg` darkgreen

    , stringStyle        = defaultStyle self `changeFg` darkcyan
    , numberStyle        = defaultStyle self `changeFg` darkred
    , errorStyle         = defaultStyle self `changeFg` red
    , hintStyle          = defaultStyle self `changeFg` cyan
    , strongHintStyle    = defaultStyle self `changeFg` magenta
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
    , cppStyle          = [Foreground red]
    , commentStyle      = [Foreground darkred]
    , keywordStyle      = [Foreground brown]
    , operatorStyle     = [Foreground white]
    , upperIdStyle      = [Foreground darkgreen]
    , stringStyle       = [Foreground purple]
    , numberStyle       = [Foreground darkred]
    , errorStyle        = [Foreground green]

    , hintStyle         = [Background darkblue]
    , strongHintStyle   = [Background blue]
    }

