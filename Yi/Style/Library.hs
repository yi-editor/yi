-- A collection of UIStyles.
module Yi.Style.Library where
import Yi.Style

-- | The default UIStyle
defaultLightTheme :: UIStyle
defaultLightTheme = UIStyle
    { window             = []
    , modeline           = [Foreground black,       Background darkcyan]
    , modelineFocused    = [Foreground brightwhite, Background darkcyan]
    , selected           = [Foreground Reverse,     Background Reverse]
    , eof                = [Foreground blue]
    , defaultStyle       = []
    , reverseStyle       = [Foreground Reverse, Background Reverse]
    , cppStyle           = [Foreground red]
    , commentStyle       = [Foreground purple]
    , keywordStyle       = [Foreground darkblue]
    , operatorStyle      = [Foreground brown]
    , upperIdStyle       = [Foreground darkgreen]
    , stringStyle        = [Foreground darkcyan]
    , numberStyle        = [Foreground darkred]
    , errorStyle         = [Background red]
    , hintStyle          = [Background cyan]
    , strongHintStyle    = [Background magenta]
    }

-- | A UIStyle inspired by the darkblue colorscheme of Vim.
darkBlueTheme :: UIStyle
darkBlueTheme = UIStyle 
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

