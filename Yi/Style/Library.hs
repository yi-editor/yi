-- A collection of UIStyles.
module Yi.Style.Library where
import Yi.Style

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

