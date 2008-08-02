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
    , reverseStyle      = [Background black, Foreground green]
    , cppStyle          = [Background black, Foreground red]
    , commentStyle      = [Background black, Foreground darkred]
    , keywordStyle      = [Background black, Foreground brown]
    , operatorStyle     = [Background black, Foreground white]
    , upperIdStyle      = [Background black, Foreground darkgreen]
    , stringStyle       = [Background black, Foreground purple]
    , numberStyle       = [Background black, Foreground darkred]
    , errorStyle        = [Background black, Foreground green]
    , hintStyle         = [Background darkblue,  Foreground white]
    , strongHintStyle   = [Background blue, Foreground white]
    }

