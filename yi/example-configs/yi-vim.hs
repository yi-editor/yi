import Yi
import Yi.Keymap.Vim (keymapSet)
import qualified Yi.Mode.Haskell as Haskell
import Yi.Style
import Yi.Style.Library
import Yi.Prelude
import Prelude ()

-- import Yi.UI.Vty (start)
-- import Yi.UI.Cocoa (start)
-- import Yi.UI.Pango (start)

-- Uncomment for Shim support
-- import qualified Yi.Mode.Shim as Shim
-- -- Shim.minorMode gives us emacs-like keybindings - what would be a good
-- -- set of keybindings for vim?
-- shimMode :: AnyMode
-- shimMode = AnyMode (Shim.minorMode Haskell.cleverMode)

{-
  For now we just make the selected style the same as the
  modeline_focused style... Just because i'm not good with
  styles yet - Jim
-}
defaultVimUiTheme :: Theme
defaultVimUiTheme = defaultTheme  `override` \super self -> super {
        selectedStyle = modelineFocusStyle self
 }

myConfigUI :: UIConfig
myConfigUI = (configUI defaultVimConfig)  {
        configFontSize = Just 10,
        configTheme = defaultVimUiTheme,
        configWindowFill = '~'
 }

main :: IO ()
main = yi $ defaultVimConfig {
    -- Uncomment for Shim support
    -- modeTable = [shimMode] <|> modeTable defaultVimConfig,
    configUI = myConfigUI,
    defaultKm = keymapSet
 }
