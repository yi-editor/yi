import Yi
import Yi.Keymap.Vim (keymap)
import qualified Yi.Mode.Haskell as Haskell
import Yi.Style
import Yi.Style.Library
import Yi.Prelude
import Prelude ()

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
defaultVimUiTheme = defaultLightTheme  `override` \super self -> super {
        selectedStyle = modelineFocusStyle self
}

myConfigUI :: UIConfig
myConfigUI = (configUI defaultConfig)  {
        configFontSize = Just 10,
        configTheme = defaultVimUiTheme,
        configWindowFill = '~'
}

main :: IO ()
main = yi $ defaultConfig {
    -- Uncomment for Shim support
    -- modeTable = [shimMode] <|> modeTable defaultConfig,
    configUI = myConfigUI,
    defaultKm = keymap
}
