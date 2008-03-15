import Yi
import Yi.Keymap.Emacs (keymap)
import qualified Yi.Mode.Shim as Shim
import Yi.UI.Common (UIConfig(..))

main :: IO ()
main = yi $ defaultConfig {
                           configUI = UIConfig { configFontSize = Just 10 },
                           defaultKm = keymap,
                           modeTable = Shim.modeTable <|> modeTable defaultConfig
                          }
