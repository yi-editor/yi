import Yi
import Yi.Keymap.Emacs (keymap)
import qualified Yi.Mode.Shim as Shim

main :: IO ()
main = yi $ defaultConfig {
                           defaultKm = keymap,
                           modeTable = Shim.modeTable <|> modeTable defaultConfig
                          }
