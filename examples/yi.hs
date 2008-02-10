import Yi
import Yi.Keymap.Emacs (keymap)

main = yi $ defaultConfig {defaultKm = keymap}