-- This is ertai's Yi config

import Yi

import Yi (Keymap, (<|>))
import Yi.Keymap.Keys (char, (?>>!))
import qualified Yi.Keymap.Vim as Vim

keymap :: Keymap
keymap = Vim.keymap <|> (char ',' ?>>! Vim.viWrite)

main = yi $ defaultConfig {defaultKeymap = keymap}