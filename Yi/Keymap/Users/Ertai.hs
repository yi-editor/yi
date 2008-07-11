-- This is ertai's Yi keymap.

module Yi.Keymap.Users.Ertai (keymap) where

import Yi (Keymap, (<|>))
import Yi.Keymap.Keys (char, (?>>!))
import qualified Yi.Keymap.Vim as Vim

keymap :: Keymap
keymap = Vim.keymap <|> (char ',' ?>>! Vim.viWrite)
