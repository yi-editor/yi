-- This is ertai's Yi keymap.

module Yi.Keymap.Users.Ertai (keymap) where

import Yi.Yi (Keymap, (<|>), event, charToEvent, write)
import qualified Yi.Keymap.Vim as Vim

keymap :: Keymap
keymap = Vim.keymap <|> (event (charToEvent ',') >> write Vim.viWrite)