-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

--
-- An example of how to write a simple top level config module that
-- pulls in a keymap from another local module.
--

module Config where

import Yi.Yi
import qualified Yi.Keymap.Vi as Keymap

yi = settings {
    keymap = Keymap.keymap
}
