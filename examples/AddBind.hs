-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

--
-- How to (statically) add a binding to vi mode, and change some
-- colours
--

module Config where

import Yi.Yi
import qualified Yi.Keymap.Vi as Vi

yi = settings {
        keymap = Vi.keymapPlus fn,
        style  = ui { modeline_focused = Style blue darkBlue }
     }

fn = char 'n'
    `action` \_ -> Just $ mapM_ insertE "yi rules"

