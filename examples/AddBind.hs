--
-- How to (statically) add a binding to vi mode, and change some
-- colours
--

module Config where

import Yi.Yi
import qualified Yi.Keymap.Vi as Vi

yi = settings { 
        keymap = Vi.keymapPlus fn,
        style  = ui { modeline_focused = Style BlueF DarkBlueB }
     }

fn = char 'n' 
    `action` \_ -> Just $ mapM_ insertE "yi rules"

