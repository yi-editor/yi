-- How to (statically) add a binding to vi mode

module Config where

import Yi.Yi
import qualified Yi.Keymap.Vi as Vi

yi = settings { keymap = Vi.keymapPlus fn }

fn = char 'n' 
    `action` \_ -> Just $ mapM_ insertE "yi rules"

