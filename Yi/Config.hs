--
-- riot/Config.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

--
-- This module defines defaults for user configurable data.
--

module Yi.Config (

        settings,       -- default values

  ) where

import Yi.Editor    ( Config(..), Key(..), Action )
import Yi.Core      

--
-- | Default settings
--
settings :: Config
settings = Config {
        keyMap       = defaultKeyMap
    -- ,styles       = dflt_styles
    }

-- ---------------------------------------------------------------------
-- | Default key bindings. Each key is bound to a Core editor function.
-- Having this a single function is annoying, as I have to redefine the
-- lot to change one binding. It should be a set of bindings.
--

--
-- vi-like bindings
--
defaultKeyMap :: Key -> Action
defaultKeyMap (Key 'q')   = quitE
defaultKeyMap (Key '\^R') = refreshE
defaultKeyMap (Key 'h')   = leftE
defaultKeyMap (Key 'l')   = rightE
defaultKeyMap (Key '1')   = topE
defaultKeyMap (Key 'G')   = botE
defaultKeyMap (Key 'j')   = downE
defaultKeyMap (Key 'k')   = upE
defaultKeyMap (Key '^')   = solE
defaultKeyMap (Key '$')   = eolE
defaultKeyMap (Key 'r')   = replaceE
defaultKeyMap (Key 'i')   = insertE
defaultKeyMap (Key 'x')   = deleteE
defaultKeyMap (Key 'w')   = writeE
defaultKeyMap (Key 'D')   = killE
defaultKeyMap (Key 'N')   = nextE
defaultKeyMap (Key 'P')   = prevE
defaultKeyMap (KeyUp)     = upE
defaultKeyMap (KeyDown)   = downE
defaultKeyMap (KeyLeft)   = leftE
defaultKeyMap (KeyRight)  = rightE
defaultKeyMap _           = noopE

{-
--
-- Styles
--

dflt_styles :: [StyleSpec]
dflt_styles = [
     StyleSpec ("attr_infoline", (a_bold, c_green, c_blue)),
     StyleSpec ("attr_text",     (a_none, c_white, c_black)),
     StyleSpec ("attr_entry",    (a_none, c_white, c_black)),
     StyleSpec ("attr_entry_act",(a_bold, c_white, c_black)),
     StyleSpec ("attr_entry_sel",(a_reverse, c_cyan, c_black)),
     StyleSpec ("attr_entry_act_sel",(a_reverse, c_cyan, c_black)),
     StyleSpec ("attr_message",  (a_bold, c_white, c_black)),
     StyleSpec ("attr_error",    (a_bold, c_red, c_black))
     ]

-}
