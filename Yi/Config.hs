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

import Yi.Editor    ( Config(..), Action )
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
defaultKeyMap :: Char -> Action
defaultKeyMap 'q'   = quitE

{-
defaultKeyMap '\^R' = refreshE
defaultKeyMap 'h'   = leftE
defaultKeyMap 'l'   = rightE
defaultKeyMap '1'   = topE
defaultKeyMap 'G'   = botE
defaultKeyMap 'j'   = downE
defaultKeyMap 'k'   = upE
defaultKeyMap '^'   = solE
defaultKeyMap '$'   = eolE
defaultKeyMap 'r'   = replaceE
defaultKeyMap 'x'   = deleteE
defaultKeyMap 'w'   = writeE
defaultKeyMap 'D'   = killE
defaultKeyMap 'N'   = nextE
defaultKeyMap 'P'   = prevE
defaultKeyMap k
    | k == keyUp    = upE
    | k == keyDown  = downE
    | k == keyLeft  = leftE
    | k == keyRight = rightE
    | otherwise     = noopE
-}

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
