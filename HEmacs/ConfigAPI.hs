--
-- riot/ConfigAPI.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

--
-- This module defines the type for the hooks available to user's
-- to script via ~/.hemacs/Config.hs script. It also defines default
-- values to use in case something goes wrong.
--

module HEmacs.ConfigAPI (

        Config(..),     -- all the hooks
        settings,       -- default values

  ) where

import HEmacs.Curses ( Key(..) )
import HEmacs.Version
import HEmacs.UI
import HEmacs.Style
import HEmacs.Entry

--
-- define the hooks the applicaiton 
--
data Config = Config {
    handle_key, k_map, tag_map :: KeyMap,
    topinfo_text               :: String,
    styles                     :: [StyleSpec]
}

type KeyMap =  Key -> (EditableEntry a => Status a -> IO (ELCont a))

--
-- and provide a default implementation
--
settings :: Config
settings = Config {
        handle_key   = dflt_handle_key,
        k_map        = dflt_k_map,
        tag_map      = dflt_tag_map,
        topinfo_text = dflt_topinfo_text,
        styles       = dflt_styles
    }

-- defaults

dflt_handle_key :: KeyMap
dflt_handle_key (KeyChar 'q') = e_quit
dflt_handle_key (KeyChar '\^R') = e_refresh
dflt_handle_key (KeyChar '.') = e_hscroll 2
dflt_handle_key (KeyChar ',') = e_hscroll (-2)
--dflt_handle_key (KeyDown)     = e_vscroll 2
--dflt_handle_key (KeyUp)       = e_vscroll (-2)
dflt_handle_key (KeyChar ' ') = e_pgdn
dflt_handle_key (KeyChar 'b') = e_pgup
dflt_handle_key (KeyChar 'u') = e_pgupentry
dflt_handle_key (KeyChar 'v') = e_pgdnentry
dflt_handle_key (KeyPPage)    = e_pgupentry
dflt_handle_key (KeyNPage)    = e_pgdnentry
dflt_handle_key (KeyHome)     = e_firstentry
dflt_handle_key (KeyEnd)      = e_lastentry
dflt_handle_key (KeyChar 'n') = e_nextentry
dflt_handle_key (KeyChar 'p') = e_preventry
dflt_handle_key (KeyUp)       = e_preventry
dflt_handle_key (KeyDown)     = e_nextentry
dflt_handle_key (KeyEnter)    = e_actentry
dflt_handle_key (KeyChar '\^M') = e_actentry
dflt_handle_key (KeyChar 'd') = e_delentry
dflt_handle_key (KeyChar 'e') = e_editentry
dflt_handle_key (KeyBackspace) = e_collapse
dflt_handle_key (KeyChar 'a') = e_newentry
dflt_handle_key (KeyChar 'r') = e_newunder
dflt_handle_key (KeyChar '\^_') = e_undo
dflt_handle_key (KeyChar '\^^') = e_redo
dflt_handle_key (KeyChar 't') = e_tag TagToggle
dflt_handle_key (KeyChar 'k') = e_submap "k" dflt_k_map
dflt_handle_key (KeyChar ';') = e_submap ";" dflt_tag_map
dflt_handle_key k             = e_fallback dflt_handle_key k

dflt_k_map :: KeyMap
dflt_k_map (KeyChar 'u') = e_firstentry
dflt_k_map (KeyChar 'v') = e_lastentry
dflt_k_map (KeyChar 'd') = e_save
dflt_k_map k             = e_fallback dflt_handle_key k

dflt_tag_map :: KeyMap
dflt_tag_map (KeyChar 't') = e_clear_tags
dflt_tag_map (KeyChar 'a') = e_move_tagged_after
dflt_tag_map (KeyChar 'b') = e_move_tagged_before
dflt_tag_map (KeyChar 'r') = e_move_tagged_under
dflt_tag_map (KeyChar 'd') = e_delete_tagged
dflt_tag_map k             = e_fallback dflt_handle_key k


--
-- Help
--

dflt_topinfo_text :: String
dflt_topinfo_text = package++" "++version
               ++" ** q:quit, e:edit, a:new, r:new child, kd:save"

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

