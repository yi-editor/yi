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


module Config where

import HEmacs.HEmacs

hemacs = settings 

{-

  -- override any other fields of ConfigAPI.settings here with yours

  styles = [
     StyleSpec ("attr_infoline", (a_none, c_red  , c_white)),
     StyleSpec ("attr_text",     (a_none, c_white, c_black)),
     StyleSpec ("attr_entry",    (a_none, c_white, c_black)),
     StyleSpec ("attr_entry_act",(a_bold, c_white, c_black)),
     StyleSpec ("attr_entry_sel",(a_reverse, c_cyan, c_black)),
     StyleSpec ("attr_entry_act_sel",(a_reverse, c_cyan, c_black)),
     StyleSpec ("attr_message",  (a_bold, c_white, c_black)),
     StyleSpec ("attr_error",    (a_bold, c_red, c_black))
  ]

}
-}
