--
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--

--
-- Front end to the library, for use by external scripts. Just reexports
-- a bunch of modules.
--
-- You should therefore:
--      import Yi.Yi
-- in your ~/.yi/ scripts
--

module Yi.Yi (
              -- all things re-exported here are made available to keymaps definitions.
        module Yi.Core,
        module Yi.Buffer,
        module Yi.CharMove,
        module Yi.Style,
        module Yi.Eval,
        module Yi.Event, -- hack, for key defns
        module Yi.Search,
        module Yi.Region,
        module Yi.Interact,
        module Yi.Debug,
        module Control.Monad -- since all actions are monadic, this is very useful to combine them.
   ) where

import Yi.Core
import Yi.Buffer
import Yi.CharMove
import Yi.Style
import Yi.Search
import Yi.Region
import Yi.Interact hiding (Interact, runProcess, write)
import Yi.Debug
import Control.Monad
import Yi.Eval


import Yi.Event -- so we can see key defns


