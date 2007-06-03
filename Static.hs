--
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
-- | Frontend to the static binary. We have a separte frontend (rather
-- than putting main in Yi.hs) so we don't get ZCMain_main_* symbols
-- in -package yi, which lets us have multiple frontends, and load
-- them all in ghci.
--
module Static ( main ) where

import Yi.Debug
import Yi.Boot
import qualified Yi.Main 

main :: IO ()
main = do
  initDebug ".yi-static.dbg"
  kernel <- initialize
  Yi.Main.main kernel
