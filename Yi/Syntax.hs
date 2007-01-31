{-# OPTIONS -#include "YiUtils.h" #-}
--
-- Copyright (C) 2007 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

-- | This module defines a common interface for syntax highlighters.
--
-- Yi syntax highlighters are expressed as explicit lazy computations
-- of type 'SynHL' below; this type is effectively isomorphic to [Char]
-- -> [Color], but are explicitly lazy to admit safe fast uses.
--

module Yi.Syntax ( HLState, highlight, highinit, highend
                 ) where

import Yi.Vty

-- | Currently, we reqire all syntax highlighters to use an Integer for state.
-- An existentially-quantified type would probably be Better.

type HLState = Integer

-- | The master type of highlighters.
--
-- Highlighters (currently) directly use the Vty color types.

-- type Highlighter = Integer -> Char -> (Integer, [Vty.Attr])

-- | A simple test highlighter.

highinit :: Integer
highinit = 0

highlight :: Integer -> Char -> (Integer, [Attr])
highlight 0 'f' = (1,[])
highlight 0 _   = (0,[attr])
highlight 1 'o' = (2,[])
highlight 1 _   = (0,[attr,attr])
highlight 2 'o' = (0,[redA,redA,redA]) where redA = attr { fg = red }
highlight 2 _   = (0,[attr,attr,attr])

highend :: Integer -> [Attr]
highend = flip replicate attr . fromInteger
