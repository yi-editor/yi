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
-- of type 'Highligher a' below; this type is effectively isomorphic
-- to [Char] -> [Style], but are explicitly lazy to admit safe fast uses.
--

module Yi.Syntax ( Highlighter(..)
                 ) where

import Yi.Style

import qualified Data.ByteString as B

    

-- | The main type of syntax highlighters.  This record type combines all
-- the required functions, and is parametrized on the type of the internal
-- state.
--
-- Highlighters currently directly use the Vty color types; among other
-- things, this prevents the gtk port from using synhl.
data Highlighter a = SynHL 
                           { hlStartState :: a -- ^ The start state for the highlighter.
                             -- | Colorize a block of data passed in as a ByteString,
                             -- returning the new state and any attributes produced.
                             -- This *must* be implementable as a `B.foldl'.
                           , hlColorize :: B.ByteString -> a -> (a, [(Int,Style)])
                             -- | Colorize the end of file; this exists only to inform
                             -- states that lookahead will never happen.
                           , hlColorizeEOF :: a -> [(Int,Style)]
                           }
