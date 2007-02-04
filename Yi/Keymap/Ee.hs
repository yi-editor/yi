--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
-- Introductory binding for the 'ee' editor
--

module Yi.Keymap.Ee ( keymap ) where

import Yi.Yi         hiding ( keymap )
import Yi.Editor            ( Keymap )
import Yi.Interact

keymap :: Keymap
keymap = runProcess (command +++ insert) . map eventToChar

--
-- Control keys:
-- ^a ascii code           ^i tab                  ^r right
-- ^b bottom of text       ^j newline              ^t top of text
-- ^c command              ^k delete char          ^u up
-- ^d down                 ^l left                 ^v undelete word
-- ^e search prompt        ^m newline              ^w delete word
-- ^f undelete char        ^n next page            ^x search
-- ^g begin of line        ^o end of line          ^y delete line
-- ^h backspace            ^p prev page            ^z undelete line
-- ^[ (escape) menu
--

insert :: Interact Char ()
insert  = do c <- satisfy (const True); write (insertE c)

command :: Interact Char () 
command = choice [event c >> write act | (c, act) <- cmds]
    where cmds = [('\^R', rightE          ),  
                  ('\^B', botE            ),
                  ('\^T', topE            ),
                  ('\^K', deleteE         ),
                  ('\^U', upE             ),
                  ('\^D', downE           ),
                  ('\^L', leftE           ),
                  ('\^G', solE            ),
                  ('\^O', eolE            ),
                  ('\^Y', killE           ),
                  ('\^H', deleteE >> leftE),
                  ('\^X', quitE           )]


