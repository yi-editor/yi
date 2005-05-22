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

module Yi.Keymap.Ee ( keymap, EeMode ) where

import Yi.Yi         hiding ( keymap )
import Yi.Editor            ( Action )

type EeMode = Lexer () Action

keymap :: [Char] -> [Action]
keymap cs = let (actions,_,_) = execLexer mode (cs, ()) in actions

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

mode :: Lexer () Action
mode = insert >||< command  

insert :: Lexer () Action
insert  = anything `action` \[c] -> Just (insertE c)
        where anything = alt ['\0' .. '\255']
 
command :: Lexer () Action
command = cmd `action` \[c] -> Just $ case c of
                '\^R' -> rightE
                '\^B' -> botE
                '\^T' -> topE
                '\^K' -> deleteE
                '\^U' -> upE
                '\^D' -> downE
                '\^L' -> leftE
                '\^G' -> solE
                '\^O' -> eolE
                '\^Y' -> killE
                '\^H' -> deleteE >> leftE
                '\^X' -> quitE
                _     -> undefined
        where cmd = alt "\^R\^B\^T\^K\^U\^D\^L\^G\^O\^Y\^H\^X"

