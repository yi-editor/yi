--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
--

--
-- Introductory binding for the 'ee' editor
--

module Yi.Keymap.Ee ( keymap ) where

import Yi.Yi

keymap :: Keymap
keymap = comap eventToChar (command +++ insert)

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


