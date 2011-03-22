-- Copyright (c) 2005, 2008 Don Stewart - http://www.cse.unsw.edu.au/~dons

-- Introductory binding for the 'ee' editor

module Yi.Keymap.Ee ( keymap ) where

import Yi.Yi
import Control.Arrow
import Yi.Keymap.Emacs.KillRing

keymap :: Keymap
keymap = comap eventToChar (command +++ insert)

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

insert :: Interact Char ()
insert  = do c <- satisfy (const True); write (insertN c)

command :: Interact Char ()
command = choice [event c >> write act | (c, act) <- cmds]
    where cmds = [('\^R', rightB          ),
                  ('\^B', botB            ),
                  ('\^T', topB            ),
                  ('\^K', deleteN 1       ),
                  ('\^U', execB Move VLine Backward),
                  ('\^D', execB Move VLine Forward),
                  ('\^L', leftB           ),
                  ('\^G', moveToSol       ),
                  ('\^O', moveToEol       ),
                  ('\^Y', killLineE       ),
                  ('\^H', bdeleteB        ),
                  ('\^X', quitEditor           )]


