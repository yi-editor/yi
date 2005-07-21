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

module Yi.Keymap.Mg where

-- 
-- MG(1)                      OpenBSD Reference Manual                      MG(1)
-- 
-- NNAAMMEE
--      mmgg - emacs-like text editor
-- 
-- SSYYNNOOPPSSIISS
--      mmgg [_o_p_t_i_o_n_s] [_f_i_l_e _._._.]
-- 
-- DDEESSCCRRIIPPTTIIOONN
--      mmgg is intended to be a small, fast, and portable editor for people who
--      can't (or don't want to) run the real emacs for one reason or another, or
--      are not familiar with the vi(1) editor.  It is compatible with emacs be-
--      cause there shouldn't be any reason to learn more editor types than emacs
--      or vi(1).
-- 
--      The options are as follows:
-- 
--      _+_n_u_m_b_e_r
--              Go to the line specified by number (do not insert a space between
--              the "+" sign and the number).  If a negative number is specified,
--              the line number counts backwards from the end of the file i.e.
--              +-1 will be the last line of the file, +-2 will be second last,
--              and so on.
-- 
--      --ff _<_m_o_d_e_>
--              Run the mode command for all buffers created from arguments on
--              the command line, including the scratch buffer and all files.
-- 
--      --nn      Turn off backup file generation.
-- 
--      Normal editing commands are very similar to Gnu Emacs.  In the following
--      examples, ^X means control-X, and M-X means Meta-X, where the Meta key
--      may be either a special key on your keyboard or the ALT key; otherwise
--      ESC followed by the key X works as well.
-- 
--      ^F     Forward character
--      ^B     Backwards character
--      ^N     Next line
--      ^P     Previous line
--      ^A     Start of line
--      ^E     End of line
--      ^D     delete current character
--      ^S     interactive search forward
--      ^R     interactive search backwards
--      ^O     Open a new line at cursor position
--      ^T     transpose characters
--      ^U     Repeat next command 4 times (can be cascaded i.e. ^u^u^f will move
--             16 characters forward)
-- 
--      ^K     kill to end of line (placing into kill buffer)
--      ^Y     yank kill buffer into current location
--      ^@     set mark
--      ^W     kill region (cuts from previously set mark to current location,
--             into kill buffer)
--      M-W    copy region (into kill buffer)
-- 
--      ^V     Next page
--      M-V    Previous page
--      M-<    start of buffer
--      M->    end of buffer
-- 
--      ^X^C   Quit (you will be asked if you want to save files)
--      ^X-O   Next window.
--      ^X-N   Next window.
--      ^X-P   Previous window.
--      ^X-U   Undo.
-- 
--      For more key bindings, type ``M-x describe-bindings''.
-- 
--      mmgg differs primarily in not having special modes for tasks other than
--      straight editing, e.g., mail and news, and in not having special modes
--      that support various programming languages.  It does have text justifica-
--      tion and auto-fill mode.  Since it is written completely in C, there is
--      no language in which you can write extensions.  However, you can rebind
--      keys and change some parameters.  There are no limits to line length or
--      format.  Command, buffer, and file name completion and listing can be
--      done using the spacebar and `?', respectively.
-- 
--      Amongst other major differences, the mmgg configuration files are much sim-
--      pler than real emacs.  There are two configuration files, _._m_g, and _._m_g_-
--      _T_E_R_M.  Here, TERM represents the name of your terminal type; e.g., if
--      your terminal type is set to ``vt100'', mmgg will use _._m_g_-_v_t_1_0_0 as a start-
--      up file.  The terminal type startup file is used first.  See the manual
--      for a full list of the commands that can go in the files.
-- 
--      Here's another example sequence that you may find useful.  By default,
--      ``()'' and ``[]'' are recognized as brackets, so bracket matching can be
--      done.  The following defines ``{}'' as brackets, and turns on the mode
--      that causes the cursor to "blink" to show you matching brackets.
-- 
--            global-set-key } blink-matching-paren-hack
--            blink-matching-paren
--            set-default-mode blink
-- 
--      More complicated key mappings are also possible, though there are some
--      internal limitations compared to regular emacs.  An example of how to map
--      control characters and sequences follows, illustrating the Gosling-like
--      line scrolling characters.
-- 
--            global-set-key "\^Z" scroll-one-line-up
--            global-set-key "\ez" scroll-one-line-down
--            global-set-key "\^_" suspend-emacs
-- 
-- FFIILLEESS
--      ~/.mg       normal startup file
--      ~/.mg-TERM  terminal-specific startup file
-- 
-- SSEEEE AALLSSOO
--      vi(1)
-- 
-- BBUUGGSS
--      When you type `?' to list possible file names, buffer names, etc., a help
--      buffer is created for the possibilities.  In Gnu Emacs, this buffer goes
--      away the next time you type a real command.  In mmgg, you must use "^X-1"
--      to get rid of it.
-- 
--      The undo feature has a minor difference compared to the same feature in
--      Gnu Emacs.  When the end of the undo records list is reached, mmgg will not
--      stop and inform the user for one undo keystroke before continuing.
-- 
-- OpenBSD 3.7                    February 25, 2000                             2

import Yi.Yi         hiding ( keymap )
import Yi.Editor            ( Action )
import qualified Yi.Map as M

import Numeric   ( showOct )
import Data.Char ( ord )

type MgMode = Lexer () Action

keymap :: [Char] -> [Action]
keymap cs = let (actions,_,_) = execLexer mode (cs, ()) in actions

mode :: Lexer () Action
mode = insert >||< command >||< ctrlx

insert :: Lexer () Action
insert  = anything `action` \[c] -> Just (insertE c)
        where anything = alt ['\0' .. '\255']
 
command :: Lexer () Action
command = cmd `action` \[c] -> Just $ 
                case M.lookup c cmdMap of
                        Nothing -> undefined
                        Just a  -> a
        where
            cmd = alt $ M.keys cmdMap

cmdMap :: M.Map Char Action
cmdMap = M.fromList [
       ('\^F' , rightE),
       ('\^B' , leftE),
       ('\^N' , downE),
       ('\^P' , upE),
       ('\^A' , solE),
       ('\^E' , eolE),
       ('\^D' , deleteE),
       ('\^S' , errorE "search forwards, not yet implemented"),
       ('\^R' , errorE "search backwards, not yet implemented"),
       ('\^O' , solE >> insertE '\n' >> upE),
       ('\^T' , errorE "transpose, unimplemented"),
       ('\^U' , errorE "repeat command 4 times unimplemented"),
       ('\^K' , readRestOfLnE >>= setRegE >> killE),
       ('\^Y' , getRegE >>= \s -> mapM_ insertE s >> solE),
       ('\^@' , errorE "set mark, unimplemented"),
       ('\^W' , errorE "kill region, unimplemented"),
       ('\^Q' , quitE),
       ('\^Z' , suspendE),
       ('\^_' , undoE) ]

ctrlx :: Lexer () Action
ctrlx = char '\^X' 
        `meta` \_ _ -> (Just (Right (msgE "C-x-")), (), Just ctrlxMode)

ctrlxMode :: Lexer () Action
ctrlxMode = cmd 
        `meta` \[c] _ -> (Just (Right (f c)),  (), Just mode) -- and leave 
        where
            cmd = alt $ M.keys ctrlXMap
            f c = case M.lookup c ctrlXMap of
                        Nothing -> undefined
                        Just a  -> a

ctrlXMap :: M.Map Char Action
ctrlXMap = M.fromList [
        ('\^C', quitE),
        ('\^S', fwriteE),
        ('\^W', errorE "write file unimplemented"),
        ('\^G', errorE "keyboard quit unimplemented"),
        ('O',   nextWinE),
        ('N',   nextWinE),
        ('P',   prevWinE),
        ('0',   closeE),
        ('1',   errorE "delete-other-windows unimplemented"),
        ('2',   splitE),
        ('U',   undoE),
        ('=',   do (_,_,ln,col,pt,pct) <- bufInfoE
                   c <- readE
                   msgE $ "Char: "++[c]++" (0"++showOct (ord c) ""++
                          ")  point="++show pt++
                          "("++pct++
                          ")  line="++show ln++
                          "  row=? col="++ show col),
        ('g',  errorE "goto line unimplemented") 
        ]

