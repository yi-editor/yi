{-# OPTIONS -fglasgow-exts #-}
--
-- for pattern guards
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

import Yi.Yi         hiding ( keymap )
import Yi.Editor            ( Action )
import Yi.Char
import Yi.MkTemp            ( mkstemp )
import qualified Yi.Map as M

import Numeric              ( showOct )
import Data.Char            ( ord, chr, isSpace )
import Data.Bits
import Data.List            ((\\), isPrefixOf)

import System.IO            ( hPutStr, hClose )

-- 
-- MG(1)                      OpenBSD Reference Manual                      MG(1)
-- 
-- NNAAMMEE
--      mg - emacs-like text editor
-- 
-- SSYYNNOOPPSSIISS
--      mg [_o_p_t_i_o_n_s] [_f_i_l_e _._._.]
-- 
-- DDEESSCCRRIIPPTTIIOONN
--      mg is intended to be a small, fast, and portable editor for people who
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
--      mg differs primarily in not having special modes for tasks other than
--      straight editing, e.g., mail and news, and in not having special modes
--      that support various programming languages.  It does have text justifica-
--      tion and auto-fill mode.  Since it is written completely in C, there is
--      no language in which you can write extensions.  However, you can rebind
--      keys and change some parameters.  There are no limits to line length or
--      format.  Command, buffer, and file name completion and listing can be
--      done using the spacebar and `?', respectively.
-- 
--      Amongst other major differences, the mg configuration files are much sim-
--      pler than real emacs.  There are two configuration files, _._m_g, and _._m_g_-
--      _T_E_R_M.  Here, TERM represents the name of your terminal type; e.g., if
--      your terminal type is set to ``vt100'', mg will use _._m_g_-_v_t_1_0_0 as a start-
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
--      away the next time you type a real command.  In mg, you must use "^X-1"
--      to get rid of it.
-- 
--      The undo feature has a minor difference compared to the same feature in
--      Gnu Emacs.  When the end of the undo records list is reached, mg will not
--      stop and inform the user for one undo keystroke before continuing.
-- 
-- OpenBSD 3.7                    February 25, 2000                             2
--

------------------------------------------------------------------------

c_ :: Char -> Char
c_ = ctrlLowcase

m_ :: Char -> Char
m_ = setMeta

-- ---------------------------------------------------------------------
-- map extended names to corresponding actions
--
extended2action :: M.Map String Action
extended2action = M.fromList [ (ex,a) | (ex,_,a) <- globalTable ]

--
-- map keystrokes to extended names
--
keys2extended   :: M.Map [Char] String
keys2extended   = M.fromList [ (k,ex) | (ex,ks,_) <- globalTable, k <- ks ]

--
-- map chars to actions
--
keys2action :: [Char] -> Action
keys2action ks | Just ex <- M.lookup ks keys2extended
               , Just a  <- M.lookup ex extended2action  = a
               | otherwise = errorE $ "No binding for "++ show ks

--
-- keystrokes only 1 character long
--
unitKeysList :: [Char]
unitKeysList = [ k | (_,ks,_) <- globalTable, [k] <- ks ]

--
-- C-x mappings
--
ctrlxKeysList :: [Char]
ctrlxKeysList = [ k | (_,ks,_) <- globalTable, ['\^X',k] <- ks ]

--
-- M-O mappings
--
metaoKeysList :: [Char]
metaoKeysList = [ k | (_,ks,_) <- globalTable, [m,k] <- ks, m == m_ 'O' ]

------------------------------------------------------------------------
--
-- global key/action/name map
--
globalTable :: [(String,[String],Action)]
globalTable = [
  ("apropos",                   
        [[c_ 'h', 'a']],
        errorE "apropos unimplemented"),
  ("backward-char",             
        [[c_ 'b'], [m_ 'O', 'D'], [keyLeft]],
        leftE),
  ("backward-kill-word",        
        [[m_ '\127']],
        bkillWordE),
  ("backward-word",             
        [[m_ 'b']],
        prevWordE),
  ("beginning-of-buffer",       
        [[m_ '<']],
        topE),
  ("beginning-of-line",         
        [[c_ 'a'], [m_ 'O', 'H']],
        solE),
  ("call-last-kbd-macro",       
        [[c_ 'x', 'e']],
        errorE "call-last-kbd-macro unimplemented"),
  ("capitalize-word",           
        [[m_ 'c']],
        capitaliseWordE),
  ("copy-region-as-kill",       
        [[m_ 'w']],
        errorE "copy-region-as-kill unimplemented"),
  ("delete-backward-char",      
        [['\127'], ['\BS'], [keyBackspace]],
        bdeleteE),
  ("delete-blank-lines",        
        [[c_ 'x', c_ 'o']],
        errorE "delete-blank-lines unimplemented"),
  ("delete-char",               
        [[c_ 'd']],
        deleteE),
  ("delete-horizontal-space",   
        [[m_ '\\']],
        errorE "delete-horizontal-space unimplemented"),
  ("delete-other-windows",      
        [[c_ 'x', '1']],
        errorE "delete-other-windows unimplemented"),
  ("delete-window",             
        [[c_ 'x', '0']],
        closeE),
  ("describe-bindings",         
        [[c_ 'h', 'b']],
        describeBindings),
  ("describe-key-briefly",      
        [[c_ 'h', 'c']],
        msgE "Describe key briefly: " >> cmdlineFocusE >> metaM describeKeymap),
  ("digit-argument",            
        [ [m_ d] | d <- ['0' .. '9'] ],
        errorE "digit-argument unimplemented"),
  ("dired",                     
        [[c_ 'x', 'd']],
        errorE "dired unimplemented"),
  ("downcase-region",           
        [[c_ 'x', c_ 'l']],
        errorE "downcase-region unimplemented"),
  ("downcase-word",             
        [[m_ 'l']],
        lowercaseWordE),
  ("end-kbd-macro",             
        [[c_ 'x', ')']],
        errorE "end-kbd-macro unimplemented"),
  ("end-of-buffer",             
        [[m_ '>']],
        botE),
  ("end-of-line",               
        [[c_ 'e'], [m_ 'O', 'F']],
        eolE),
  ("enlarge-window",            
        [[c_ 'x', '^']],
        errorE "enlarge-window unimplemented"),
  ("exchange-point-and-mark",   
        [[c_ 'x', c_ 'x']],
        errorE "exchange-point-and-mark unimplemented"),
  ("execute-extended-command",  
        [[m_ 'x']],
        msgE "M-x " >> cmdlineFocusE >> metaM metaXmap),
  ("fill-paragraph",            
        [[m_ 'q']],
        errorE "fill-paragraph unimplemented"),
  ("find-alternate-file",       
        [[c_ 'c', c_ 'v']],
        errorE "find-alternate-file unimplemented"),
  ("find-file",                 
        [[c_ 'x', c_ 'f']],
        errorE "find-file unimplemented"),
  ("find-file-other-window",    
        [[c_ 'x', '4', c_ 'f']],
        errorE "find-file-other-window unimplemented"),
  ("forward-char",              
        [[c_ 'f'], [m_ 'O', 'C'], [keyRight]],
        rightE),
  ("forward-paragraph",         
        [[m_ ']']],
        nextNParagraphs 1),
  ("forward-word",              
        [[m_ 'f']],
        nextWordE),
  ("goto-line",                 
        [[c_ 'x', 'g']],
        errorE "goto-line unimplemented"),
  ("help-help",                 
        [[c_ 'h', c_ 'h']],
        errorE "help-help unimplemented"),
  ("insert-file",               
        [[c_ 'x', 'i']],
        errorE "insert-file unimplemented"),
  ("isearch-backward",          
        [[c_ 'r']],
        errorE "isearch-backward unimplemented"),
  ("isearch-forward",           
        [[c_ 's']],
        errorE "isearch-forward unimplemented"),
  ("just-one-space",            
        [[m_ ' ']],
        insertE ' '),
  ("keyboard-quit",             
        [[c_ 'g'], 
         [c_ 'h', c_ 'g'], 
         [c_ 'x', c_ 'g'], 
         [c_ 'x', '4', c_ 'g'],
         [m_ (c_ 'g')]
        ],
        msgE "Quit" >> metaM defaultKeymap),
  ("kill-buffer",               
        [[c_ 'x', 'k']],
        closeE),
  ("kill-line",                 
        [[c_ 'k']],
        readRestOfLnE >>= setRegE >> killE),
  ("kill-region",               
        [[c_ 'w']],
        errorE "kill-region unimplemented"),
  ("kill-word",                 
        [[m_ 'd']],
        killWordE),
  ("list-buffers",              
        [[c_ 'x', c_ 'b']],
        errorE "list-buffers unimplemented"),
  ("negative-argument",         
        [[m_ '-']],
        errorE "negative-argument unimplemented"),
  ("newline",                   
        [['\n']],
        insertE '\n'),
  ("newline-and-indent",        
        [],
        errorE "newline-and-indent unimplemented"),
  ("next-line",                 
        [[c_ 'n'], [m_ 'O', 'B'], [keyDown]],
        downE),
  ("not-modified",              
        [[m_ '~']],
        errorE "not-modified unimplemented"),
  ("open-line",                 
        [[c_ 'o']],
        solE >> insertE '\n' >> upE),
  ("other-window",              
        [[c_ 'x', 'n'], [c_ 'x', 'o']],
        nextWinE),
  ("previous-line",             
        [[c_ 'p'], [m_ 'O', 'A'], [keyUp]],
        upE),
  ("previous-window",           
        [[c_ 'x', 'p']],
        prevWinE),
  ("query-replace",             
        [[m_ '%']],
        errorE "query-replace unimplemented"),
  ("quoted-insert",             
        [[c_ 'q']],
        errorE "quoted-insert unimplemented"),
  ("recenter",                  
        [[c_ 'l']],
        errorE "recenter unimplemented"),
  ("save-buffer",               
        [[c_ 'x', c_ 's']],
        fwriteE), -- should know if a filename has been set
  ("save-buffers-kill-emacs",   
        [[c_ 'x', c_ 'c']],
        fwriteE >> quitE),
  ("save-some-buffers",         
        [[c_ 'x', 's']],
        errorE "save-some-buffers unimplemented"),
  ("scroll-down",               
        [[m_ '[', '5', '~'], [m_ 'v'], [keyPPage]],
        upScreenE),
  ("scroll-other-window",       
        [[m_ (c_ 'v')]],
        errorE "scroll-other-window unimplemented"),
  ("scroll-up",                 
        [[c_ 'v'], [m_ '[', '6', '~'], [keyNPage]],
        downScreenE),
  ("search-backward",           
        [[m_ 'r']],
        errorE "search-backward unimplemented"),
  ("search-forward",            
        [[m_ 's']],
        errorE "search-forward unimplemented"),
  ("set-fill-column",           
        [[c_ 'x', 'f']],
        errorE "set-fill-column unimplemented"),
  ("set-mark-command",          
        [[c_ ' ']],
        errorE "set-mark-command unimplemented"),
  ("split-window-vertically",   
        [[c_ 'x', '2']],
        splitE),
  ("start-kbd-macro",           
        [[c_ 'x', '(']],
        errorE "start-kbd-macro unimplemented"),
  ("suspend-emacs",             
        [[c_ 'z']],
        suspendE),
  ("switch-to-buffer",          
        [[c_ 'x', 'b']],
        errorE "switch-to-buffer unimplemented"),
  ("switch-to-buffer-other-window", 
        [[c_ 'x', '4', 'b']],
        errorE "switch-to-buffer-other-window unimplemented"),
  ("transpose-chars",           
        [[c_ 't']],
        swapE),
  ("undo",                      
        [[c_ 'x', 'u'], [c_ '_']],
        undoE),
  ("universal-argument",        
        [[c_ 'u']],
        errorE "universal-argument unimplemented"),
  ("upcase-region",             
        [[c_ 'x', c_ 'u']],
        errorE "upcase-region unimplemented"),
  ("upcase-word",               
        [[m_ 'u']],
        uppercaseWordE),
  ("what-cursor-position",      
        [[c_ 'x', '=']],
        whatCursorPos),
  ("write-file",                
        [[c_ 'x', c_ 'w']],
        fwriteE), -- should prompt for a filename
  ("yank",                      
        [[c_ 'y']],
        errorE "yank unimplemented")
  ]

------------------------------------------------------------------------

type MgMode = Lexer MgState Action

data MgState = MgState { 
        acc    :: String,       -- a line buffer
        prompt :: String        -- current prompt
     }

dfltState :: MgState
dfltState = MgState [] []

defaultKeymap :: [Char] -> [Action]
defaultKeymap = keymap

------------------------------------------------------------------------

keymap :: [Char] -> [Action]
keymap cs = let (actions,_,_) = execLexer mode (cs, dfltState) in actions

------------------------------------------------------------------------

-- default bindings
mode :: MgMode
mode = insert >||< command >||< 
       ctrlxSwitch  >||< 
       metaSwitch   >||<
       metaOSwitch  >||<
       metaXSwitch

------------------------------------------------------------------------

-- self insertion
insert :: MgMode
insert  = anything `action` \[c] -> Just (insertE c)
 
-- C- commands
command :: MgMode
command = cmd `action` \[c] -> Just $ if c `elem` unitKeysList
                                        then keys2action [c]
                                        else undefined
        where cmd = alt $ unitKeysList

------------------------------------------------------------------------

-- switch to ctrl-X submap
ctrlxSwitch :: MgMode
ctrlxSwitch = char '\^X' 
        `meta` \_ st -> (with (msgE "C-x-"), st, Just ctrlxMode)

-- ctrl x submap
ctrlxMode :: MgMode
ctrlxMode = cmd 
        `meta` \[c] st -> (with (msgClrE >> f c), st, Just mode)
        where
            cmd = alt ctrlxKeysList
            f c = if c `elem` ctrlxKeysList
                    then keys2action ['\^X',c]
                    else undefined

------------------------------------------------------------------------
--
-- on escape, we'd also like to switch to M- mode
--

-- switch to meta mode
metaSwitch :: MgMode
metaSwitch = char '\ESC'        -- hitting ESC also triggers a meta char
        `meta` \_ st -> (with (msgE "ESC-"), st,  Just metaMode)

-- 
-- a fake mode. really just looking up the binding for: m_ c
--
metaMode :: MgMode
metaMode = alt ['\0' .. '\255']       -- not quite right
        `meta` \[c] st -> (Just (Right (msgClrE >> f c)), st, Just mode) -- and leave
        where
            f c = if (m_ c) `elem` unitKeysList
                    then keys2action [m_ c]
                    else undefined

------------------------------------------------------------------------

-- switch to meta O mode
metaOSwitch :: MgMode
metaOSwitch = char (m_ 'O')
        `meta` \_ st -> (Just (Right (msgE "ESC-O-")), st,  Just metaOMode)

metaOMode :: MgMode
metaOMode = cmd 
        `meta` \[c] st -> (Just (Right (msgClrE >> f c)), st, Just mode)
        where
            cmd = alt metaoKeysList
            f c = if c `elem` metaoKeysList
                      then keys2action [m_ 'O',c]
                      else undefined

------------------------------------------------------------------------

-- execute an extended command
metaXSwitch :: MgMode
metaXSwitch = (char (m_ 'x') >|< char (m_ 'X'))
        `meta` \_ _ -> (with (msgE "M-x " >> cmdlineFocusE)
                       , metaXEnterState
                       , Just metaXMode)

metaXEnterState :: MgState
metaXEnterState = MgState { prompt = "M-x ", acc = [] }

metaXmap :: [Char] -> [Action]
metaXmap cs = 
        let (actions,_,_) = execLexer metaXMode (cs, metaXEnterState) 
        in actions

--
-- a line buffer mode, where we ultimately map the command back to a
-- keystroke, and execute that.
--
metaXMode :: MgMode
metaXMode = metaXChar >||<  metaXEdit >||< metaXEval

------------------------------------------------------------------------

-- accumulate characters after the M-x prompt
metaXChar :: MgMode
metaXChar = anyButDelNlArrow
        `meta` \[c] st -> (with (msgE (prompt st ++ (reverse (acc st)) ++ [c]))
                          , st{acc=c:acc st}
                          , Just metaXMode)

    -- display old prompt + this char
    where anyButDelNlArrow = alt $ any' \\ (enter' ++ delete' ++ ['\ESC',keyUp,keyDown])

-- edit the M-x line
metaXEdit :: MgMode
metaXEdit = delete
    `meta` \_ st -> 
        let cs' = case acc st of 
                        []    -> []
                        (_:xs) -> xs
        in (with (msgE (prompt st ++ reverse cs')), st{acc=cs'}, Just metaXMode)

-- tab completion
-- metaXTab :: MgMode

-- metaXEscape
        
metaXEval :: MgMode
metaXEval = enter
    `meta` \_ MgState{acc=cca} -> 
        let cmd = reverse cca 
        in case M.lookup cmd extended2action of
                Nothing -> (with $ msgE "[No match]" >> cmdlineUnFocusE, MgState [] [], Just mode)
                Just a  -> (with (cmdlineUnFocusE  >> msgClrE >> a), MgState [] [], Just mode)

------------------------------------------------------------------------

describeKeyMode :: MgMode
describeKeyMode = describeChar

describeKeymap :: [Char] -> [Action]
describeKeymap cs = 
        let (actions,_,_) = execLexer describeKeyMode (cs, describeKeyEnterState) 
        in actions

describeKeyEnterState :: MgState
describeKeyEnterState = MgState { prompt = "Describe key briefly: ", acc = [] }

describeChar :: MgMode
describeChar = anything
    `meta` \[c] st -> 
        let acc' = c : acc st
            keys = reverse acc'
        in case M.lookup keys keys2extended of
            Just ex -> (with $ (msgE $ (printable keys) ++ " runs the command " ++ ex)
                                 >> cmdlineUnFocusE 
                       ,dfltState, Just mode)
            Nothing -> 
                -- only continue if this is the prefix of something in the table
                if any (isPrefixOf keys) (M.keys keys2extended)
                   then (with $ msgE (prompt st ++ keys)
                        ,st{acc=acc'}, Just describeKeyMode)
                   else (with $ (msgE $ printable keys ++ " is not bound to any function")
                                >> cmdlineUnFocusE
                        ,dfltState, Just mode)

------------------------------------------------------------------------
-- translate a string into the emacs encoding of that string
--
printable :: String->String
printable = dropSpace . printable'
    where 
        printable' ('\ESC':a:ta) = "M-" ++ [a] ++ printable' ta
        printable' ('\ESC':ta) = "ESC " ++ printable' ta
        printable' (a:ta) 
                | ord a < 32 
                = "C-" ++ [chr (ord a + 96)] ++ " " ++ printable' ta
                | isMeta a
                = "M-" ++ [clrMeta a] ++ " " ++ printable' ta
                | otherwise  = [a, ' '] ++ printable' ta
        printable' [] = []

------------------------------------------------------------------------
-- Mg-specific actions

whatCursorPos :: Action
whatCursorPos = do 
        (_,_,ln,col,pt,pct) <- bufInfoE
        c <- readE
        msgE $ "Char: "++[c]++" (0"++showOct (ord c) ""++
                ")  point="++show pt++
                "("++pct++
                ")  line="++show ln++
                "  row=? col="++ show col


describeBindings :: Action
describeBindings = do
    mf <- mkstemp "/tmp/yi.XXXXXXXXXX" 
    case mf of
        Nothing    -> error "Yi.Keymap.Mg: mkstemp failed"
        Just (f,h) -> hPutStr h s >> hClose h >> splitE >> fnewE f
    where
      s = unlines [ printable k ++ "\t\t" ++ ex 
                  | (ex,ks,_) <- globalTable
                  , k         <- ks ]

------------------------------------------------------------------------
--  
-- some regular expressions

any', enter', delete' :: [Char]
enter'   = ['\n', '\r']
delete'  = ['\BS', '\127', keyBackspace ]
any'     = ['\0' .. '\255']

delete, enter, anything :: Regexp MgState Action
delete  = alt delete'
enter   = alt enter'
anything  = alt any'

dropSpace :: [Char] -> [Char]
dropSpace = let f = reverse . dropWhile isSpace in f . f
