-- 
-- Copyright 2005 by B.Zapf
--
-- adapted from Nano.hs
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


{-

Requirements from the community:

sat 9 apr 22:20
 < stepcut> (1) showing the 'C-x - ' stuff in the message bar
 < stepcut> (2) allowing tab completion of M-x commands
 < stepcut> (3) supporting 'C-h k'
 < stepcut> (4) rebinding keys (of course)
 < stepcut> (5) C-u
 < basti_> doing WHAT? ;)
 * basti_ doesnt have that bound
 < stepcut> C-u is bound by default...
 < stepcut> try, C-u 20 a
 < basti_> ah
 < stepcut> it repeats a command
 < basti_> hmm ok
 < stepcut> C-u a will do it 4 times
 < stepcut> it is often, but not always interpreted as a repeat
 < stepcut> also, capturing/replaying keyboard macros
 < basti_> lets stay on the carpet.

 < stepcut> the hard part with (4) is how to express the command you want 
            to bind to at run-time
 < basti_> hmm.
 -!- lisppaste2 [~lisppaste@common-lisp.net] has joined #haskell
 < basti_> is there some sort of eval function?
 < stepcut> in the compiled code, you just bind the key to some code that 
            is compiled
 < basti_> i know
 < basti_> we would need 2-way association
 < stepcut> right

-}

--
-- | An emulation of the Emacs editor
--

module Yi.Keymap.Emacs where

import Yi.Editor            ( Action )
import Yi.Yi hiding         ( keymap )
import Yi.Lexers hiding (Action)

--import qualified Yi.Map as M

import Data.Char            ( chr ) --, isAlphaNum )
--import Data.List            ( (\\) )
--import Data.Maybe           ( fromMaybe )

--import Control.Exception    ( ioErrors, catchJust, try, evaluate )

-- so far, Command Sequences are stored as a string. I thought 
-- making an own datatype for them, for the sake of abstraction, but then
-- realized that this wouldn't earn a lot and complicates matters.   basti_

type CommandSequence = String

keymap :: [Char] -> [Action]
keymap cs = actions
    where 
        (actions,_,_) = execLexer emacs_km (cs, "")

type EmacsMode = Lexer EmacsState Action

type EmacsState = CommandSequence

-- the naming is still a little heterogenous, but this is the "normal"
-- key lexer...

emacs_km :: EmacsMode
emacs_km =      insChar    -- insert a keystroke
           >||< moveChar   -- or move the cursor
           >||< (char '\^X' `meta` escape) -- or go Ctl-X
           >||< (char '#'   `meta` escape) -- or go Meta-X (this is
                                        -- bound to # for the moment)

-- upon the chars 32..126, insertE 

insChar :: EmacsMode
insChar = insertChars `action` \[c] -> Just $ insertE c
          where insertChars = alt $ '\r' : map chr [32..126]

-- cursor motions

moveChar :: EmacsMode
moveChar = (char keyUp    `action` \_ -> Just upE   ) >||<
           (char keyDown  `action` \_ -> Just downE ) >||<
           (char keyRight `action` \_ -> Just rightE) >||<
           (char keyLeft  `action` \_ -> Just leftE ) 


-- this does the decoding of CommandSequences... this approach seems
-- to be good for sequences of a few keys, but it's not favourable for
-- filenames etc.

decode :: CommandSequence -> Action
decode ('\^X':('\^S':_)) = msgE "Write"
decode ('\^X':('\^C':_)) = quitE

decode _ = msgE "Unbound Control sequence"

-- this is some rudimentary "wacko mode abstraction": it escapes a
-- character into the "CommandSequence", displaying it, and goes into
-- commandChar mode.

-- Note: for prettifying the visual feedback, start here at "show ns"...

escape :: String->CommandSequence->(Maybe (Either e Action),CommandSequence,Maybe (Lexer CommandSequence Action))

escape s os = ((Just $ Right $ msgE $ show ns),
                ns,
                Just commandChar)
               where ns = os++s

-- this ends "wacko mode abstraction", going back to emacs_km

endescape :: String->CommandSequence->(Maybe (Either e Action),CommandSequence,Maybe (Lexer CommandSequence Action))

endescape s os = ((Just $ Right $ msgE ("!!!>"++(show ns)) >> decode ns),
                  "",
                  Just emacs_km)
                 where ns = os++s

--bindings :: IORef MArray Integer

commandChar :: EmacsMode
commandChar = char '\^X' `meta` escape  >||<
              ((alt $ ['\^C','\^S']++(map chr [32..123])) `meta` endescape)



{--- junk at the bottom ---}

null_km :: EmacsMode
null_km = epsilon `action` \_ -> Just undefined


--metaXChar :: EmacsMode
--metaXChar = normalChars
