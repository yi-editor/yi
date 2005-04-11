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

import Data.Char            ( chr,ord ) --, isAlphaNum )
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
        (actions,_,_) = execLexer normalMode (cs,ES "")

type EmacsMode = Lexer EmacsState Action

data EmacsState = ES CommandSequence

scrrep :: Char->String

scrrep a | ord(a)<32 = "C-"++[chr(ord(a)+96),' ']
         | otherwise = [a]

instance Show EmacsState where
  show (ES (a:ta)) = (scrrep a)++(show $ ES ta)
  show (ES []) = []


esss :: (String->String)->EmacsState->EmacsState
esss f (ES a) = ES (f a)



-- the naming is still a little heterogenous, but this is the "normal"
-- key lexer...

normalMode :: EmacsMode
normalMode =      insChar    -- insert a keystroke
           >||< moveChar   -- or move the cursor
           >||< (char '\^X' `meta` \s->(escape wackoMode $ esss (++s))) 
              -- or go Ctl-X
           >||< (char '#'   `meta` \s->(escape wackoMode $ esss (++s))) 
              -- or go Meta-X (this is bound to # for the moment)

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

decode :: EmacsState -> Action
decode (ES ('\^X':('\^S':_))) = msgE "Write"
decode (ES ('\^X':('\^C':_))) = quitE

decode s = msgE ("Unbound Control sequence "++(show s))


-- This is our "wacko mode abstraction". On userlevel you will see:
--
--  regex `meta`  escape (mode) (state->state)
--
-- which, upon regex, escapes into mode and transforms the editor state

escape :: EmacsMode->(EmacsState->EmacsState)->EmacsState->(Maybe (Either e Action),EmacsState,Maybe (Lexer EmacsState Action))

escape m f os = ((Just $ Right $ msgE $ show ns),
                ns,
                Just m)
               where ns = f os

-- On userlevel you will see:
--
--  regex `meta`  endescape (mode) (state->state) (decode)
--
-- which, upon regex, ends an escape, lets 

actescape :: EmacsMode->(EmacsState->Action)->(EmacsState->EmacsState)->(EmacsState->EmacsState)->EmacsState->(Maybe (Either e Action),EmacsState,Maybe (Lexer EmacsState Action))

actescape m act fo fn os = ((Just $ Right $ msgE ("!!!>"++(show (fo os))) >> act (fo os)),
                  (fn (fo os)),
                  Just m)


--bindings :: IORef MArray Integer

wackoMode :: EmacsMode
wackoMode = (alt (map chr [0..32]) `meta` \s->(escape wackoMode $ esss (++s) ))  >||<
            ((alt $ ['\^C','\^S']++(map chr [32..123])) `meta` 
               \s->(actescape normalMode decode (esss (++s)) (\_->ES "")))



{--- junk at the bottom ---}

null_km :: EmacsMode
null_km = epsilon `action` \_ -> Just undefined


--metaXChar :: EmacsMode
--metaXChar = normalChars
