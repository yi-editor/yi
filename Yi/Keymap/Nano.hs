-- 
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

module Yi.Keymap.Nano ( keymap ) where

import Yi.Editor            ( Action )
import Yi.Yi hiding         ( keymap )

import Data.Char            ( chr )
import Data.List            ( (\\) )
import Data.FiniteMap

import Control.Exception    ( ioErrors, catchJust )

--
-- Top level function. A function of this type is used by the editor
-- main loop to interpret actions. The second argument to @execLexer@
-- is our default state.
--
keymap :: [Char] -> [Action]
keymap cs = actions
    where 
        (actions,_,_) = execLexer nano_km (cs, Nothing)

--
-- Define some types to instantiate the lazy lexer to
--
type NanoMode  = Lexer  NanoState Action

--
-- Our state may contain a pair of strings, and a char. This is used
-- when reading in filenames typed in the cmd buffer -- to display a
-- prompt, and to accumulated the filename. See the echo keymap below.
-- In all other modes you can safely ignore the state. The char is the
-- key that was pressed to send us into cmd buffer mode.
--
type NanoState = Maybe (Char,String,String)

--
-- The default mode is the conjunction of the self-insert lexer, and the
-- cmd char lexer.
--
nano_km :: NanoMode
nano_km = insChar >||< cmdChar >||< cmdSwitch

--
-- Echo buffer mode (typing in file names at the prompt, for example)
--
echo_km :: NanoMode
echo_km = echoAccum >||< echoEdit >||< echoEval

--
-- Here's where we write a bunch of lexer fragments, corresponding to
-- the different behaviours of the editor: char insertion, cmd actions,
-- cmd line buffer editing, etc.
--

--
-- normal chars just insert themselves
--
insChar :: NanoMode
insChar = anyButCmdChar
    `action` \[c] -> Just $ insertE c
    where
        anyButCmdChar = alt $ '\r' : map chr [32 .. 126]

--
-- The basic nano keymap. It's a lexer, with an implicit state (of ()),
-- returning Just an editor action (or Nothing), for each key press.
--
cmdChar :: NanoMode
cmdChar = nanoCmdChar
    `action` \[c] -> Just (theActionOf c)
    where
        -- create a regex match any of the chars with entries
        -- in the cmdCharFM
        nanoCmdChar = alt $ keysFM cmdCharFM

        -- lookup an action, given a char. Return nopE (as a backup)
        -- in case it wasn't found.
        theActionOf :: Char -> Action
        theActionOf c = case lookupFM cmdCharFM c of
                            Just a  -> a
                            Nothing -> nopE

--
-- A key\/action table. 
-- This is where we actually map command (^) chars to actions.
--
cmdCharFM :: FiniteMap Char Action
cmdCharFM = listToFM $
    [('\127',       leftE >> deleteE)
    ,('\188',       prevBufW)    -- 'M-<' ?
    ,('\190',       nextBufW)    -- 'M->' ?
    ,('\^A',        solE)
    ,('\^B',        leftE)
    ,('\^D',        deleteE)
    ,('\^E',        eolE)
    ,('\^F',        rightE)
    ,('\^H',        leftE >> deleteE)
    ,('\^J',        undef '\^J')
    ,('\^K',        readRestOfLnE >>= setRegE >> killE)
    ,('\^N',        downE)
    ,('\^P',        upE)
    ,('\^U',        getRegE >>= mapM_ insertE)
    ,('\^V',        downScreenE)
    ,('\^X',        quitE)
    ,('\^Y',        upScreenE)
    ,(keyBackspace, leftE >> deleteE)
    ,(keyDown,      downE)
    ,(keyLeft,      leftE)
    ,(keyRight,     rightE)
    ,(keyUp,        upE)
    ,('\^G',        msgE "nano-yi : yi emulating nano")
    ,('\^I',       (do (_,s,ln,x,p,pct) <- bufInfoE 
                       msgE $ "[ line "++show ln++", col "++show x++
                              ", char "++show p++"/"++show s++" ("++pct++") ]"))
    ]

--
-- We implement the state where the user can enter a file name (for
-- reading, or writing) as a separate lexer (not just lexer fragment).
-- Certain characterrs cause a /mode/ switch, which puts us into the
-- alternate lexer. To actually do the switch requires a /meta/ action
-- which can return a new lexer to continue with -- @echo_km@ in this
-- case.
--
-- I would consider help-mode in nano to be a separate mode too. There
-- are probably others.
--
-- So, we see a \^R, we need to print a prompt, and switch to the new
-- mode, which will accumulate filename characters until nl.  To do this
-- we make use of the lexer state. We store the char that caused us to
-- switch modes, a prompt, and the accumulated filename, in this state.
-- Once the user presses Enter, we are able to evaluate the action char
-- represents (e.g. writing a file).
--
-- Meta-actions return a triple containing maybe an action to perform
-- /right now/, a modified state, and the lexer to continue parsing
-- with.
-- 
-- Since we're going to edit the command buffer, which is still slighly
-- magic (unfortunately) we need to explicitly switch focus.
--
cmdSwitch :: NanoMode
cmdSwitch = char '\^O'
    `meta` \[c] _ -> (Just (Right (msgE msg >> cmdlineFocusE))
                     ,Just (c, msg,[]), Just echo_km)
    where
        msg = "File Name to Write: "

------------------------------------------------------------------------

--
-- A simple line editor, which accumulates chars in the state
-- This is a meta action, as it must modify the implicit state (it adds
-- (or deletes) chars to the filename we're building)
--
echoAccum  :: NanoMode
echoAccum = anyButDelOrEnter
    `meta` \[c] (Just (k,p,f)) -> (Just (Right (doEcho p f c))  -- echo char
                                  ,Just (k,p,c:f)    -- accum filename in state
                                  ,Just echo_km)     -- and stay in this mode
    where
        anyButDelOrEnter = alt $ [ '\0' .. '\255' ] \\ (deleteChars ++ [ '\r' ])
        doEcho p f c = msgE $ p ++ (reverse f) ++ [c]

--
-- A simple line editor. Just chop chars off the end of the filename
-- passed in the state.
--
echoEdit :: NanoMode
echoEdit = delete
    `meta` \_ (Just (c,p,f)) ->
        let f' | f == []   = []
               | otherwise = tail f
        in (Just (Right (msgE (p ++ reverse f'))), Just (c,p,f'), Just echo_km)
    where
        delete = alt deleteChars

--
-- The user presses \n, we can evaluate the action.
-- We return the action to perform, an empty state, and the next lexer,
-- which of course is the default lexer (so we're switching back to
-- normal mode). Make sure to clear the command buffer after we're
-- done, and return focus to the normal window.
--
echoEval :: NanoMode
echoEval = enter
    `meta` \_ (Just (c,_,rf)) ->
        let f = reverse rf
            a = if f == [] then nopE 
                           else getAction f c
        in (Just (Right (a >> msgClrE >> cmdlineUnFocusE)),Nothing,Just nano_km)
    where
        enter = alt ['\n', '\r']

        --
        -- Just hard-code the mapping here, rather than use an FM. No
        -- reason why we'd do this, however.
        --
        -- TODO need a new fwriteE function that takes a file name argument.
        --
        getAction f c = case c of
            '\^O' -> catchJust ioErrors (do fwriteToE f
                                            msgE "Wrote current file.") 
                                        (msgE . show)
            _     -> undef c

-- ---------------------------------------------------------------------
-- utilities

{-
isDel :: Char -> Bool
isDel '\BS'        = True
isDel '\127'       = True
isDel c | c == keyBackspace = True
isDel _            = False
-}

undef :: Char -> Action
undef c = errorE $ "Not implemented: " ++ show c

deleteChars :: [Char]
deleteChars  = ['\BS', '\127', keyBackspace]

--
-- A Nano-ish style (make the '~' markers invisible)
-- Put something like this in your Config.hs
--
--  ui {
--       eof = Style WhiteF DefaultB
--  }
--

