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

--
-- | An emulation of the Nano editor
--

module Yi.Keymap.Nano where

import Yi.Editor            ( Action )
import Yi.Yi hiding         ( keymap )

import Data.Char            ( chr, isAlphaNum )
import Data.List            ( (\\) )
import Data.Maybe           ( fromMaybe )
import Data.FiniteMap

import Control.Exception    ( ioErrors, catchJust, try, evaluate )

--
-- | Top level function. A function of this type is used by the editor
-- main loop to interpret actions. The second argument to @execLexer@ is
-- our default state.
--
keymap :: [Char] -> [Action]
keymap cs = actions
    where 
        (actions,_,_) = execLexer nano_km (cs, Nothing)

--
-- | @NanoMode@  is the type to instantiate the lazy lexer with
--
type NanoMode  = Lexer  NanoState Action

--
-- | The lexer is able to thread state around. Our state may contain a
-- pair of strings, and a char. This is used when reading in filenames
-- typed in the cmd buffer -- to display a prompt, and to accumulated
-- the filename. See the echo keymap below.  In all other modes you can
-- safely ignore the state. The char is the key that was pressed to send
-- us into cmd buffer mode.
--
type NanoState = Maybe (Char,String,String)

--
-- | The default mode is /cmd/ mode. Our other mode is the echo buffer
-- mode. In cmd mode you can insert chars, run commands and switch to
-- the echo buffer mode.
--
nano_km :: NanoMode
nano_km = insChar >||< cmdChar >||< cmdSwitch >||< searchChar

--
-- | Echo buffer mode (typing in file names at the prompt, for example)
--
echo_km :: NanoMode
echo_km = echoAccum >||< echoEdit >||< echoEval

--
-- Here's where we write a bunch of lexer fragments, corresponding to
-- the different behaviours of the editor: char insertion, cmd actions,
-- cmd line buffer editing, etc.
--

--
-- | Normal chars just insert themselves
--
insChar :: NanoMode
insChar = anyButCmdChar
    `action` \[c] -> Just $ insertE c
    where
        anyButCmdChar = alt $ '\r' : map chr [32 .. 126]

--
-- | Command chars run actions. 
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
-- A key\/action table. This is where we actually map command (^) chars
-- to actions.
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
    ,('\^L',        refreshE)
    ,('\^M',        insertE '\n')
    ,('\^N',        downE)
    ,('\^P',        upE)
    ,('\^U',        undoE)
    ,('\^V',        downScreenE)
    ,('\^X',        quitE)
    ,('\^Y',        upScreenE)
    ,('\0',         do moveWhileE (isAlphaNum)      Right
                       moveWhileE (not.isAlphaNum)  Right )
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
-- | Switching to the command buffer
--
-- Sometimes the editor needs to ask a question of the user, in the
-- command buffer. We implement this as a separate lexer (not just lexer
-- fragment) as characters typed in this mode have different meanings to
-- normal mode.
--
-- Certain characters cause the /mode/ switch, which puts us into the
-- alternate lexer. To actually do the switch requires either a lexer
-- /meta/ action which can (purely) return a new lexer to continue with
-- (@echo_km@ in this case) or via a metaM action (which accomplishes
-- approximately the same in the IO monad).
--
-- help-mode in nano to be a separate mode too. There are probably
-- others.
--
-- For example, we see a \^R, we need to print a prompt, and switch to
-- the new mode, which will accumulate filename characters until nl.  To
-- do this we make use of the lexer state. We store the char that caused
-- us to switch modes, a prompt, and the accumulated filename, in this
-- state.  Once the user presses Enter, in the @echo_km@ mode, we are
-- able to evaluate the action char represents (e.g. writing a file).
--
-- Meta-actions return a triple containing maybe an action to perform
-- /right now/, a modified state, and the lexer to continue parsing
-- with.
-- 
-- Since we're going to edit the command buffer, which is still a
-- slighly buffer magic (unfortunately) we need to explicitly switch
-- focus.
--
cmdSwitch :: NanoMode
cmdSwitch = switchChar
    `meta` \[c] _ -> (Just (Right (msgE (prompt c) >> cmdlineFocusE))
                     ,Just (c, prompt c,[]), Just echo_km)
    where
        --
        -- we look up the echoCharFM to find a list of chars to match in
        -- this action, which we then turn into a regex with alt.
        --
        -- also, we can construct a prompt from the second component
        -- of the elem of the same fm.
        --
        switchChar = alt $ keysFM echoCharFM
        prompt  c  = snd $ fromMaybe (undefined,"") (lookupFM echoCharFM c)

--
-- | Nano search behaviour.
--
-- This is a bit subtle, and illustrates the use of metaM to modify the
-- current lexer based on the result of an IO action.  Searching is
-- quite like other mode switches, except that the prompt is based on
-- the outcome of inspecting the regex register in the editor state --
-- an IO action. However, the prompt is stored in the pure lexer state.
-- So, to pass IO results back to the pure lexer requires the metaM
-- action, to replace the current lexer with the supplied argument.
--
-- What will happen? We _abandon_ the current lexer, jumping to a new
-- lexer that begins in echo_km mode, with a state initalised with the
-- argument to 'metaM'. 'metaM' will then cause processing to continue
-- with this new lexer.
--
searchChar :: NanoMode
searchChar = char '\^W'
    `action` \_ -> Just (a >>= \v -> cmdlineFocusE >> metaM v)
    where
        a = do mre <- getRegexE
               let prompt = case mre of     -- create a prompt
                    Nothing      -> "Search: "
                    Just (pat,_) -> "Search ["++pat++"]: "
               msgE prompt
               return (mkKM prompt) -- and run our custom lexer

        --
        -- generate a new keymap lexer, beginning in echo_km mode, with
        -- state initalised with the prompt we created from 'getRegexE'.
        --
        mkKM p = \cs -> let (as,_,_) = execLexer echo_km (cs, Just ('\^W',p,[]))
                        in as

------------------------------------------------------------------------
--
-- echo buffer mode
--

--
-- | Accumulates chars in the state. This is a meta action, as it must
-- modify the implicit state (it adds (or deletes) chars to the filename
-- we're building)
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
-- | A simple line editor. Just chop chars off the end of the filename
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
-- | The user presses \n, we can evaluate the action.  We return the
-- action to perform, an empty state, and the next lexer, which of
-- course is the default lexer (so we're switching back to normal mode).
-- Make sure to clear the command buffer after we're done, and return
-- focus to the normal window.
--
echoEval :: NanoMode
echoEval = enter
    `meta` \_ (Just (c,_,rf)) ->
        let f = reverse rf
            a = case lookupFM echoCharFM c of
                    Just (fn,_) -> fn f
                    Nothing     -> nopE

        in (Just (Right (a >> msgClrE >> cmdlineUnFocusE)),Nothing,Just nano_km)
    where
        enter = alt ['\n', '\r']

--
-- | Actions that mess with the echo (or command) buffer. Notice how
-- these actions take a @String@ as an argument, and the second
-- component of the elem of the fm is a string that could be used as a
-- prompt (in @cmdSwitch@ -- sometimes the prompt is set using an IO
-- action, in which case we ignore the prompt component of the fm)
--
echoCharFM :: FiniteMap Char ((String -> Action), String)
echoCharFM = listToFM $
    [('\^O',     
     (\f -> if f == [] 
            then nopE
            else catchJust ioErrors (do fwriteToE f ; msgE "Wrote current file.") 
                                    (msgE . show) 
     ,"File Name to Write: "))

    ,('\^_',
     (\s -> do e <- try $ evaluate $ read s
               case e of Left _   -> errorE "[ Come on, be reasonable ]"
                         Right ln -> gotoLnE ln >> solE
     ,"Enter line number: "))
    
    ,('\^W',
     (\p -> case p of [] -> searchE Nothing
                      _  -> searchE (Just p)
     ,undefined))
    ]

-- ---------------------------------------------------------------------
-- utilities

undef :: Char -> Action
undef c = errorE $ "Not implemented: " ++ show c

deleteChars :: [Char]
deleteChars  = ['\BS', '\127', keyBackspace]

-- A Nano-ish style (make the '~' markers invisible)
-- Put something like this in your Config.hs
--
--  ui {
--       eof = Style WhiteF DefaultB
--  }
