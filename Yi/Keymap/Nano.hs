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
import qualified Yi.Map as M

import Data.Char            ( chr, isAlphaNum )
import Data.List            ( (\\) )
import Data.Maybe           ( fromMaybe )

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
type NanoMode = Lexer NanoState Action

--
-- | The lexer is able to thread state around. Our state may contain a
-- pair of strings, and a char. This is used when reading in filenames
-- typed in the cmd buffer -- to display a prompt, and to accumulated
-- the filename. See the echo keymap below.  In all other modes you can
-- safely ignore the state. The char is the key that was pressed to send
-- us into cmd buffer mode.
--
-- Also, when entering text into the command buffer, in some modes extra
-- key bindings become available (such as searching). Any extra bindings
-- to add to the echo mode are passed in the state as an @OnlyMode@
-- (which is just a wrapped mode)
--
type NanoState = Maybe (Char,String,String,OnlyMode)

--
-- Hide the mode inside a data type to avoid cycles
--
newtype OnlyMode = Only NanoMode

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
-- | The null keymap, useful as an argument to >||< sometimes
--
null_km :: NanoMode
null_km = epsilon `action` \_ -> Just undefined

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
        nanoCmdChar = alt $ M.keys cmdCharFM

        -- lookup an action, given a char. Return nopE (as a backup)
        -- in case it wasn't found.
        theActionOf :: Char -> Action
        theActionOf c = case M.lookup c cmdCharFM of
                            Just a  -> a
                            Nothing -> nopE

--
-- A key\/action table. This is where we actually map command (^) chars
-- to actions.
--
cmdCharFM :: M.Map Char Action
cmdCharFM = M.fromList $
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
    ,('\^X',        do b <- isUnchangedE ; if b then quitE else switch2WriteMode)
    ,('\^Y',        upScreenE)
    ,('\^Z',        suspendE)
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

    where
        --
        -- | print a message and switch to sub-mode lexer for Y\/N questions
        --
        switch2WriteMode :: Action
        switch2WriteMode = do
            msgE "Save modified buffer (ANSWERING \"No\" WILL DESTROY CHANGES) ? "
            cmdlineFocusE
            metaM $ askYN (fwriteE >> quitE) (quitE) keymap

------------------------------------------------------------------------
--
-- | Construct a new lexer for yes, no and cancel questions. The lexer
-- value returned can assume control by applying @metaM@ to that value.
--
-- The lexer table is populated with the first two @Action@ arguments
-- bound to y and n, cancel bound to ctrl-c, and anything else ignored.
-- Once we've processed a valid keystroke, we return control to the
-- keymap specified by the third argument.
--
askYN ::  Action -> Action -> ([Char] -> [Action]) -> ([Char] -> [Action])
askYN y_act n_act cont = 
    \cs -> let (actions,_,_) = execLexer ync_mode (cs, Nothing) in actions

    where
        ync_mode = yes_mode >||< no_mode >||< cancel

        yes_mode = (char 'Y' >|< char 'y') `action` \_ -> Just $ y_act >> done
        no_mode  = (char 'N' >|< char 'n') `action` \_ -> Just $ n_act >> done
        cancel   = char '\^C' `action` \_ -> Just $ msgE "[ Cancelled ]" >> done

        done     = cmdlineUnFocusE >> metaM cont

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
                     ,Just (c, prompt c, [], Only null_km), Just echo_km)
    where

        --
        -- we look up the echoCharFM to find a list of chars to match in
        -- this action, which we then turn into a regex with alt.
        --
        -- also, we can construct a prompt from the second component
        -- of the elem of the same fm.
        --
        switchChar = alt $ M.keys echoCharFM
        prompt  c  = snd $ fromMaybe (undefined,"") (M.lookup c echoCharFM)

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
-- Note that searchChar will override the binding for \^W in switchChar
-- above
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
        mkKM p cs = 
            let (as,_,_) = execLexer (echo_km >||< search_km) 
                                     (cs, Just ('\^W',p,[], Only search_km)) 
            in as

--
-- When searching, a few extra key bindings become available, which
-- immediately interrupt the echo mode, perform an action, and then drop
-- back to normal mode.
--
-- ^G Get Help ^Y First Line  ^R Replace     M-C Case Sens  M-R Regexp
-- ^C Cancel   ^V Last Line   ^T Go To Line  M-B Direction  Up History 
--
-- We augment the echo keymap with the following bindings, by passing
-- them in the @OnlyMode@ field of the lexer state. The echo keymap the
-- knows how to add in these extra bindings.
--
search_km :: NanoMode
search_km = srch_g >||< srch_y >||< srch_v >||< srch_t >||< srch_c >||< srch_r
  where
    srch_g = char '\^G' `andthen` msgE "nano-yi : yi emulating nano"

    srch_y = char '\^Y' `andthen` (gotoLnE 0 >> solE)
    srch_v = char '\^V' `andthen` (do (_,x,_,_,_,_) <- bufInfoE
                                      gotoLnE x >> solE)

    srch_t = char '\^T' `andthen` msgE "unimplemented" -- goto line

    srch_c = char '\^C' `andthen` msgE "[ Search Cancelled ]"
    srch_r = char '\^R' `andthen` msgE "unimplemented"

    -- M-C
    -- M-R
    -- M-B
    -- Up

    -- do 'a' and return to the normal mode
    c `andthen` a = 
        c `meta` \_ _ -> (Just (Right (a>>cmdlineUnFocusE)),Nothing,Just nano_km)

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
    `meta` \[c] (Just (k,p,f,Only m)) -> 
                    (Just (Right (doEcho p f c))  -- echo char
                    ,Just (k,p,c:f,Only m)             -- accum filename in state
                    ,Just (echo_km >||< m))       -- and stay in this mode
    where
        anyButDelOrEnter = alt $ [ '\0' .. '\255' ] \\ (deleteChars ++ [ '\r' ])
        doEcho p f c = msgE $ p ++ (reverse f) ++ [c]

--
-- | A simple line editor. Just chop chars off the end of the filename
-- passed in the state.
--
echoEdit :: NanoMode
echoEdit = delete
    `meta` \_ (Just (c,p,f,Only m)) ->
        let f' | f == []   = []
               | otherwise = tail f
        in (Just (Right (msgE (p ++ reverse f')))
           ,Just (c,p,f',Only m)
           ,Just (echo_km >||< m))
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
    `meta` \_ (Just (c,_,rf,_)) ->
        let f = reverse rf
            a = case M.lookup c echoCharFM of
                    Just (fn,_) -> fn f
                    Nothing     -> nopE

        in (Just (Right (a >> cmdlineUnFocusE)),Nothing,Just nano_km)
    where
        enter = alt ['\n', '\r']

--
-- | Actions that mess with the echo (or command) buffer. Notice how
-- these actions take a @String@ as an argument, and the second
-- component of the elem of the fm is a string that could be used as a
-- prompt (in @cmdSwitch@ -- sometimes the prompt is set using an IO
-- action, in which case we ignore the prompt component of the fm)
--
echoCharFM :: M.Map Char ((String -> Action), String)
echoCharFM = M.fromList $
    [('\^O',     
     (\f -> if f == [] 
            then nopE
            else catchJust ioErrors (do fwriteToE f ; msgE "Wrote current file.") 
                                    (msgE . show) 
     ,"File Name to Write: "))

    ,('\^_',
     (\s -> do e <- try $ evaluate $ read s
               case e of Left _   -> errorE "[ Come on, be reasonable ]"
                         Right ln -> gotoLnE ln >> solE >> msgClrE
     ,"Enter line number: "))
    
    ,('\^W',
     (\p -> case p of [] -> searchE Nothing  [] Right
                      _  -> searchE (Just p) [] Right
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
