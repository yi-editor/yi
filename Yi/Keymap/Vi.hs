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
-- | Y I -- as the name suggests ;) -- uses vi as its default key
-- bindings. Feel free to write your own bindings in ~/.yi/Keymap.hs.
-- You must provide a function 'keymap' of type: Char -> Action
--

module Yi.Keymap.Vi ( keymap ) where

import Yi.Core
import Yi.Editor    ( Keymap(..) )  -- just for now
import Yi.UI        -- hack, just for now, so we can see key defns

import Data.Char
import Data.Maybe
import Control.Monad

-- ---------------------------------------------------------------------
--
-- This function must be implemented by any user keybinding
--
keymap :: Char -> IO Keymap
keymap c = cmd c        -- vi starts in command mode

--
-- | Return the next mode to use
--
nextCmd, nextIns, nextEx :: IO Keymap
nextCmd = return (Keymap cmd)
nextIns = return (Keymap ins)
nextEx  = return (Keymap ex)

-- ---------------------------------------------------------------------
-- | Command mode
--
-- What's going on here? We match a keystroke (or multiple keystrokes)
-- on the lhs, and perform an editor action on the rhs. Finally, we
-- return the keymap function we wish to use for the next action -- this
-- is how we switch modes.
--
-- If we see digits, we need to accumulate them as they may be needed to
-- repeat actions.
--
cmd :: Char -> IO Keymap
cmd c | isDigit c = getdigits [c] >>= uncurry do_cmd
      | otherwise = do_cmd Nothing c
  where
    getdigits :: [Char] -> IO (Maybe Int, Char)    
    getdigits cs = do 
        c' <- getcE
        if isDigit c' then getdigits (c':cs) 
                      else return (Just (read $ reverse cs), c')

do_cmd :: (Maybe Int) -> Char -> IO Keymap

do_cmd mi c
-- Search forward for the current word.
    | c == '\^A' = not_impl 

-- Page backwards count screens.
    | c == '\^B' || c == keyPPage 
    = upScreensE (fromMaybe 1 mi) >> nextCmd

-- Scroll forward count lines.
    | c == '\^D' = not_impl

-- Scroll forward count lines, leaving the current line and column as is
    | c == '\^E' = not_impl

-- Page forward count screens.
    | c == keyNPage || c == '\^F'    
    = downScreensE (fromMaybe 1 mi) >> nextCmd

-- Display the file information.
    | c == '\^G' = do (f,_,ln,_,_,pct) <- bufInfoE 
                      msgE $ show f ++ " Line " ++ show ln ++ " ["++ pct ++"]"
                      nextCmd

-- Move the cursor back count characters in the current line.
    | c == 'h' || c == '\^H'  || c == keyLeft
    = leftOrSolE (fromMaybe 1 mi) >> nextCmd

-- Move the cursor down count lines without changing the current column.
    | c == 'j' || c == '\^J' || c == '\^N'  || c == keyDown
    = replicateM_ (fromMaybe 1 mi) downE >> nextCmd

-- Repaint the screen.
    | c == '\^L' || c == '\^R' = not_impl

-- Move the cursor down count lines to the first non-blank character.
    | c == '\^M' || c == '+' = not_impl

-- Move the cursor up one line
    | c == '\^P' || c == 'k' || c == keyUp
    = replicateM_ (fromMaybe 1 mi) upE >> nextCmd     
    
-- Return to the most recent tag context.
    | c == '\^T' = not_impl

-- Scroll backwards count lines.
    | c == '\^U' = not_impl

-- Switch to the next lower screen in the window
    | c == '\^W'    = nextWinE >> nextCmd

-- Scroll backwards count lines
    | c == '\^Y'    = not_impl

-- Suspend the current editor session.
    | c == '\^Z'    = not_impl

-- Execute ex commands or cancel partial commands.
    | c == '\ESC'   = not_impl

-- Push a tag reference onto the tag stack.
    | c == '\^]'    = not_impl

-- Switch to the most recently edited file.
    | c == '\^^'    = not_impl

--  Move the cursor right one character.
    | c ==  'l' || c == ' ' || c == keyRight
    = rightOrEolE (fromMaybe 1 mi) >> nextCmd

-- Replace text with results from a shell command.
    | c == '!'   = not_impl

-- Increment or decrement the number under the cursor. (this is weird!)
    | c == '#'  = not_impl

-- Move the cursor to the end of a line, n times.
    | c == '$'  = eolE >> nextCmd   -- TODO doesn't respect 'count'

-- Move to the matching character, from the set of ([{}]), /* */, cpp stuff
    | c == '%'  = not_impl

-- Repeat the previous substitution command on the current line.
    | c == '&'  = not_impl

-- Back up count sentences.
    | c == '('  = not_impl

-- Move forward count sentences.
    | c == ')'  = not_impl

-- Reverse find character count times.
    | c == ','  = not_impl

-- Move to the first non-blank of the previous line, count times.
    | c == '-'  = not_impl

-- Repeat the last vi command that modified text.
    | c == '.'  = not_impl

-- search! not regular expressions at the moment.
    | c == '/'  = do
        msgE "/"
        let loop w  = do
                k <- getcE
                case () of {_
                    | k == '\n'         -> return (reverse w)
                    | k == '\r'         -> return (reverse w)
                    | k == '\BS'        -> del w >>= loop
                    | k == keyBackspace -> del w >>= loop
                    | otherwise         -> msg (reverse (k:w)) >> loop (k:w)
                }
                where msg s = msgE ('/' : s)
                      del []     = msg []           >> return []
                      del (_:cs) = msg (reverse cs) >> return cs

        s <- loop []
        searchE (Just s)
        nextCmd

    | c == 'n' = searchE Nothing >> nextCmd    -- > reuse last expression

    | c == 'N' || c == '?'  = not_impl

-- Move to the first character in the current line.
    | c == '0'   = solE >> nextCmd

-- Execute an ex command.
    | c == ':'   = msgClrE >> msgE ":" >> nextEx 

-- Repeat the last character find count times.
    | c == ';'   = not_impl

-- Shift lines right. TODO 4 is not really very good...
    | c ==  '>' 
    = do c' <- getcE
         when (c' == '>') $ replicateM_ (fromMaybe 1 mi) $ 
                solE >> mapM_ insertE (replicate 4 ' ')
         nextCmd

-- Shift lines left.
    | c == '<'  = not_impl

-- Execute a named buffer
    | c == '@'  = not_impl

-- Enter input mode, appending the text after the end of the line.
    | c == 'A' = eolE            >> nextIns

-- Move backwards count bigwords
    | c == 'B' = not_impl

-- Change text from the current position to the end-of-line.
-- If buffer is specified, ``yank'' the deleted text into buffer. TODO
-- todo-- not vi.
    | c == 'C' = readRestOfLnE >>= setRegE >> killE >> nextIns

-- Delete text from the current position to the end-of-line.
    | c == 'D' = readRestOfLnE >>= setRegE >> killE >> nextCmd

--  Move forward count end-of-bigwords.
    | c == 'E' = not_impl

-- Search count times backward through the current line for character
    | c == 'F' = not_impl

-- Move to line count, or the last line of the file if count is not specified.
    | c == 'G' = do case mi of
                        Nothing     -> botE
                        Just n      -> gotoLnE n -- topE
                    nextCmd

-- Move to the screen line count - 1 lines below the top of the screen
    | c == 'H' = downFromTosE ((fromMaybe 1 mi ) - 1) >> nextCmd

-- Enter input mode, inserting the text at the beginning of the line.
    | c == 'I' = solE              >> nextIns

-- Join lines.
    | c == 'J' = eolE >> deleteE{-'\n'-} >> nextCmd

-- Move to the screen line count - 1 lines above the bottom of the screen.
    | c == 'L' = upFromBosE ((fromMaybe 1 mi ) - 1) >> nextCmd

-- Move to the screen line in the middle of the screen.
    | c == 'M' = middleE  >> nextCmd

-- Enter input mode, appending text in a new line above the current line.
    | c == 'O' = solE >> insertE '\n' >> upE >> nextIns

-- Insert text from a buffer.
    | c == 'P' = not_impl

-- Exit vi (or visual) mode and switch to ex mode.
    | c == 'Q' = not_impl

-- Enter input mode, replacing the characters in the current line.
    | c == 'R' = not_impl

-- Substitute count lines.
    | c == 'S' = solE >> readLnE >>= setRegE >> killE >> nextIns

-- Search backwards, count times, through the current line for the
-- character after the specified character.
    | c == 'T' = not_impl

-- Restore the current line to its state before the cursor last moved to it.
    | c == 'U' = not_impl

-- Move forward count bigwords.
    | c == 'W' = not_impl

-- Delete count characters before the cursor. (TODO yank not impl).
    | c == 'X' 
    = let n = (fromMaybe 1 mi) in leftOrSolE n >> replicateM_ n deleteE >> nextCmd

-- Copy (or ``yank'') count lines into the specified buffer
    | c == 'Y' = not_impl

-- Write the file and exit vi.
    | c == 'Z' = do c' <- getcE
                    when (c' == 'Z') $ viWrite >> quitE
                    nextCmd

------------------------------------------------------------------------

-- Enter input mode, appending the text after the cursor.
    | c == 'a' = rightOrEolE 1     >> nextIns

-- Move to the start of the current line.
    | c == '|'  = solE             >> nextCmd

-- Delete count characters.
    | c == 'x'  
    = replicateM_ (fromMaybe 1 mi) deleteE >> nextCmd

-- Copy the line the cursor is on.
    | c == 'y' = do c' <- getcE 
                    if c' == 'y' 
                        then readLnE >>= setRegE
                        else nopE
                    nextCmd

-- Append the copied line after the line the cursor is on.
    | c == 'p' = do s <- getRegE
                    eolE >> insertE '\n' >> mapM_ insertE s >> solE
                    nextCmd

-- Enter input mode, inserting the text before the cursor.
    | c == 'i' = nextIns

-- Enter input mode, appending text in a new line under the current line.
    | c == 'o' = eolE >> insertE '\n' >> nextIns


-- Delete the line the cursor is on.
    | c == 'd' = do c' <- getcE
                    when (c' == 'd') $ solE >> killE >> deleteE
                    nextCmd

-- Replace character.
    | c == 'r' = getcE >>= writeE >> nextCmd

-- Reverse the case of the next character
    | c == '~' = do c' <- readE
                    let c'' = if isUpper c' then toLower c' else toUpper c'
                    writeE c''
                    nextCmd

    | otherwise = nopE >> nextCmd

    where not_impl = msgE "Not implemented" >> nextCmd

-- ---------------------------------------------------------------------
-- * Insert mode
--
ins :: Char -> IO Keymap
-- Return to command mode.
ins '\ESC'  = leftOrSolE 1 >> nextCmd

-- Erase the last character.
ins c | isDel c       = deleteE       >> nextIns
      | c == keyPPage = upScreenE     >> nextIns
      | c == keyNPage = downScreenE   >> nextIns

-- Insert character
ins c  = do 
        (_,s,_,_,_,_) <- bufInfoE
        when (s == 0) $ insertE '\n' -- vi behaviour at start of file
        insertE c
        nextIns

-- ---------------------------------------------------------------------
-- * Ex mode
-- accumulate keys until esc or \n, then try to work out what was typed
--
-- TODO think about how to do this as a better lexer
--
ex :: Char -> IO Keymap

ex k = msgClrE >> loop [k]
  where
    loop [] = do msgE ":"
                 c <- getcE
                 if isDel c
                    then msgClrE >> nextCmd       -- deleted request
                    else loop [c]
    loop w@(c:cs) 
        | isDel c           = deleteWith cs
        | c == '\ESC'       = msgClrE             >> nextCmd
        | c == '\r'         = execEx (reverse cs) >> nextCmd
        | otherwise         = do msgE (':':reverse w)
                                 c' <- getcE
                                 loop (c':w)

    execEx :: String -> Action
    execEx "w"   = viWrite
    execEx "q"   = closeE
    execEx "q!"  = closeE
    execEx "wq"  = viWrite >> quitE
    execEx "n"   = nextBufW
    execEx "N"   = nextBufW
    execEx "p"   = prevBufW
    execEx "P"   = prevBufW
    execEx "sp"  = splitE
    execEx ('e':' ':f)  = fnewE f
    execEx ('s':'/':cs) = viSub cs
    execEx cs    = viCmdErr cs

    deleteWith []     = msgClrE >> msgE ":"      >> loop []
    deleteWith (_:cs) = msgClrE >> msgE (':':cs) >> loop cs

-- ---------------------------------------------------------------------
-- | Try to do a substitute
--
viSub :: [Char] -> Action
viSub cs = do
    let (pat,rep') = break (== '/')  cs
        (rep,opts) = case rep' of
                        []     -> ([],[])
                        (_:ds) -> case break (== '/') ds of
                                    (rep'', [])    -> (rep'', [])
                                    (rep'', (_:fs)) -> (rep'',fs)
    case opts of
        ['g'] -> searchAndRepLocal pat rep  -- TODO
        _     -> searchAndRepLocal pat rep

-- ---------------------------------------------------------------------
-- | Try and write a file in the manner of vi\/vim
--
viWrite :: Action
viWrite = do 
    (f,s,_,_,_,_) <- bufInfoE 
    fwriteE
    msgE $ show f++" "++show s ++ "C written"

--
-- | An invalid command
--
viCmdErr :: [Char] -> Action
viCmdErr s = msgE $ "The "++s++ " command is unknown."

-- | Is a delete sequence
isDel :: Char -> Bool
isDel '\BS'        = True
isDel '\127'       = True
isDel c | c == keyBackspace = True
isDel _            = False

