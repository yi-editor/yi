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

module Vi ( keymap ) where

import Yi.Core
import Yi.Editor    ( Keymap(..) )  -- just for now
import Yi.UI        -- hack, just for now, so we can see key defns

import Data.Char
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
cmd :: Char -> IO Keymap

-- Move the cursor left one character.
cmd 'h'   = leftOrSolE 1    >> nextCmd

-- Move the cursor down count lines without changing the current column.
cmd 'j'   = downE           >> nextCmd
cmd '\^J' = downE           >> nextCmd
cmd '\^N' = downE           >> nextCmd

-- Move the cursor up one line
cmd 'k'   = upE             >> nextCmd     

--  Move the cursor right one character.
cmd 'l'   = rightOrEolE 1   >> nextCmd
cmd ' '   = rightOrEolE 1   >> nextCmd

-- Move the cursor to the end of a line.
cmd '$'   = eolE            >> nextCmd

-- Move to the first character in the current line.
cmd '0'   = solE            >> nextCmd

-- Move to the start of the current line.
cmd '|'  = solE             >> nextCmd

-- Delete the character the cursor is on.
cmd 'x' = deleteE           >> nextCmd

-- Join lines.
cmd 'J' = eolE >> deleteE >> insertE ' ' >> nextCmd

-- Enter input mode, inserting the text at the beginning of the line.
cmd 'I' = solE              >> nextIns

-- Enter input mode, inserting the text before the cursor.
cmd 'i' = nextIns

-- Execute an ex command.
cmd ':' = msgClrE >> msgE ":" >> nextEx 


-- Enter input mode, appending the text after the cursor.
cmd 'a' = rightOrEolE 1     >> nextIns


-- Enter input mode, appending the text after the end of the line.
cmd 'A' = eolE            >> nextIns

-- Enter input mode, appending text in a new line above the current line.
cmd 'O' = solE >> insertE '\n' >> nextIns

-- Enter input mode, appending text in a new line under the current line.
cmd 'o' = eolE >> insertE '\n' >> nextIns


cmd c 
-- Page backwards count screens.
    | c == keyPPage = upScreenE     >> nextCmd
    | c == '\^B'    = upScreenE     >> nextCmd

-- Page forward count screens.
    | c == keyNPage = downScreenE   >> nextCmd
    | c == '\^F'    = downScreenE   >> nextCmd

-- The cursor arrow keys should work for movement too
    | c == keyUp    = upE           >> nextCmd
    | c == keyDown  = downE         >> nextCmd
    | c == keyLeft  = leftOrSolE 1  >> nextCmd
    | c == keyRight = rightOrEolE 1 >> nextCmd

-- Delete text from the current position to the end-of-line.
cmd 'D' = killE                   >> nextCmd

-- Delete the line the cursor is on.
cmd 'd' = do c <- getcE
             when (c == 'd') $ solE >> killE >> deleteE
             nextCmd

-- Replace character.
cmd 'r' = getcE >>= writeE >> nextCmd

-- Write the file and exit vi.
cmd 'Z' = do c <- getcE ; when (c == 'Z') $ viWrite >> quitE
             nextCmd

-- Shift lines right.
cmd '>' = do c <- getcE
             when (c == '>') $ solE >> mapM_ insertE (replicate 4 ' ')
             nextCmd

-- Reverse the case of the next character
cmd '~' = do c <- readE
             let c' = if isUpper c then toLower c else toUpper c
             writeE c'
             nextCmd

-- Switch to the next lower screen in the window, or to the first screen if
-- there are no lower screens in the window.
cmd '\^W' = nextWinE >> nextCmd

-- Display the file information.
cmd '\^G' = do
    (f,_,ln,pct) <- bufInfoE 
    msgE $ show f ++ " Line " ++ show ln ++ " ["++ pct ++"]" 
    nextCmd

cmd _ = nopE >> nextCmd

-- ---------------------------------------------------------------------
-- * Insert mode
--
ins :: Char -> IO Keymap
-- Return to command mode.
ins '\ESC'  = leftOrSolE 1 >> nextCmd

-- Erase the last character.
ins '\^H' = deleteE                   >> nextIns
ins c | c == keyBackspace = deleteE   >> nextIns

ins c | c == keyPPage = upScreenE     >> nextIns
      | c == keyNPage = downScreenE   >> nextIns

-- Insert character
ins c  = do 
        (_,s,_,_) <- bufInfoE
        when (s == 0) $ insertE '\n' -- vi behaviour at start of file
        insertE c
        nextIns

ins _ = nopE >> nextIns

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
                 if c == '\BS' || c == keyBackspace
                    then msgClrE >> nextCmd       -- deleted request
                    else loop [c]
    loop w@(c:cs) 
        | c == '\BS'        = deleteWith cs
        | c == keyBackspace = deleteWith cs
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
    execEx ('e':' ':f) = fnewE f
    execEx cs    = viCmdErr cs

    deleteWith []     = msgClrE >> msgE ":"      >> loop []
    deleteWith (_:cs) = msgClrE >> msgE (':':cs) >> loop cs

ex _ = nopE >> nextEx

-- ---------------------------------------------------------------------
-- | Try and write a file in the manner of vi\/vim
--
viWrite :: Action
viWrite = do 
    (f,s,_,_) <- bufInfoE 
    fwriteE
    msgE $ show f++" "++show s ++ "C written"

--
-- | An invalid command
--
viCmdErr :: [Char] -> Action
viCmdErr s = msgE $ "The "++s++ " command is unknown."

