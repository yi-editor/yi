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

module Yi.Keymap ( keymap ) where

import Yi.Core
import Yi.UI        -- hack, just for now, so we can see key defns

import Data.Char
import Control.Monad

-- vi is a modeful editor, so we store some state. TODO design?
--
import Data.IORef
import System.IO.Unsafe     ( unsafePerformIO )

-- ---------------------------------------------------------------------
-- | Lets remember what editor mode we are in
--
data Mode = C     -- ^ command mode
          | I     -- ^ insert mode
          | E     -- ^ ex mode

-- | By default, vi starts in command mode
--
mode :: IORef Mode
mode = unsafePerformIO $ newIORef C

beginInsert, beginCommand, beginEx :: IO ()

beginInsert  = writeIORef mode I
beginCommand = writeIORef mode C
beginEx      = writeIORef mode E

-- ---------------------------------------------------------------------
--
-- This function must be implemented by any user keybinding
--
keymap :: Char -> Action
keymap c = readIORef mode >>= flip key c 

-- ---------------------------------------------------------------------
-- | Actual lexer
--
key :: Mode -> Char -> Action

-- 
-- * Command mode
--

-- Move the cursor left one character.
key C 'h'  = leftOrSolE 1

-- Move the cursor down count lines without changing the current column.
key C 'j'    = downE
key C '\^J'  = downE
key C '\^N'  = downE

-- Move the cursor up one line
key C 'k'  = upE

--  Move the cursor right one character.
key C 'l'  = rightOrEolE 1
key C ' '  = rightOrEolE 1

-- Move the cursor to the end of a line.
key C '$'  = eolE

-- Move to the first character in the current line.
key C '0'  = solE

-- Move to the start of the current line.
key C '|'  = solE

-- Enter input mode, inserting the text at the beginning of the line.
key C 'I' = solE >> beginInsert

-- Enter input mode, inserting the text before the cursor.
key C 'i' = beginInsert

-- Execute an ex command.
key C ':' = msgClrE >> msgE ":" >> beginEx 

-- Delete the character the cursor is on.
key C 'x' = deleteE

-- Enter input mode, appending the text after the cursor.
key C 'a' = rightOrEolE 1 >> beginInsert

-- Enter input mode, appending the text after the end of the line.
key C 'A' = eolE >> beginInsert

-- Enter input mode, appending text in a new line above the current line.
key C 'O' = solE >> insertE '\n' >> beginInsert

-- Enter input mode, appending text in a new line under the current line.
key C 'o' = eolE >> insertE '\n' >> beginInsert

-- Join lines.
key C 'J' = eolE >> deleteE >> insertE ' '

key C c 
-- Page backwards count screens.
    | c == keyPPage = upScreenE
    | c == '\^B'    = upScreenE

-- Page forward count screens.
    | c == keyNPage = downScreenE
    | c == '\^F'    = downScreenE

-- The cursor arrow keys should work for movement too
    | c == keyUp    = upE
    | c == keyDown  = downE
    | c == keyLeft  = leftOrSolE 1
    | c == keyRight = rightOrEolE 1

-- Delete text from the current position to the end-of-line.
key C 'D' = killE

-- Delete the line the cursor is on.
key C 'd' = do c <- getcE ; when (c == 'd') $ solE >> killE >> deleteE

-- Replace character.
key C 'r' = getcE >>= writeE

-- Write the file and exit vi.
key C 'Z' = do c <- getcE ; when (c == 'Z') $ viWrite >> quitE

-- Shift lines right.
key C '>' = do c <- getcE
               when (c == '>') $ solE >> mapM_ insertE (replicate 4 ' ')

-- Reverse the case of the next character
key C '~' = do c <- readE
               let c' = if isUpper c then toLower c else toUpper c
               writeE c'

-- Switch to the next lower screen in the window, or to the first screen if
-- there are no lower screens in the window.
key C '\^W' = nextWinE

-- Display the file information.
key C '\^G' = do
    (f,_,ln,pct) <- bufInfoE 
    msgE $ show f ++ " Line " ++ show ln ++ " ["++ pct ++"]" 

-- ---------------------------------------------------------------------
-- * Insert mode
--

-- Return to command mode.
key I '\ESC'  = leftOrSolE 1 >> beginCommand  -- ESC

-- Erase the last character.
key I '\^H'   = deleteE
key I c | c == keyBackspace = deleteE

key I c | c == keyPPage = upScreenE
        | c == keyNPage = downScreenE

-- Insert character
key I c  = do 
        (_,s,_,_) <- bufInfoE
        when (s == 0) $ insertE '\n' -- vi behaviour at start of file
        insertE c

-- ---------------------------------------------------------------------
-- * Ex mode
-- accumulate keys until esc or \n, then try to work out what was typed
--
-- TODO think about how to do this as a better lexer
--
key E k = msgClrE >> loop [k]
  where
    loop [] = do msgE ":"
                 c <- getcE
                 if c == '\BS' || c == keyBackspace
                    then msgClrE >> beginCommand  -- deleted request
                    else loop [c]
    loop w@(c:cs) 
        | c == '\BS'  = deleteWith cs
        | c == keyBackspace = deleteWith cs
        | c == '\ESC' = msgClrE >> beginCommand  -- cancel 
        | c == '\r'   = execEx (reverse cs) >> beginCommand
        | otherwise   = do msgE (':':reverse w)
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

-- anything we've missed
key _  _  = nopE

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
