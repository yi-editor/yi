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

import Yi.Editor
import Yi.Core

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
-- This function must be implemented by any user keybinding:
--
keymap :: Char -> Action
keymap c = do m <- readIORef mode ; key m c 

-- ---------------------------------------------------------------------
-- | Actual lexer
--
key :: Mode -> Char -> Action

-- 
-- * Command mode
--
key C 'h' = leftE
key C 'j' = downE
key C 'k' = upE
key C 'l' = rightE
key C '$' = eolE
key C '0' = solE
key C '|' = solE
key C ':' = msgE ":" >> beginEx 
key C 'i' = beginInsert
key C 'a' = rightOrEolE 1 >> beginInsert
key C 'A' = eolE          >> beginInsert
key C 'O' = solE >> insertE '\n' >> beginInsert
key C 'o' = eolE >> insertE '\n' >> rightE >> beginInsert
key C 'x' = deleteE
key C 'D' = killE
key C 'J' = eolE >> deleteE >> insertE ' '
key C 'd' = do c <- getcE ; when (c == 'd') $ solE >> killE >> deleteE
key C 'Z' = do c <- getcE ; when (c == 'Z') quitE
key C c | c == keyUp    = upE
        | c == keyDown  = downE
        | c == keyLeft  = leftE
        | c == keyRight = rightE
key C '>' = do c <- getcE
               when (c == '>') $ solE >> mapM_ insertE (replicate 4 ' ')
key C '\23' = nextE

-- ---------------------------------------------------------------------
-- * Insert mode
--
key I '\27'  = leftOrSolE 1 >> beginCommand  -- ESC
key I '\8'   = leftE >> deleteE
key I c  | c == keyHome      = topE
         | c == keyBackspace = leftE >> deleteE
key I c      = insertE c    >> rightE

-- ---------------------------------------------------------------------
-- * Ex mode
-- TODO not very scalable..
--
key E c = do 
    msgE (':':[c])       -- TODO general scheme for echoing
    case c of
        'w' -> do 
            c' <- getcE
            case c' of 
                '\13' -> viWrite
                'q'   -> do 
                    msgE (":wq")
                    c'' <- getcE       
                    case c'' of
                        '\13' -> viWrite >> quitE
                        _     -> viCmdErr (c:c':[c''])
                _  -> viCmdErr (c:[c'])
        'q' -> do
            c' <- getcE
            case c' of 
                '\13' -> quitE
                _     -> viCmdErr (c:[c'])
        _ -> viCmdErr [c]
    beginCommand

key _  _  = noopE

-- ---------------------------------------------------------------------
-- | Try and write a file in the manner of vi/vim
--
viWrite :: Action
viWrite = do 
    (f,s) <- infoE 
    writeE
    msgE $ show f++" "++show s ++ "C written"

--
-- | An invalid command
--
viCmdErr :: [Char] -> Action
viCmdErr s = msgE $ "Not an editor command: "++s

