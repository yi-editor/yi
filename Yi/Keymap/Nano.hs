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
-- TODO specify location of title bar, and contents. What the empty
-- screen is filled with. More info from Core.hs about buffer and window
-- status.
--

module Yi.Keymap.Nano ( keymap ) where

import Yi.Core
import Yi.Editor        ( Keymap(..) )  -- just for now
import Yi.UI                            -- hack, just for now
import Control.Monad    ( when )

--
-- A Nano key binding for yi
--
keymap :: Char -> IO Keymap
keymap c = nano c >> return (Keymap keymap)  -- modeless, always the same keymap

--
-- Execute a single command
--
nano :: Char -> Action
nano '\^G'  = msgE "nano-yi : yi emulating nano emulating nano" -- todo
nano '\^X'  = quitE
nano '\^O'  = fwriteE
nano '\^J'  = msgE "^J == justify : undefined" -- TODO
nano '\^R'  = nanoReadF    -- not really right
nano '\^Y'  = upScreenE
nano '\^V'  = downScreenE
nano '\^A'  = solE
nano '\^E'  = eolE
nano '\^D'  = deleteE
nano '\^K'  = do s <- readLnE
		 setRegE (s ++ "\n") >> solE >> killE >> deleteE
nano '\^U'  = do s <- getRegE
		 solE >> mapM_ insertE s
nano '\188' = prevBufW     -- 'M-<' ?
nano '\190' = nextBufW     -- 'M->' ?
nano c
    | c == keyUp || c == '\^P'    = upE
    | c == keyDown || c == '\^N'  = downE
    | c == keyLeft || c == '\^B'  = leftOrSolE 1  -- todo unconditional left
    | c == keyRight || c == '\^F' = rightOrEolE 1 -- todo unconditional right
    | c == '\^H' || isDel c = leftOrSolE 1 >> deleteE

-- TODO why doesn't ^C work?
nano '\^I'  = do
    (_,s,ln,x,p,pct) <- bufInfoE 
    msgE $ "[ line "++show ln++", col "++show x++
           ", char "++show p++"/"++show s++" ("++pct++") ]"

nano c  = do        -- just insert
        (_,s,_,_,_,_) <- bufInfoE
        when (s == 0) $ insertE '\n' -- sof behaviour
        insertE c

--
-- Accumulate characters from the user till \n or \r
--
nanoReadF :: Action
nanoReadF = do
    msg []
    let loop w = do
            k <- getcE
            case () of {_
                | k == '\n'         -> return (reverse w)
                | k == '\r'         -> return (reverse w)
                | otherwise         -> msg (reverse (k:w)) >> loop (k:w)
            }
    f <- loop []
    fnewE f

    where msg s  = msgE $ "File to insert [from ./] : " ++ s
          del []     = msg [] >> return []
          del (_:cs) = msg cs >> return cs
    
isDel :: Char -> Bool
isDel '\BS'        = True
isDel '\127'       = True
isDel c | c == keyBackspace = True
isDel _            = False
