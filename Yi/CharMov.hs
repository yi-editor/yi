{-# OPTIONS -fglasgow-exts -cpp -#include YiUtils.h #-}
--
-- -fglasgow-exts for deriving Typeable
-- 
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
--               2004 Tuomo Valkonen
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

module Yi.CharMov (
    doSkipWhile,    -- :: Action -> IO Bool -> (Char -> Bool) -> Action
    doSkipCond,     -- :: Action -> IO Bool -> (Char -> Bool) -> Action

    (>>||),         -- :: IO Bool -> IO Bool -> IO Bool

    skipWordE,      -- :: Action
    bskipWordE      -- :: Action
) where

import Yi.Core
import Yi.Editor            ( Action )

import Data.Char            ( isSpace )
import Control.Monad        ( when )

doSkipWhile :: Action -> IO Bool -> (Char -> Bool) -> Action
doSkipWhile mov chkend check = do
    e <- chkend
    c <- readE
    when (not e && check c) (mov >> doSkipWhile mov chkend check)

doSkipCond :: Action -> IO Bool -> (Char -> Bool) -> Action
doSkipCond mov chkend check = do
    c <- readE
    if check c
        then mov >> doSkipWhile mov chkend check
        else mov >> doSkipWhile mov chkend (not . check)


-- | Monadic OR operation.
(>>||) :: Monad a => a Bool -> a Bool -> a Bool
a >>|| b = a >>= \ra -> if (not ra) then b else return True

-- | Skip to next whitespace or non-whitespace inversely depending on
-- the character under point.
skipWordE :: Action
skipWordE = doSkipCond rightE (atEolE >>|| atEofE) isSpace

-- | Backwards Skip to next whitespace or non-whitespace inversely 
-- depending on the character under point.
bskipWordE :: Action
bskipWordE = doSkipCond leftE (atSolE >>|| atSofE) isSpace
