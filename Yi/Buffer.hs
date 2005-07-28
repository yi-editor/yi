-- 
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
-- | An interface to a one dimensional mutable 'Buffer', providing
-- cursor movement and editing commands
--

module Yi.Buffer (
        Buffer(..),
        Point, Size,
    ) where

import {-# SOURCE #-} Yi.Undo   ( URAction )
import Yi.Regex                 ( Regex  )

import Data.Unique              ( Unique )

import Foreign.C.String         ( CStringLen )

--
-- | The 'Buffer' class defines editing operations over one-dimensional`
-- mutable buffers, which maintain a current /point/.
--

type Point = Int
type Size  = Int

class Buffer a where

    -- | Construct a new buffer initialised with the supplied name and list
    newB  :: FilePath -> [Char] -> IO a

    -- | Free any resources associated with this buffer
    finaliseB :: a -> IO ()

    -- | Construct a new buffer filled with contents of file
    hNewB :: FilePath -> IO a

    -- | Write buffer into file
    hPutB :: a -> FilePath -> IO ()

    -- | String name of this buffer
    nameB :: a -> String

    -- | Name of file associated with this buffer
    getfileB :: a -> IO (Maybe FilePath)

    -- | Set the name of the file associated with this buffer
    setfileB :: a -> FilePath -> IO ()

    -- | Unique key of this buffer
    keyB :: a -> Unique

    ------------------------------------------------------------------------

    -- | Has the buffer been modified. @True@ if unchanged
    isUnchangedB :: a -> IO Bool

    -- | Number of characters in the buffer
    sizeB      :: a -> IO Int

    -- | Extract the current point
    pointB     :: a -> IO Int

    -- | Return the contents of the buffer as a list
    elemsB     :: a -> IO [Char]

    -- | Return @n@ elems starting at @i@ of the buffer as a list
    nelemsB    :: a -> Int -> Int -> IO [Char]

    -- | Return a list of pointers to @n@ C strings, starting at point.
    ptrToLnsB  :: a -> Int -> Int -> IO [CStringLen]

    ------------------------------------------------------------------------
    -- Point based operations

    -- | Move point in buffer to the given index
    moveTo     :: a -> Int -> IO ()

    -- | Move point -1
    leftB       :: a -> IO ()
    leftB a     = leftN a 1

    -- | Move cursor -n
    leftN       :: a -> Int -> IO ()
    leftN a n   = pointB a >>= \p -> moveTo a (p - n)

    -- | Move cursor +1
    rightB      :: a -> IO ()
    rightB a    = rightN a 1

    -- | Move cursor +n
    rightN      :: a -> Int -> IO ()
    rightN a n = pointB a >>= \p -> moveTo a (p + n)

    ------------------------------------------------------------------------

    -- | Read the character at the current point
    readB      :: a -> IO Char

    -- | Read the character at the given index
    readAtB    :: a -> Int -> IO Char

    -- | Write an element into the buffer at the current point
    writeB     :: a -> Char -> IO ()

    ------------------------------------------------------------------------

    -- | Insert the character at current point, extending size of buffer
    insertB    :: a -> Char -> IO ()

    -- | Insert the list at current point, extending size of buffer
    insertN    :: a -> [Char] -> IO ()

    ------------------------------------------------------------------------

    -- | Delete the character at current point, shrinking size of buffer
    deleteB    :: a -> IO ()

    -- | Delete @n@ characters forward from the current point
    deleteN    :: a -> Int -> IO ()

    -- | Delete @n@ characters backwards from the current point
    -- deleteNback :: a -> Int -> IO ()

    -- | Delete characters forwards to index
    -- deleteTo    :: a -> Int -> IO ()
    
    -- | @deleteNAt b n p@ deletes @n@ characters at position @p@
    deleteNAt :: a -> Int -> Int -> IO ()

    ------------------------------------------------------------------------
    -- undo/redo

    -- | Undo the last action that mutated the buffer's contents
    undo        :: a -> IO ()

    -- | Redo the last action we that was undone.
    redo        :: a -> IO ()

    -- | Required implementation of how to invert an action, for undo
    -- generates circular dependency between Undo.hs and Buffer.hs
    getActionB  :: Buffer a => URAction -> (a -> IO URAction) 

    ------------------------------------------------------------------------
    -- Line based editing

    -- | Return true if the current point is the start of a line
    atSol       :: a -> IO Bool

    -- | Return true if the current point is the end of a line
    atEol       :: a -> IO Bool

    -- | True if point at start of file
    atSof       :: a -> IO Bool

    -- | True if point at end of file
    atEof       :: a -> IO Bool
    
    -- | Move point to start of line
    moveToSol   :: a -> IO ()

    -- | Offset from start of line
    offsetFromSol :: a -> IO Int

    -- | Index of start of line 
    indexOfSol    :: a -> IO Int

    -- | Index of end of line 
    indexOfEol    :: a -> IO Int

    -- | Move point to end of line
    moveToEol   :: a -> IO ()

    -- | Move @x@ chars back, or to the sol, whichever is less
    moveXorSol  :: a -> Int -> IO ()

    -- | Move @x@ chars forward, or to the eol, whichever is less
    moveXorEol  :: a -> Int -> IO ()

    -- | Delete (back) to start of line
    -- deleteToSol :: a -> IO ()

    -- | Delete to end of line
    deleteToEol :: a -> IO ()

    -- | Delete the entire line the point is in
    -- deleteLn    :: a -> IO ()

    -- | Move point up one line
    lineUp      :: a -> IO ()

    -- | Move point down one line
    lineDown    :: a -> IO ()

    -- | Return the current line number
    curLn       :: a -> IO Int

    -- | Go to line number @n@. @n@ is indexed from 1. Returns the
    -- actual line we went to (which may be not be the requested line,
    -- if it was out of range)
    gotoLn      :: a -> Int -> IO Int

    --
    -- | Go to line indexed from current point
    --
    gotoLnFrom  :: a -> Int -> IO Int

    ---------------------------------------------------------------------
   
    -- | Return index of next string in buffer that matches argument
    searchB      :: a -> [Char] -> IO (Maybe Int)

    -- | Return indices of next string in buffer matched by regex
    regexB       :: a -> Regex -> IO (Maybe (Int,Int))

    ---------------------------------------------------------------------

    -- | Set this buffer mark (TODO: have a set of these (bookmarks, error list, etc.))
    setMarkB        :: a -> Int -> IO ()
    getMarkB        :: a -> IO Int