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
        lineUp, lineDown, rightB, leftB, readB, deleteB, deleteN, deleteToEol, elemsB, readAtB,
        atSol, atSof, atEol, atEof, 
        Point, Size,
    ) where

import {-# SOURCE #-} Yi.Undo   ( URAction )
import Yi.Regex                 ( Regex  )

import Data.Unique              ( Unique )

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

    -- | Return @n@ elems starting at @i@ of the buffer as a list
    nelemsB    :: a -> Int -> Int -> IO [Char]

    ------------------------------------------------------------------------
    -- Point based operations

    -- | Move point in buffer to the given index
    moveTo     :: a -> Int -> IO ()

    ------------------------------------------------------------------------

    -- | Write an element into the buffer at the current point
    -- This is an unsafe operation, no bounds checks are performed
    writeB     :: a -> Char -> IO ()

    ------------------------------------------------------------------------

    -- | Insert the list at current point, extending size of buffer
    insertN    :: a -> [Char] -> IO ()

    ------------------------------------------------------------------------

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
    moveXorSol a x = moveAXuntil a leftB x atSol

    -- | Move @x@ chars forward, or to the eol, whichever is less
    moveXorEol  :: a -> Int -> IO ()
    moveXorEol a x = moveAXuntil a rightB x atEol

    -- Move using the direction specified by the 2nd argument, until
    -- either we've moved @n@, the 3rd argument, or @p@ the 4th argument
    -- is True
    moveAXuntil :: a -> (a -> IO ()) -> Int -> (a -> IO Bool) -> IO ()

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
    unsetMarkB      :: a -> IO ()


-- | Move point -1
leftB       :: Buffer a => a -> IO ()
leftB a     = leftN a 1

-- | Move cursor -n
leftN       :: Buffer a => a -> Int -> IO ()
leftN a n   = pointB a >>= \p -> moveTo a (p - n)

-- | Move cursor +1
rightB      :: Buffer a => a -> IO ()
rightB a    = rightN a 1

-- | Move cursor +n
rightN      :: Buffer a => a -> Int -> IO ()
rightN a n = pointB a >>= \p -> moveTo a (p + n)

-- ---------------------------------------------------------------------
-- Line based movement and friends

-- | Move point up one line

lineUp :: Buffer a => a -> IO ()
lineUp b = do
    x <- offsetFromSol b
    moveToSol b
    leftB b
    moveToSol b
    moveXorEol b x
{-# INLINE lineUp #-}

-- | Move point down one line
lineDown :: Buffer a => a -> IO ()
lineDown b = do
    x <- offsetFromSol b
    moveToEol b
    rightB b
    moveXorEol b x
{-# INLINE lineDown #-}


-- | Return the contents of the buffer as a list
elemsB ::  Buffer a => a -> IO [Char]
elemsB b = do n <- sizeB b
              nelemsB b n 0

-- | Read the character at the current point
readB :: Buffer a => a -> IO Char
readB b = pointB b >>= readAtB b

-- | Read the character at the given index
-- This is an unsafe operation: character NUL is returned when out of bounds
readAtB :: Buffer a => a -> Int -> IO Char
readAtB b i = do
    s <- nelemsB b 1 i
    return $ case s of
               [c] -> c
               _ -> '\0'

-- | Delete the character at current point, shrinking size of buffer
deleteB :: Buffer a => a -> IO ()
deleteB a = deleteN a 1

-- | Delete @n@ characters forward from the current point
deleteN :: Buffer a => a -> Int -> IO ()
deleteN _ 0 = return ()
deleteN b n = do
  point <- pointB b
  deleteNAt b n point

-- | Delete to the end of line, excluding it.
deleteToEol :: Buffer a => a -> IO ()
deleteToEol b = do
    p <- pointB b
    moveToEol b
    q <- pointB b
    deleteNAt b (q-p) p



------------------------------------------------------------------------

-- | Return true if the current point is the start of a line
atSol ::  Buffer a => a -> IO Bool
atSol a = do p <- pointB a
             if p == 0 then return True
                       else do c <- readAtB a (p-1)
                               return (c == '\n')

-- | Return true if the current point is the end of a line
atEol ::  Buffer a => a -> IO Bool
atEol a = do p <- pointB a
             e <- sizeB a
             if p == e
                    then return True

                    else do c <- readAtB a p
                            return (c == '\n')

-- | True if point at start of file
atSof ::  Buffer a => a -> IO Bool
atSof a = do p <- pointB a
             return (p == 0)

-- | True if point at end of file
atEof :: Buffer a => a -> IO Bool
atEof a = do p <- pointB a
             e <- sizeB a
             return (p == e)


