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
-- | This module defines operations on a two-dimension buffer. Instances of
--  'Buffer' are available.
--

module Yi.Buffer where

--
-- | The 'Buffer' defines operations over two-dimensional rectangular
-- buffers, providing basic editing operations.
--
class Buffer a where

    -- | A buffer has some meta data
    name        :: a -> String

    setname     :: a -> String -> a

    -- | Get the buffer's contents as a '[String]'
    contents    :: a -> [String]

    -- | Fill up the buffer
    setcontents :: a -> [String] -> a

    -- | New empty buffer
    new         :: a

    ------------------------------------------------------------------------

    -- | Pair of width and height (x,y)
    size        :: a -> (Int,Int)

    -- | Set the size
    setsize     :: a -> (Int,Int) -> a

    -- | Width of the buffer
    width       :: a -> Int

    width b = fst (size b)

    -- | Height of the buffer
    height      :: a -> Int

    height b = snd (size b)

    ------------------------------------------------------------------------

    -- | Character under the cursor
    char        :: a -> Char

    -- | Current position of the cursor
    point       :: a -> (Int,Int)

    ------------------------------------------------------------------------

    -- | Move cursor to arbitrary point. If the point overruns in any
    -- dimension, the cursor is moved to the limit in that dimension
    moveto      :: a -> (Int,Int) -> a

    -- | Move cursor -n in @x@
    leftn       :: a -> Int -> a

    leftn b n = case point b of (x,y) -> b `moveto` (x-n, y)

    -- | Move cursor -1 in @x@
    left        :: a -> a
    
    left b = leftn b 1

    -- | Move cursor +n in @x@
    rightn      :: a -> Int -> a

    rightn b n = case point b of (x,y) -> b `moveto` (x+n,  y)

    -- | Move cursor +1 in @x@
    right       :: a -> a
    
    right b = rightn b 1

    -- | Move cursor +n in @y@
    upn         :: a -> Int -> a

    upn b n   = case point b of (x,y) -> b `moveto` (x, y-n)

    -- | Move cursor +1 in @y@
    up          :: a -> a

    up b = upn b 1

    -- | Move cursor -n in @y@
    downn       :: a -> Int -> a

    downn b n = case point b of (x,y) -> b `moveto` (x,y+n)

    -- | Move cursor -1 in @y@
    down        :: a -> a

    down b = downn b 1

    ------------------------------------------------------------------------

    {-
    b_delete_to         :: BPoint -> BufTrans a
    b_delete_n          :: BCount -> BufTrans a
    b_delete_backward,
    b_delete_forward    :: BufTrans a

    b_insert            :: String -> BufTrans a
    b_insertk           :: String -> BufTrans a
    -}

------------------------------------------------------------------------

--
-- | A really simple buffer implementation
--
data BasicBuffer = BasicBuffer {
        b_name      :: !String      -- ^ buffer name 
       ,b_contents  :: [String]     -- ^ contents
       ,b_size      :: !(Int,Int)  -- ^ buffer width and height
       ,b_cursor    :: !(Int,Int)  -- ^ cursor position
--     ,b_modified  :: !Bool        -- ^ have we modified this buffer?
    }

instance Buffer BasicBuffer where
    name        = b_name  
    setname b s = b { b_name = s }

    contents    = b_contents
    setcontents b ss = b { b_contents = ss }

    size        = b_size
    setsize b s = b { b_size = s }

    point       = b_cursor
            
    --
    -- save move to. always end up somewhere with a valid Char
    -- here's where [[Char]] sucks. TODO simplify
    --
    moveto b (x,y) = 
        let ss    = b_contents b
            h     = height b
            len   = length ss

            -- find safe y index
            y' | y >= h    = h - 1     -- end of buffer
               | y >= len  = len - 1   -- end of contents
               | y <  0    = 0         -- start
               | otherwise = y

            -- find line to index
            l  | length ss <= y' = last ss  -- goto to last line
               | otherwise       = ss !! y'

            -- find point in line to jump to
            w  | length l >= 2 = length l - 1 -- ignore '\n'
               | otherwise     = 0
            
            -- now work out safe x index
            x' | x >= w    = w
               | x <  0    = 0
               | otherwise = x

        in b { b_cursor = (x', y') }
                        
    -- always safe
    char b         = let ss    = b_contents b
                         (x,y) = b_cursor b
                     in (ss !! y) !! x

    new = BasicBuffer {
            b_name     = "undefined"
           ,b_contents = [[]] 
           ,b_size     = (0,0)
           ,b_cursor   = (0,0)
          }
