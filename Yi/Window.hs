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
-- | A 'Window' is a rectangular view onto a buffer. Editor actions
-- manipulate buffers via particular windows onto that buffer. Some
-- editor actions, such as scrolling, only manipulate windows, without
-- touching the underlying buffer.
--

module Yi.Window where

import Yi.Buffer
import Data.Unique              ( Unique, newUnique )

--
-- | A window onto a buffer. 
--
-- Windows need some small information about the terminal. For example,
-- they need to know the height and width.  That's about it.
--
data Window = 
        Window {
            key         :: !Unique      -- ^ each window has a unique 
           ,bufkey      :: !Unique      -- ^ the buffer this window opens to
           ,mode        :: !String      -- ^ this window's modeline
           ,origin      :: !(Int,Int)   -- ^ (y,x) origin of this window
           ,height      :: !Int         -- ^ height of this window
           ,width       :: !Int         -- ^ width of this window
           ,winpnt      :: !Int         -- ^ cached point of the cursor
           ,tospnt      :: !Int         -- ^ the buffer point of the top of screen
           ,cursor      :: !(Int,Int)   -- ^ cursor point on screen
        }

instance Eq Window where
    Window { key = u } == Window { key = v }  = u == v

-- ---------------------------------------------------------------------
-- Construction

--
-- A new window
--
emptyWindow :: Buffer a => a -> (Int,Int) -> IO Window
emptyWindow b (h,w) = do
    m  <- updateModeLine b w 0
    wu <- newUnique
    return $ Window {
                key    = wu
               ,bufkey = (keyB b)
               ,mode   = m
               ,origin = (0,0)  -- TODO what about vnew etc.
               ,height = h-1    -- + 1 for the modeline
               ,width  = w
               ,winpnt = 0      -- cache point when out of focus
               ,tospnt = 0
               ,cursor = (0,0)  -- (y,x)
             }

--
-- | Given a buffer, and some information update the modeline
--
updateModeLine :: Buffer a => a -> Int -> Int -> IO String
updateModeLine b w' p = do
    let f = nameB b
    s    <- sizeB b
    let pct    = getPercent p s
        spaces = replicate (w' - (2 + length f + length pct)) ' '
    return $ "\"" ++ f ++ "\"" ++ spaces ++ pct

------------------------------------------------------------------------
--
-- | Give a point, and the file size, gives us a percent string
--
getPercent :: Int -> Int -> String
getPercent a b = show p ++ "%"
    where p = ceiling ((fromIntegral a) / (fromIntegral b) * 100 :: Double) :: Int

-- ---------------------------------------------------------------------
-- Window actions. These are, often, wrappers around the underlying
-- buffer actions.  Some actions, such as scrolling, are just windows
-- only. When we modify the buffer state, we must be sure to keep the
-- window up to date as well.
--

--
-- | The point moves backwards in the buffer, but the cursor doesn't
-- change positions, unless we reach the top of the file. Once this
-- happens we move the cursor up the file too.
--
moveUpW :: Buffer a => Window -> a -> IO Window
moveUpW w b = do
    lineUp b
    p <- pointB b
    let (cy,_) = cursor w
    x <- offsetFromSol b
    if (p < tospnt w)   -- have to scroll window, cursor-y stays the same
        then return w { winpnt = p, tospnt = p-x, cursor = (cy,x) }
        else return w { winpnt = p, cursor = (max 0 (cy-1),x)  }

--
-- | The cursor moves up, staying with its original line, unless it
-- reaches the top of the screen.
-- 
-- TODO too complicated
--
moveDownW :: Buffer a => Window -> a -> IO Window
moveDownW w@(Window { height=h, tospnt=t }) b = do
    op <- pointB b
    ox <- offsetFromSol b
    lineDown b
    p  <- pointB b
    x  <- offsetFromSol b
    ss <- nelemsB b (p - t) t   -- do something about this calculation
    let lns = length (lines ss)
        (cy,_) = cursor w
    case () of {_
        | lns-1 > h-2         -- at bottom of screen
        -> do moveTo b t      -- TODO fix this. move tospnt down 1 line
              lineDown b 
              q <- pointB b
              moveTo b p 
              return w { winpnt = p, tospnt = q, cursor = (cy,x) }
        | op - ox == p - x               -- didn't change position. eof
        -> return w { winpnt = p, cursor = (cy, x) } 
        | otherwise         -- just move down
        -> return w { winpnt = p, cursor = (min (h-2) (cy+1),x) }
    }

--
-- | shift the window, but don't move the point, unless at start of file
--
scrollUpW :: Buffer a => Window -> a -> IO Window
scrollUpW w b = undefined

--
-- | Move the cursor left or start of line
--
leftW :: Buffer a => Window -> a -> IO Window
leftW w b = moveXorSol b 1 >> updateX w b

--
-- | Move the cursor right or end of line
--
rightW :: Buffer a => Window -> a -> IO Window
rightW w b = moveXorEol b 1 >> updateX w b

--
-- | Move to the start of the line
--
moveToSolW :: Buffer a => Window -> a -> IO Window
moveToSolW w b = moveToSol b >> updateX w b

--
-- | Move to the end of the line
--
moveToEolW :: Buffer a => Window -> a -> IO Window
moveToEolW w b = moveToEol b >> updateX w b

--
-- | Move left or sol
--
moveXorSolW :: Buffer a => Int -> Window -> a -> IO Window
moveXorSolW i w b = moveXorSol b i >> updateX w b

--
-- | Move right or eol
--
moveXorEolW :: Buffer a => Int -> Window -> a -> IO Window
moveXorEolW i w b = moveXorEol b i >> updateX w b

------------------------------------------------------------------------
--
-- | update window point, and cursor in X dimension
--
updateX :: Buffer a => Window -> a -> IO Window
updateX w b = do
    let (y,_) = cursor w
    p <- pointB b
    x <- offsetFromSol b
    return w { winpnt = p, cursor = (y,x) }

