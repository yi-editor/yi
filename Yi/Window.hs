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

import Data.Char                ( isLatin1 )
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
       ,cursor      :: !(Int,Int)   -- ^ cursor point on screen

       ,pnt         :: !Int         -- ^ current point
       ,lineno      :: !Int         -- ^ current line number

       ,tospnt      :: !Int         -- ^ the buffer point of the top of screen
       ,toslineno   :: !Int         -- ^ line number of top of screen
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
               ,height = h-1    -- - 1 for the modeline
               ,width  = w
               ,cursor = (0,0)  -- (y,x)
               ,pnt       = 0      -- cache point when out of focus
               ,lineno    = 1
               ,tospnt    = 0
               ,toslineno = 1      -- start on line 1
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
moveUpW w@(Window {lineno=ln}) b
    | ln == 1          = return w  -- at top of file
    | otherwise = do           
        lineUp b
        x <- offsetFromSol b
        p <- pointB b
        let cy = fst $ cursor w
        if toslineno w < ln
            then flip update b w {    -- just move cursor up
                        lineno = ln - 1, 
                        cursor = (cy-1,x) 
                    }
            else flip update b w {    -- scroll screen up
                        toslineno = toslineno w -1, 
                        lineno    = ln - 1,
                        tospnt    = p - x, 
                        cursor    = (cy,x) 
                    }

--
-- | The cursor moves up, staying with its original line, unless it
-- reaches the top of the screen.
--
moveDownW :: Buffer a => Window -> a -> IO Window
moveDownW w@(Window { height=h, lineno=ln } ) b = do
    ll <- atLastLine b 
    if ll then return w      -- eof, go no further
          else do
    let cy = fst $ cursor w
    lineDown b
    x <- offsetFromSol b
    if ln - toslineno w < h-2
        then flip update b w {    -- just scroll cursor
                    lineno = ln + 1,
                    cursor = (cy+1,x)
                }
        else do 
            t' <- indexOfNLFrom b (tospnt w)
            flip update b w { -- at bottom line of screen, so scroll
                    toslineno = toslineno w + 1,
                    lineno = ln + 1,
                    tospnt = t',
                    cursor = (cy,x)     -- doesn't move
                }

------------------------------------------------------------------------
--
-- | Move the cursor left or start of line
--
leftW :: Buffer a => Window -> a -> IO Window
leftW w b = moveXorSol b 1 >> update w b

--
-- | Move the cursor right or end of line
--
rightW :: Buffer a => Window -> a -> IO Window
rightW w b = moveXorEol b 1 >> update w b

--
-- | Move to the start of the line
--
moveToSolW :: Buffer a => Window -> a -> IO Window
moveToSolW w b = moveToSol b >> update w b

--
-- | Move to the end of the line
--
moveToEolW :: Buffer a => Window -> a -> IO Window
moveToEolW w b = moveToEol b >> update w b

--
-- | Move left or sol
--
moveXorSolW :: Buffer a => Int -> Window -> a -> IO Window
moveXorSolW i w b = moveXorSol b i >> update w b

--
-- | Move right or eol
--
moveXorEolW :: Buffer a => Int -> Window -> a -> IO Window
moveXorEolW i w b = moveXorEol b i >> update w b

------------------------------------------------------------------------
--
-- | Insert a character. Don't move.
--
insertW :: Buffer a => Char -> Window -> a -> IO Window
insertW c w b = do
    w' <- do case c of
                '\13'          -> insertB b '\n'
                _ | isLatin1 c -> insertB b c
                  | otherwise  -> return ()
             return w
    update w' b

--
-- | Delete character.
-- Handle eof properly
-- 
deleteW :: Buffer a => Window -> a -> IO Window
deleteW w b = deleteB b >> update w b

------------------------------------------------------------------------
--
-- | update window point, and cursor in X dimension. and reset pnt cache
--
update :: Buffer a => Window -> a -> IO Window
update w b = do
    let (y,_) = cursor w
    x <- offsetFromSol b
    p <- pointB b
    return w { pnt = p, cursor = (y,x) }

--
-- | On the last line of the file
--
atLastLine :: Buffer a => a -> IO Bool
atLastLine b = do
    p <- pointB b
    moveToEol b
    pointB b
    e <- atEof b
    moveTo b p
    return e

--
-- | Given a point, return the point of the next line down
--
indexOfNLFrom:: Buffer a => a -> Int -> IO Int
indexOfNLFrom b i = do
    p <- pointB b
    moveTo b i
    lineDown b
    q <- pointB b
    moveTo b p
    return q

