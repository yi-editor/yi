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
                key       = wu
               ,bufkey    = (keyB b)
               ,mode      = m
               ,origin    = (0,0)  -- TODO what about vnew etc.
               ,height    = h-1    -- - 1 for the modeline
               ,width     = w
               ,cursor    = (0,0)  -- (y,x)
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
moveUpW w b | lineno w == 1 = return w
            | otherwise     = lineUp b >> decY w b >>= flip update b

--
-- | The cursor moves up, staying with its original line, unless it
-- reaches the top of the screen.
--
moveDownW :: Buffer a => Window -> a -> IO Window
moveDownW w b = do
    ll <- atLastLine b 
    if ll then return w      -- eof, go no further
          else lineDown b >> incY w b >>= flip update b

------------------------------------------------------------------------
--
-- | Move the cursor left or start of line
--
leftOrSolW :: Buffer a => Window -> a -> IO Window
leftOrSolW w b = moveXorSol b 1 >> update w b

--
-- | Move the cursor right or end of line
--
rightOrSolW :: Buffer a => Window -> a -> IO Window
rightOrSolW w b = moveXorEol b 1 >> update w b

{-
--
-- | Move the cursor left, or to the end of the previous line 
--
leftW :: Buffer a => Window -> a -> IO Window
leftW w b  = do
    leftB b
    if atEol b      -- we moved up one line
        then
        else

--
-- | Move the cursor right, or to the start of the next line 
--
rightW :: Buffer a => Window -> a -> IO Window
rightW w b = rightB b >>
-}


-- ---------------------------------------------------------------------
-- X-axis movement

-- | Move to the start of the line
moveToSolW :: Buffer a => Window -> a -> IO Window
moveToSolW w b = moveToSol b >> update w b

-- | Move to the end of the line
moveToEolW :: Buffer a => Window -> a -> IO Window
moveToEolW w b = moveToEol b >> update w b

-- | Move left @n@ or start of line
moveXorSolW :: Buffer a => Int -> Window -> a -> IO Window
moveXorSolW i w b = moveXorSol b i >> update w b

-- | Move right @n@ or end of line
moveXorEolW :: Buffer a => Int -> Window -> a -> IO Window
moveXorEolW i w b = moveXorEol b i >> update w b

-- ---------------------------------------------------------------------
-- Editing operations

--
-- | Insert a single character, shift point right to eol
-- If we insert a \n, then move to the start of the new line
--
insertW :: Buffer a => Char -> Window -> a -> IO Window
insertW c w b = do
    case c of                               -- insertB doesn't change the point
        '\13'          -> insertB b '\n'
        _ | isLatin1 c -> insertB b c
          | otherwise  -> return ()
    w' <- moveXorEolW 1 w b                       -- and shift the point
    if c == '\13'       -- newline, so move down with new line
        then moveDownW w' b >>= flip moveToSolW b
        else return w'

--
-- | Delete character. Don't move point unless at EOF
--
-- In vi, you can't delete past the sol, here however, you can keep
-- deleteing till you have an empty file. 
-- 
deleteW :: Buffer a => Window -> a -> IO Window
deleteW w b = do 
    eof <- atEof b
    sol <- atSol b      -- about to delete \n
    let tos = lineno w - 1 == toslineno w
    deleteB b
    case () of {_
        | eof && sol && tos -> moveToEolW w b >>= flip decY b {- scroll screen -}
        | eof && sol        -> moveToEolW w b >>= flip decY b  -- todo should handle 0
        | otherwise         -> update w b
    }

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
-- | Decrememt the y-related values. Might change top of screen point
-- TODO deal with sof
--
decY :: Buffer a => Window -> a -> IO Window
decY w b = do
    p <- pointB b           -- current point
    x <- offsetFromSol b    -- x offset
    let (y,_) = cursor w 
        curln = lineno w
        topln = toslineno w
        w' = w { lineno = curln-1 }
    return $ if topln < curln
             then w' { cursor = (y-1, x) }                  -- just move cursor
             else w' { toslineno = topln-1, tospnt = p-x }  -- or move window

--
-- | incrememt the y-related values.
-- TODO deal with eof
--
incY :: Buffer a => Window -> a -> IO Window
incY w@(Window {height=h}) b = do
   let (y,x) = cursor w 
       curln = lineno w
       topln = toslineno w
       w' = w { lineno = curln+1 }
   t <- indexOfNLFrom b (tospnt w)
   return $ if curln - topln < h - 2   
            then w' { cursor = (y+1,x) }                  -- just move cursor
            else w' { toslineno = topln + 1, tospnt = t } -- scroll window

------------------------------------------------------------------------

--
-- | On the last line of the file
-- TODO faster please
--
atLastLine :: Buffer a => a -> IO Bool
atLastLine b = do
    p <- pointB b
    moveToEol b
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

