--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
import Yi.FastBuffer            ( FBuffer ) -- for specialisation

import Data.Char                ( isLatin1 )
import Data.Unique              ( Unique, newUnique )
import Control.Monad            ( when )

--
-- | A window onto a buffer.
--
-- Windows need some small information about the terminal. For example,
-- they need to know the height and width.  That's about it.
--
-- This doesn't handle line wrapping, yet.
--
data Window =
    Window {
        key         :: !Unique         -- ^ each window has a unique
       ,bufkey      :: !Unique         -- ^ the buffer this window opens to
       ,mode        :: !Bool           -- ^ this window has modeline?
       ,height      :: !Int            -- ^ height of this window
       ,width       :: !Int            -- ^ width of this window

       ,cursor      :: !(Int,Int)      -- ^ cursor point on screen (y,x)

       ,pnt         :: !Int            -- ^ current point

       ,tospnt      :: !Int            -- ^ the buffer point of the top of screen
       ,toslineno   :: !Int            -- ^ line number of top of screen
    }


instance Eq Window where
    Window { key = u } == Window { key = v }  = u == v

instance Ord Window where
    Window { key = u } `compare` Window { key = v }  = u `compare` v
    Window { key = u } <     Window { key = v }      = u <     v

-- ---------------------------------------------------------------------
-- Construction

--
-- A new window
--
-- The origin, height and width should be calculated with reference to
-- all existing windows.
--
emptyWindow :: Buffer a => a -> (Int,Int) -> IO Window
emptyWindow b (h,w) = do
    wu <- newUnique
    let win = Window {
                    key       = wu
                   ,mode      = False
                   ,bufkey    = (keyB b)
                   ,height    = h-1    -- - 1 for the cmdline?
                   ,width     = w
                   ,cursor    = (0,0)  -- (y,x) (screen columns, needs to know about tabs)
                   ,pnt       = 0      -- cache point when out of focus
                   ,tospnt    = 0
                   ,toslineno = 1      -- start on line 1
              }
    return win

------------------------------------------------------------------------
--
-- | Given a buffer, and some information update the modeline
-- There's some useful code in textinfo.window.c. Worth a read.
--
-- N.B. the contents of modelines should be specified by keymaps, and
-- not hardcoded.
--
updateModeLine :: Buffer a => Window -> a -> IO (Maybe String)
updateModeLine w' b = do
    if not (mode w') then return Nothing else do
    ln <- curLn b
    let f    = nameB b
        lns  = show ln
        top  = toslineno w'
        cols = show $ 1 + snd (cursor w')

    p <- indexOfEol b
    s <- sizeB b
    let pct = if top == 1 then "Top" else getPercent p s

    case flip replicate ' ' (16 - length cols - length pct) of { spc' ->
    case flip replicate ' ' (width w' - (3 + sum
                (map length [f,lns,cols,pct,spc']))) of { spaces ->
    return $ Just $ "\"" ++ f ++ "\"" ++ spaces ++
                    lns ++ "," ++ cols ++ spc' ++ pct
    }}

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
-- | The point moves backwards in the buffer, but the screen doesn't
-- scroll, until we reach the top of the screen.
--
moveUpW :: Buffer a => Window -> a -> IO ()
moveUpW w b = lineUp b

--
-- | The cursor moves up, staying with its original line, unless it
-- reaches the top of the screen.
--
moveDownW :: Buffer a => Window -> a -> IO ()
moveDownW w b = do
    ll <- atLastLine b
    when (not ll) $      -- eof, go no further
      lineDown b

-- ---------------------------------------------------------------------
-- | Wacky moveToW function

moveToW :: Buffer a => Int -> Window -> a -> IO ()
moveToW np w b = do
    moveTo b np

-- | goto an arbitrary line in the file. center that line on the screen
-- gotoLn is (fast as possible) an O(n) op atm.
--
gotoLnW :: Buffer a => Int -> Window -> a -> IO Int
gotoLnW n w b = do
    gotoLn b n
    
--
-- | Goto a line offset from the current line
--
gotoLnFromW :: Buffer a => Int -> Window -> a -> IO Int
gotoLnFromW n w b = do
    gotoLnFrom b n

--
-- | Move the cursor left or start of line
--
leftOrSolW :: Buffer a => Window -> a -> IO ()
leftOrSolW w b = moveXorSol b 1    

--
-- | Move the cursor right or end of line
--
rightOrSolW :: Buffer a => Window -> a -> IO ()
rightOrSolW w b = moveXorEol b 1   

-- | Move to the start of the line
moveToSolW :: Buffer a => Window -> a -> IO ()
moveToSolW w b = moveToSol b

-- | Move to the end of the line
moveToEolW :: Buffer a => Window -> a -> IO ()
moveToEolW w b = moveToEol b

-- | Move left @n@ or start of line
moveXorSolW :: Buffer a => Int -> Window -> a -> IO ()
moveXorSolW i w b = moveXorSol b i

-- | Move right @n@ or end of line
moveXorEolW :: Buffer a => Int -> Window -> a -> IO ()
moveXorEolW i w b = moveXorEol b i

-- ---------------------------------------------------------------------
-- Editing operations

-- | Insert a single character
insertW :: Buffer a => Char -> Window -> a -> IO ()
insertW c = insertNW [c]

-- | Insert a whole String at the point
insertNW :: Buffer a => String -> Window -> a -> IO ()
insertNW cs w b = do
    let cs' = [if c == '\13' then '\n' else c | c <- cs, isLatin1 c]
    insertN b cs'
    

--
-- | Delete character. Don't move point unless at EOF
--
-- In vi, you can't delete past the sol, here however, you can keep
-- deleteing till you have an empty file.
--
-- TODO think about end of file situation.
--
deleteNW :: Buffer a => Window -> a -> Int -> IO ()
deleteNW w b i = do

    -- delete up to eof chars
    when (i > 1) $ do
        rightB b
        deleteN b (i-1)
        eof <- atEof b
        when (not eof) $ leftB b

    sof <- atSof b
    eof <- atEof b  -- are we going to del eof
    sol <- atSol b  -- and we're not on the same line?

    deleteB b

    when (eof && sol && not sof) $
        moveToEolW w b

deleteNAtW :: Buffer a => Window -> a -> Int -> Int -> IO ()
deleteNAtW w b i p = deleteNAt b i p

--
-- | Kill all the characters to the end of the line
-- If there is no \n at the end of the line, scroll up 1
--
deleteToEolW :: Buffer a => Window -> a -> IO ()
deleteToEolW w b = do
    deleteToEol b

------------------------------------------------------------------------

--
-- return True if we're on the last line, and there's no \n at the end
--
noNLatEof :: Buffer a => a -> IO Bool
noNLatEof b = do
    p <- pointB b
    moveToEol b
    c  <- readB b
    moveTo b p
    return (c /= '\n')

--
-- | return index of Sol on line @n@ above current line
--
indexOfSolAbove :: Buffer a => a -> Int -> IO Int
indexOfSolAbove b n = do
    p <- pointB b
    moveToSol b
    loop n b
    q <- pointB b
    moveTo b p
    return q

    where loop 0 _  = return ()
          loop i b' = lineUp b' >> loop (i-1) b'

{-# SPECIALIZE indexOfSolAbove :: FBuffer -> Int -> IO Int #-}

--
-- | Adjust the window's size-related fields, assuming a new window
-- height, and width. Now, if we know the height of the screen, and the
-- number of lines, center the line in the screen please.
--
resize :: Buffer a => Int -> Int -> Window -> a -> IO Window
resize y x w b = return $ w { height = y, width = x }

