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
-- Derived from: riot/UI.hs
--
--      Copyright (c) Tuomo Valkonen 2004.
--
-- Released under the same license.
--

--
-- | This module defines a user interface implemented using ncurses. 
--
-- TODO The user interface abstractions should try to be general enough to
-- permit multiple user interfaces without changin UI.foo calls in
-- Core.hs
--
module Yi.UI (

        -- * UI initialisation 
        start, end, 
        screenSize,

        -- * Input
        getKey,

        -- * Drawing
        refresh,
        reset,
        fillLine,
        drawCmdLine,
        clearCmd,

        -- * Messages
        warn,
        
        module Yi.Curses   -- UIs need to export the symbolic key names

  )   where

-- TODO the above api should be redesigned. Consider the vi screen api
-- to ncurses for a nice well thought out editor api.

import Yi.Buffer
import Yi.Editor
import Yi.Window

import Yi.Curses hiding ( refresh, Window )
import qualified Yi.Curses as Curses

import Data.List
import qualified Control.Exception      ( catch )

--
-- | how to initialise the ui
--
start :: IO ()
start = do
    Curses.initCurses                   -- initialise the screen
    Curses.keypad Curses.stdScr True    -- grab the keyboard

--
-- | Clean up and go home
--
end :: IO ()
end = Curses.endWin

------------------------------------------------------------------------
-- | Find the current screen height and width.
--
screenSize :: IO (Int, Int)
screenSize = Curses.scrSize

-- ---------------------------------------------------------------------
-- | Read a key. UIs need to define a method for getting events.
--
getKey :: IO () -> IO Char
getKey refresh_fn = do
    Control.Exception.catch (Curses.cBreak True) (\_ -> return ())
    k <- Curses.getCh
    case k of
        Nothing -> getKey refresh_fn
        Just k' | k' == Curses.keyResize 
                -> do -- snew <- get_size s
                      refresh_fn --snew
                      getKey refresh_fn
                | otherwise -> return k'
 
-- ---------------------------------------------------------------------
-- | Redraw the entire terminal from the UI state
--
redraw :: IO ()
redraw = do
    ws <- getWindows
    gotoTop
    mapM_ drawWindow ws
    drawCmdLine ":"
    w <- getWindow
    drawCursor (cursor w)

--
-- | Draw a screen to the screen
--
-- TODO take heed of the origin and size params correctly.
-- Take head of 'active' status
-- TODO set cursor invisible
--
drawWindow :: Window -> IO ()
drawWindow (Window { bufkey=u, mode=m, origin=(_,_), 
                     height=h, width=w, tospnt=t } ) = do
    b  <- getBufferWith u
    ss <- nelemsB b (h*w) t           -- maximum visible contents of buffer
    mapM_ (drawLine w) $ take (h-1) $ (lines ss) ++ repeat "~"
    cset_attr (Curses.setReverse Curses.attr0 True , Curses.Pair (0))
    drawLine w m    -- modeline
    reset

--
-- | Draw the editor command line. Make sure not to drop of end of screen.
--
drawCmdLine :: String -> IO ()
drawCmdLine s = do
    (h,w) <- Curses.scrSize
    Curses.wMove Curses.stdScr (h-1) 0
    drawLine (min (w-1) (length s)) s

--
-- | lazy version is faster than calculating length of s
--
drawLine :: Int -> String -> IO ()
drawLine w s  = Curses.wAddStr Curses.stdScr $ take w (s ++ repeat ' ')

--
-- | Given the cursor position in the window. Draw it.
-- TODO take account of offsets
--
drawCursor :: (Int,Int) -> IO ()
drawCursor (y,x) = Curses.withCursor Curses.CursorVisible $ do
    gotoTop
    cset_attr (Curses.setReverse Curses.attr0 True, Curses.Pair 1)
    Curses.wMove Curses.stdScr (y) (x)
    reset

--
-- | move cursor to origin of stdScr.
--
gotoTop :: IO ()
gotoTop = Curses.wMove Curses.stdScr 0 0

{-
redraw :: IO ()
redraw = do
    bs      <- getBuffers
    count   <- lengthBuffers
    current <- getCurrentBuffer
    (h,w)   <- screenSize
    let (y, r) = (h - 1) `quotRem` count    -- work out height of buffers
    gotoTop
    mapM_ (\b -> if b == current
                 then drawMainBufferXY w (y+r) b
                 else drawBufferXY w y b) bs

    case elemIndex current bs of    -- how many screens down is the active one?
            Nothing -> return ()    -- no active screen
            Just i  -> let yoff = y * i
                       in drawPoint yoff 0{-nohorizsplit-} 
                                    (y+r-2) (w-1) current

--
-- | Draw as much of the buffer as we are told to do
--  active buffer gets a slightly different modeline
--
drawBuffer :: Buffer a => Int -> Int -> Int -> a -> IO ()
drawBuffer main w h buf = do
    ss <- nelemsB buf (w*h) 0
    mapM_ (drawLine w) $ take (h-1) $ (lines ss) ++ repeat "~"
    cset_attr (Curses.setReverse Curses.attr0 True , Curses.Pair (main))
    drawModeLine w (nameB buf)
    reset

drawBufferXY :: Buffer a => Int -> Int -> a -> IO ()
drawBufferXY = drawBuffer 1

drawMainBufferXY :: Buffer a => Int -> Int -> a -> IO ()
drawMainBufferXY = drawBuffer 0
-}

------------------------------------------------------------------------
{-
--
-- | Draw the bottom, command line, with contents @ss@
-- This is its own screen, nothing else touches it
--
drawCmd :: String -> IO ()
drawCmd s = do
    (h,w) <- Curses.scrSize
    Curses.wMove Curses.stdScr (h-1) 0
    drawLine (min (w-1) (length s)) s
-}

clearCmd :: IO ()
clearCmd = do
    (h,w) <- Curses.scrSize
    Curses.wMove Curses.stdScr (h-1) 0
    drawLine (w-1) " "

------------------------------------------------------------------------

-- gotoBottom :: IO ()
-- gotoBottom = do
--     (h,_) <- Curses.scrSize
--     Curses.wMove Curses.stdScr h 0

--
-- | Fill to end of line spaces
--
fillLine :: IO ()
fillLine = Curses.clrToEol

-- ---------------------------------------------------------------------
--

--
-- | manipulate the current attributes of the standard screen
--
cset_attr :: (Curses.Attr, Curses.Pair) -> IO ()
cset_attr (a, p) = Curses.wAttrSet Curses.stdScr (a, p)

--
-- | Reset the screen to normal values
--
reset :: IO ()
reset = cset_attr (Curses.attr0, Curses.Pair 0)

-- ---------------------------------------------------------------------
-- Refreshing
--
    
--
-- | redraw and refresh the screen
--
refresh :: IO ()
refresh = redraw >> Curses.refresh

------------------------------------------------------------------------
-- misc

warn :: String -> IO ()
warn msg = do   -- do_message s attr_message msg
    Curses.wMove Curses.stdScr 0 0
    Curses.wAddStr Curses.stdScr $ take 80 $ msg ++ repeat ' '

