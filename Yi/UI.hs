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

        -- * Drawing messages
        drawCmdLine,

        module Yi.Curses   -- UIs need to export the symbolic key names

  )   where

-- TODO the above api should be redesigned. Consider the vi screen api
-- to ncurses for a nice well thought out editor api.

import Yi.Buffer
import Yi.Editor
import Yi.Window

import Yi.Curses hiding ( refresh, Window )
import qualified Yi.Curses as Curses

import Data.Maybe
import Data.List
import Control.Monad                    ( when )
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
    -- draw all windows
    ws <- getWindows
    gotoTop
    mapM_ drawWindow ws

    cl <- readEditor cmdline
    drawCmdLine cl

    -- work out origin of current window from index of that window in win list
    -- still grubby because we aren't using the /origin/ field of 'Window'
    -- _sigh_ assumes bottom window has rem
    w <- getWindow
    when (isJust w) $ do
        (Just i) <- getWindowInd
        (h,_)    <- screenSize
        let o_y = i * (fst $ getY h (length ws))
        drawCursor (o_y,0) $ cursor $ fromJust w

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
    -- draw buffer contents
    b  <- getBufferWith u
    ss <- nelemsB b (h*w) t           -- maximum visible contents of buffer
    mapM_ (drawLine w) $ take (h-1) $ (lines ss) ++ repeat "~"

    -- draw modeline
    cset_attr (Curses.setReverse Curses.attr0 True , Curses.Pair (0))
    drawLine w m
    reset
    

--
-- | Draw the editor command line. Make sure not to drop of end of screen.
--
drawCmdLine :: String -> IO ()
drawCmdLine s = do
    (h,w) <- Curses.scrSize
    Curses.wMove Curses.stdScr (h-1) 0
    let w' = min (w-1) (length s)   -- hmm. what if this is big?
    drawLine w' s
    fillLine
    Curses.wMove Curses.stdScr (h-1) w'

--
-- | lazy version is faster than calculating length of s
--
drawLine :: Int -> String -> IO ()
drawLine w s  = Curses.wAddStr Curses.stdScr $ take w (s ++ repeat ' ')

--
-- | Given the cursor position in the window. Draw it.
-- TODO take account of offsets
--
drawCursor :: (Int,Int) -> (Int,Int) -> IO ()
drawCursor (o_y,_o_x) (y,x) = Curses.withCursor Curses.CursorVisible $ do
    gotoTop
    cset_attr (Curses.setReverse Curses.attr0 True, Curses.Pair 1)
    Curses.wMove Curses.stdScr (o_y + y) (x)
    reset

--
-- | move cursor to origin of stdScr.
--
gotoTop :: IO ()
gotoTop = Curses.wMove Curses.stdScr 0 0

--
-- | Fill to end of line spaces
--
fillLine :: IO ()
fillLine = Curses.clrToEol

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
    
--
-- | redraw and refresh the screen
--
refresh :: IO ()
refresh = redraw >> Curses.refresh

