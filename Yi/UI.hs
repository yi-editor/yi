{-# OPTIONS -#include "YiCurses.h" #-}

#include "config.h"

--
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
        start, end, suspend,
        screenSize, initcolours,

        -- * Input
        getKey,

        -- * Drawing
        refresh,
        resizeui,

        module Yi.Curses   -- UIs need to export the symbolic key names

  )   where

-- TODO the above api should be redesigned. Consider the vi screen api
-- to ncurses for a nice well thought out editor api.

import Yi.Buffer        ( Buffer(ptrToLnsB) )
import Yi.Editor
import Yi.Window
import Yi.Style
import Yi.Curses hiding ( refresh )
import qualified Yi.Curses as Curses

import qualified Data.FastPackedString as P

import Data.Char                    ( ord )
import Data.Maybe                   ( isNothing, fromJust )
import Data.List

import Foreign

import Control.Monad                ( when )
import System.IO.Unsafe             ( unsafePerformIO )
import System.Posix.Signals         ( raiseSignal, sigTSTP )

--
-- | how to initialise the ui
--
start :: (IO ()) -> IO ()
start fn = do
    Curses.initCurses fn                -- initialise the screen
    initcolours ui
    Curses.keypad Curses.stdScr True    -- grab the keyboard

--
-- | Clean up and go home. Refresh is needed on linux. grr.
--
end :: IO ()
end = Curses.endWin

--
-- | Suspend the program
--
suspend :: IO ()
suspend = raiseSignal sigTSTP

--
-- | Find the current screen height and width.
--
screenSize :: IO (Int, Int)
screenSize = Curses.scrSize

--
-- | Read a key. UIs need to define a method for getting events.
-- We only need to refresh if we don't have the SIGWINCH signal handler
-- working for us.
--
getKey :: IO () -> IO Char
getKey refresh_fn = do
    k <- Curses.getCh
    if k == Curses.keyResize
        then do
#ifndef SIGWINCH
              refresh_fn
#endif
              getKey refresh_fn
        else return k

--
-- | Redraw the entire terminal from the UI state
-- Optimised.
--
-- It is crucial that redraw doesn't modify the editor state (of course
-- it shouldn't). Just slipping in a modifyEditor_ there  will kill
-- your redraw speed, as every redraw will trigger another redraw...
-- So don't be tempted.
--
-- Two points remain: horizontal scrolling, and tab handling.
--
redraw :: IO ()
redraw = withEditor $ \e ->

    case getWindows e     of { ws  ->
    case cmdline e        of { cl  ->
    case cmdlinefocus e   of { cmdfoc ->
    case uistyle e        of { sty ->
    case getWindowOf e    of { w   ->
    case getWindowIndOf e of { Nothing -> return () ; (Just i) -> do

    gotoTop
    mapM_ (drawWindow e w sty) ws               -- draw all windows

    withStyle (window sty) $ drawCmdLine cl     -- draw cmd line

    -- and now position cursor.
    -- will be influenced by whether the focused window is scrolled left or right
    when (not cmdfoc) $
        case w of
           -- calculate origin of focused window
           -- sum of heights of windows above this one on screen.

           -- needs to be shifted 'x' by the width of the tabs on this line
           Just w' ->
                case sum [ height (ws !! k) | k <- [0 .. (i-1)] ] of
                        o_y -> drawCursor (o_y,0) (cursor w')

           _ -> return ()

    }}}}}}

-- ---------------------------------------------------------------------
-- PRIVATE:

--
-- | Draw a screen to the screen
--
-- This function does most of the allocs, and needs to be optimised to
-- bits
--
-- Ok. Now, how do we deal with line wrapping? The lns we get from ptrs
-- will have to be broken up, dropping some off the end. The cursor
-- will have to be recalculated too.
--
drawWindow :: Editor
           -> Maybe Window
           -> UIStyle
           -> Window
           -> IO ()

drawWindow e mwin sty win =

    case win of { Window {bufkey=u, mode=m, height=h, width=w, tospnt=t} ->
    case window sty of { wsty ->
    case eof    sty of { eofsty -> do
    case findBufferWith e u of { b -> do
    let off = case m of Nothing -> 0 ; _ -> 1 -- correct for modeline

    lns <- ptrToLnsB b t (h - off) w

    -- draw each buffer line
    -- ToDo, horizontal scrolling. determine how many screen widths to
    -- drop off the string (i.e. add to the ptr..)
    --
    -- This `len' doesn't take tabs into account. Leading to curses draw
    -- errors on lines with tabs in them. Solution, find a len that
    -- includes tab widths
    --
    -- `len' is number of chars in the line -- not the screen width.
    -- so `w' is wrong in the presence of tabs.
    --
    -- need a function that takes the desired width, and tells us how
    -- many real chars to take.
    --
    (y,_) <- getYX Curses.stdScr
    withStyle wsty $ flip mapM_ lns $ \(ptr,len) -> do
        throwIfErr_ (P.pack "drawWindow") $
            waddnstr Curses.stdScr ptr (fromIntegral len)

    -- and any eof markers (should be optional)
    withStyle eofsty $ do
        (_,x) <- getYX Curses.stdScr

        when (x /= 0) $ throwIfErr_ (P.pack "waddnstr") $
            P.unsafeUseAsCString (P.pack "\n") $ \cstr -> 
                waddnstr Curses.stdScr cstr 1

        (y',_) <- getYX Curses.stdScr
        let diff = h - off - (y' - y)
        if windowfill e /= ' '
            then mapM_ (drawLine w) $ take diff $ repeat [windowfill e]
            else Curses.wMove Curses.stdScr (y' + diff) 0 -- just move the cursor

    -- draw modeline
    when (not $ isNothing m) $ do
        fn <- return $! case mwin of
                Just win' | win' == win -> modeline_focused
                _         -> modeline
        withStyle (fn sty) $! drawLine w (fromJust m)

    }}}}

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
-- | Draw an error message to the command line
--
{-
drawErrLine :: String -> IO ()
drawErrLine s = do
    sty <- readEditor uistyle
    withStyle (error_messages sty) $ drawCmdLine s
-}

--
-- | lazy version is faster than calculating length of s
-- 
-- waddnstr should do, shouldn't it?
--
drawLine :: Int -> String -> IO ()
drawLine w s = 
  let ps = P.pack s
      qs = P.concat [ps, replicatePS (w-P.length ps) ' ']
  in P.unsafeUseAsCString qs $ \cstr -> throwIfErr_ (P.pack "drawLine") $
        Curses.waddnstr Curses.stdScr cstr (fromIntegral w)

--
-- | Given the cursor position in the window. Draw it.
-- TODO take account of offsets
--
drawCursor :: (Int,Int) -> (Int,Int) -> IO ()
drawCursor (o_y,_o_x) (y,x) = Curses.withCursor Curses.CursorVisible $ do
    gotoTop
    (h,w) <- scrSize
    Curses.wMove Curses.stdScr (min (h-1) (o_y + y)) (min (w-1) x)

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
-- | Reset the screen to normal values
--
-- reset :: IO ()
-- reset = setAttribute (Curses.attr0, Curses.Pair 0)

--
-- | redraw and refresh the screen
--
refresh :: IO ()
refresh = redraw >> Curses.refresh

--
-- | Resize the window
-- From "Writing Programs with NCURSES", by Eric S. Raymond and Zeyd M. Ben-Halim
--
resizeui :: IO (Int,Int)
resizeui = do
    Curses.endWin
    Curses.resetParams
    Curses.refresh
    Curses.scrSize


-- replicateP w c = P.unfoldr w (\u -> Just (u,u)) c
replicatePS :: Int -> Char -> P.FastString
replicatePS w c = unsafePerformIO $ P.generate w $ \ptr -> go ptr w
    where 
        x = fromIntegral . ord $ c
        go _   0 = return w
        go ptr n = poke ptr x >> go (ptr `plusPtr` 1) (n-1)
