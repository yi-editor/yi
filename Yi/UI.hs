{-# OPTIONS -#include "YiUtils.h" #-}
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

-- | This module defines a user interface implemented using ncurses.

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

import Yi.Buffer        ( Buffer( ptrToLnsB
                                , getMarkB  ) )
import Yi.Editor
import Yi.Window
import Yi.Style
import Yi.Curses hiding ( refresh )
import qualified Yi.Curses as Curses

import qualified Data.ByteString as P
import qualified Data.ByteString.Base as P
import qualified Data.ByteString.Char8 as C

import Data.Maybe                   ( isNothing, fromJust )
import Data.List

import Control.Monad                ( when )
import System.Posix.Signals         ( raiseSignal, sigTSTP )


-- Just really to allow me to give some signatures
-- import Foreign.C.Types      ( CInt )
import Foreign.C.String     ( CString )
------------------------------------------------------------------------

-- | how to initialise the ui
start :: (IO ()) -> IO ()
start fn = do
    Curses.initCurses fn                -- initialise the screen
    initcolours ui
    Curses.keypad Curses.stdScr True    -- grab the keyboard

-- | Clean up and go home
end :: IO ()
end = Curses.endWin

-- | Suspend the program
suspend :: IO ()
suspend = raiseSignal sigTSTP

-- | Find the current screen height and width.
screenSize :: IO (Int, Int)
screenSize = Curses.scrSize

--
-- | Read a key. UIs need to define a method for getting events.
-- We only need to refresh if we don't have the SIGWINCH signal handler
-- working for us.
--
getKey :: IO () -> IO Char
getKey _refresh_fn = do
    k <- Curses.getCh
#ifdef KEY_RESIZE
    if k == Curses.keyResize
        then do
#ifndef SIGWINCH
              refresh_fn
#endif
              getKey refresh_fn
        else return k
#else
    return k
#endif

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
    -- so t is the current point at the top of the screen.
    -- pnt is where the current cursor is.
    case win of { Window { bufkey = u
                         , mode   = m
                         , height = h
                         , width  = w
                         , tospnt = t 
                         , pnt    = point} ->
    case window sty   of { wsty ->
    case selected sty of { selsty ->
    case eof    sty   of { eofsty -> do
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

    {-
      That was the first attempt, now what we'll do is split each
      line in to three parts, the bit before the selection, the
      bit within the selection and the bit after the selection,
      at most one line will have all there parts
      (and then only if the selection forms a middle part of
      one line), most will either be all selection, if the
      selection covers multiple lines, or completely before or
      completely after the selection. 
    -}
    markPoint <- getMarkB b
    let startSelect = min markPoint point
        stopSelect  = (max markPoint point) + 1

        -- @todo{signature}
        lineTest (sol, len) = startSelect <= sol &&
                              stopSelect > sol

        -- The integer argument is the current point at the start of
        -- the line we wish to draw
        drawLines :: Int -> [(CString, Int)] -> IO ()
        drawLines _ []                    = return ()
        drawLines sol ((ptr, len) : rest) =
            -- @todo{Make sure these can't *all* be zero
            -- Notice for example that some conditions imply others, eg
            -- stopSelect < eol implies startSelect < eol, so
            -- startSelect > sol && stopSelect < eol implies that
            -- the selection starts and ends on this line.

            -- I think that the inSel conditions can be slightly optimised.
            let eol        = sol + len
                byteString = P.packCString ptr
                beforeSel
                    | startSelect > sol         = min len (startSelect - sol)
                    | otherwise                 = 0
                inSel    
                    -- selection starts and ends on this line
                    | startSelect >= sol &&
                      stopSelect  < eol         = stopSelect - startSelect

                    -- selection is entirely before this line
                    | stopSelect  < sol         = 0

                    -- selection is entirely after this line
                    | startSelect > eol         = 0

                    -- this line is entirely within the selection
                    | startSelect < sol &&
                      stopSelect  > eol         = len

                    -- selection begins before this line, ends during it
                    | startSelect < sol &&
                      stopSelect  > sol         = stopSelect - sol

                    -- selection begins on this line, ends after it
                    | startSelect >= sol &&
                      stopSelect  > eol         = eol - startSelect

                    -- selection outside this line (not really needed)
                    | otherwise                 = 0
                afterSel = len - (beforeSel + inSel)
                --     | stopSelect < eol          = min len (eol - stopSelect)
                --     | otherwise                 = 0
                (beforeSelPtrB,
                 afterStartPtrB) = P.splitAt beforeSel byteString
                (inSelPtrB,
                 afterSelPtrB)   = P.splitAt inSel afterStartPtrB
            in
            -- @todo{A little optimisation by not drawing any of the
            -- three parts whose length is zero.
            do withStyle wsty $ P.useAsCString beforeSelPtrB $
                     \pointer ->
                     throwIfErr_ (C.pack "drawWindow") $
                     waddnstr Curses.stdScr pointer $
                     fromIntegral beforeSel
               withStyle selsty $ P.useAsCString inSelPtrB $
                     \pointer ->
                     throwIfErr_ (C.pack "drawWindow") $
                     waddnstr Curses.stdScr pointer $
                     fromIntegral inSel
               withStyle wsty $ P.useAsCString afterSelPtrB $
                     \pointer ->
                     throwIfErr_ (C.pack "drawWindow") $
                     waddnstr Curses.stdScr pointer $
                     fromIntegral afterSel
               drawLines (len + sol) rest

    drawLines t lns

--    withStyle lineStyle $ flip mapM_ lns $ \(ptr,len) -> do
--        throwIfErr_ (C.pack "drawWindow") $
--            waddnstr Curses.stdScr ptr (fromIntegral len)

    -- and any eof markers (should be optional)
    withStyle eofsty $ do
        (_,x) <- getYX Curses.stdScr

        when (x /= 0) $ throwIfErr_ (C.pack "waddnstr") $
            P.unsafeUseAsCString (C.pack "\n") $ \cstr -> 
                waddnstr Curses.stdScr cstr 1

        (y',_) <- getYX Curses.stdScr
        let diff = h - off - (y' - y)
        mapM_ (\s -> drawLine w s >> clrToEol >> lineDown) $ 
              take diff $ repeat [windowfill e]
        {-
        if windowfill e /= ' '
            then mapM_ (\s -> drawLine w s >> clrToEol >> lineDown) $ 
                    take diff $ repeat [windowfill e]
            else Curses.wMove Curses.stdScr (y' + diff) 0 -- just move the cursor
        -}


    -- draw modeline
    when (not $ isNothing m) $ do
        fn <- return $! case mwin of
                Just win' | win' == win -> modeline_focused
                _         -> modeline
        withStyle (fn sty) $! drawLine w (fromJust m)

    }}}}}

--
-- | Draw the editor command line. Make sure not to drop off end of screen.
--
drawCmdLine :: String -> IO ()
drawCmdLine s = do
    (h,w) <- Curses.scrSize
    Curses.wMove Curses.stdScr (h-1) 0
    let w' = min (w-1) (length s)   -- hmm. what if this is big?
    drawLine w' s
    fillLine
    Curses.wMove Curses.stdScr (h-1) w'

-- | Draw a line quickly
drawLine :: Int -> String -> IO ()
drawLine w s = case C.pack s of
    ps -> P.unsafeUseAsCString ps $ \cstr -> 
        throwIfErr_ (C.pack "drawLine") $
            Curses.waddnstr Curses.stdScr cstr (fromIntegral (min w (P.length ps)))

lineDown :: IO ()
lineDown = do
    (h,_) <- screenSize
    (y,_) <- Curses.getYX Curses.stdScr
    Curses.wMove Curses.stdScr (min h (y+1)) 0

-- | Given the cursor position in the window. Draw it.
drawCursor :: (Int,Int) -> (Int,Int) -> IO ()
drawCursor (o_y,_o_x) (y,x) = Curses.withCursor Curses.CursorVisible $ do
    gotoTop
    (h,w) <- scrSize
    Curses.wMove Curses.stdScr (min (h-1) (o_y + y)) (min (w-1) x)

-- | move cursor to origin of stdScr.
gotoTop :: IO ()
gotoTop = Curses.wMove Curses.stdScr 0 0

-- | Fill to end of line spaces
fillLine :: IO ()
fillLine = Curses.clrToEol

-- | redraw and refresh the screen
refresh :: IO ()
refresh = redraw >> Curses.refresh

-- | Resize the window
-- From "Writing Programs with NCURSES", by Eric S. Raymond and Zeyd M. Ben-Halim
resizeui :: IO (Int,Int)
resizeui = do
    Curses.endWin
    Curses.resetParams
    Curses.refresh
    Curses.scrSize
