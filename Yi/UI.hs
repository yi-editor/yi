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
-- | User interface abstractions. Should always be general enough to
-- permit multiple user interfaces.
--
module Yi.UI (

        -- * UI initialisation 
        start, end, 
        screenSize,

        fillLine,       -- IO ()

        refresh,
        reset,
        warn,
        
        getKey,

  ) where

import Yi.Buffer
import qualified Yi.Curses as Curses
import qualified Yi.Editor as Editor
-- import Yi.Style

import Data.List
-- import Data.Maybe                       ( isJust, fromJust ) 
-- import Control.Monad                    ( when )
import qualified Control.Exception      ( catch )

------------------------------------------------------------------------
-- Initialisation

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

--
-- | Find the current screen height and widith. This uses the ffi. You
-- should probably use Editor.getScreenSize once everything is
-- initialised
--
screenSize :: IO (Int, Int)
screenSize = Curses.scrSize

-- ---------------------------------------------------------------------
-- | Read a key.
--
getKey :: IO () -> IO Editor.Key
getKey refresh_fn = do
    Control.Exception.catch (Curses.cBreak True) (\_ -> return ())
    k <- Curses.getCh
    case k of
        Nothing               -> getKey refresh_fn
        Just Editor.KeyResize -> do -- snew <- get_size s
                                    refresh_fn --snew
                                    getKey refresh_fn
        Just k' -> return k'
 
-- ---------------------------------------------------------------------
-- Drawing stuff
--
-- TODO define this in terms of the ncurses WINDOW abstraction, or even
-- pads.
--

--
-- | Draw as much of the buffer as we are told to do
--
drawBufferXY :: Buffer a => Int -> Int -> a -> IO ()
drawBufferXY w h buf = do
    ss <- contents buf
    mapM_ (drawLine w) $ take (h-1) $ (lines ss) ++ repeat []
    cset_attr (Curses.setReverse Curses.attr0 True , Curses.Pair (1))
    drawModeLine w (name buf)
    reset

--
-- | active buffer gets a slightly different modeline
--
drawMainBufferXY :: Buffer a => Int -> Int -> a -> IO ()
drawMainBufferXY w h buf = do
    ss <- contents buf
    mapM_ (drawLine w) $ take (h-1) $ (lines ss) ++ repeat []
    cset_attr (Curses.setReverse Curses.attr0 True , Curses.Pair 0)
    drawModeLine w (name buf)
    reset

--
-- | draw a simple modeline
--
drawModeLine :: Int -> String -> IO ()
drawModeLine w title = drawLine w ("\"" ++ title ++ "\"" ++ repeat ' ')

--
-- | needs to be fast
--
drawLine :: Int -> String -> IO ()
drawLine w s  = Curses.wAddStr Curses.stdScr $ take w (s ++ repeat ' ')

-- 
-- | redraw the entire screen from the editor state
--
redraw :: IO ()
redraw = do
    bs      <- Editor.getBuffers
    count   <- Editor.lengthBuffers
    current <- Editor.getCurrentBuffer
    (h,w)   <- screenSize
    let (y, r) = (h - 1) `quotRem` count    -- work out height of buffers
    gotoOrigin
    mapM_ (\b -> if b == current
                 then drawMainBufferXY w (y+r) b
                 else drawBufferXY w y b) bs

    case elemIndex current bs of    -- how many windows down is the active one?
            Nothing -> return ()    -- no active window
            Just i  -> let yoff = y * i
                       in drawPoint yoff 0{-nohorizsplit-} 
                                    (y+r-2) (w-1) current

--
-- hack. convert a point in the buffer to an (x,y) index in [[Char]]
-- think about how to do this efficiently. Use WINDOW?
-- Input is the Y and X origin of the buffer to draw on the screen, and
-- the buffer itself.
--
drawPoint :: Buffer a => Int -> Int -> Int -> Int -> a -> IO ()
drawPoint y_orig x_orig y_max x_max buf = Curses.withCursor Curses.CursorVisible $ do

    ss <- contents buf -- again :(
    p  <- point buf

    --
    -- calculate the offset of the buffer's point in terms of x and y
    -- offsets from the origin of the buffer.
    --
    -- NB the point is progressing, but we're refusing to scroll for now.
    --
    let prev = reverse $ take p ss
        y = min y_max $ length $ filter (== '\n') prev
        x = min x_max $ case elemIndex '\n' prev of {Just a -> a; _ -> p}

    cset_attr (Curses.setReverse Curses.attr0 True , Curses.Pair 1)
    Curses.wMove Curses.stdScr (y_orig + y) (x_orig + x)
    reset

--
-- | move cursor to origin of stdScr.
--
gotoOrigin :: IO ()
gotoOrigin = Curses.wMove Curses.stdScr 0 0

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
