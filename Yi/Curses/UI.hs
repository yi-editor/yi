{-# OPTIONS -cpp -#include YiUtils.h -#include <signal.h> #-}
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

module Yi.Curses.UI (

        -- * UI initialisation 
        start, end, suspend, 
        initcolours,
        screenSize,

        -- * Input
        getKey,

        -- * Drawing
        refresh,
        resizeui,

        module Yi.Curses.Curses   -- UIs need to export the symbolic key names

  )   where

-- TODO the above api should be redesigned. Consider the vi screen api
-- to ncurses for a nice well thought out editor api.

import Yi.Buffer        ( Buffer(ptrToLnsB) )
import Yi.Editor
import Yi.Window
import Yi.Style

import Yi.Curses.Curses hiding ( refresh, Window )
import qualified Yi.Curses.Curses as Curses

import Data.Char                    ( ord )
import Data.Maybe                   ( isNothing, isJust, fromJust )
import Data.List
import Data.IORef

import Control.Monad                ( when )
import Control.Exception            ( handle )

import System.IO.Unsafe             ( unsafePerformIO )

import System.Posix.Signals

--
-- | how to initialise the ui
--
start :: (IO ()) -> IO ()
start fn = do
    Curses.initCurses fn                -- initialise the screen
    initcolours
    Curses.keypad Curses.stdScr True    -- grab the keyboard

--
-- | And turn on the colours
--
initcolours :: IO ()
initcolours = do
    sty <- readEditor uistyle
    pairs <- initUiColors sty
    writeIORef pairMap pairs
    uiAttr (window sty) >>= \(_,p) -> bkgrndSet nullA p

--
-- | Clean up and go home
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
    --Control.Exception.catch (Curses.cBreak True) (\_ -> return ())
    k <- Curses.getCh
    case k of
        Nothing -> getKey refresh_fn
        Just k' | k' == Curses.keyResize 
                -> do
#ifndef SIGWINCH
                      refresh_fn
#endif
                      getKey refresh_fn
                | otherwise -> return k'
 
--
-- | Redraw the entire terminal from the UI state
-- Optimised.
--
-- It is crucial that redraw doesn't modify the editor state (of course
-- it shouldn't). Just slipping in a modifyEditor_ there  will kill
-- your redraw speed, as every redraw will trigger another redraw...
-- So don't be tempted.
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

    -- work out origin of current window from index of that window in win list
    -- still grubby because we aren't using the /origin/ field of 'Window'
    -- _sigh_ assumes bottom window has rem
    --
    -- this is no good for line wrapping.
    --
    if (not cmdfoc && isJust w) 
        then do (h,_)  <- screenSize
                let w' = fromJust w
                case i * (fst $! getY h (length ws)) of { o_y ->
                    drawCursor (o_y,0) (cursor w')
                }
        else return () -- Curses.wMove Curses.stdScr y x

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

    lns <- ptrToLnsB b t (h - off)

    -- draw each buffer line
    (y,_) <- getYX Curses.stdScr
    withStyle wsty $ flip mapM_ lns $ \(ptr,len) -> 
        throwIfErr_ "drawWindow" $
            waddnstr Curses.stdScr ptr (fromIntegral $ min len w)

    -- and any eof markers
    withStyle eofsty $ do
        (_,x) <- getYX Curses.stdScr
        when (x /= 0) $ throwIfErr_ "waddch" $
            waddch Curses.stdScr (fromIntegral $ ord '\n') -- no nl at eof.  better flush
        (y',_) <- getYX Curses.stdScr
        mapM_ (drawLine w) $ take (h - off - (y' - y)) $ repeat "~"

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
drawLine :: Int -> String -> IO ()
drawLine w s = Curses.wAddStr Curses.stdScr $! take w (s ++ repeat ' ')

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
-- | manipulate the current attributes of the standard screen
--
setAttribute :: (Curses.Attr, Curses.Pair) -> IO ()
setAttribute (a, p) = Curses.wAttrSet Curses.stdScr (a, p)

--
-- | Reset the screen to normal values
--
reset :: IO ()
reset = setAttribute (Curses.attr0, Curses.Pair 0)
    
--
-- | redraw and refresh the screen
--
refresh :: IO ()
refresh = redraw >> Curses.refresh

--
-- | Resize the window
-- From "Writing Programs with NCURSES", by Eric S. Raymond and Zeyd M. Ben-Halim
--
--
resizeui :: IO (Int,Int)
resizeui = do
    Curses.endWin
    Curses.refresh
    Curses.scrSize

--
-- | Set some colours, perform an action, and then reset the colours
--
withStyle :: Style -> (IO ()) -> IO ()
withStyle sty fn = uiAttr sty >>= setAttribute >> fn >> reset

------------------------------------------------------------------------
--
-- | Set up the ui attributes, given a ui style record
--
-- Returns an association list of pairs for foreground and bg colors,
-- associated with the terminal color pair that has been defined for
-- those colors.
--
-- TODO remember to update this if new fields are added to the ui
--
initUiColors :: UIStyle -> IO [((Curses.Color, Curses.Color), Pair)]
initUiColors (UIStyle {
                 window=wn, 
                 modeline_focused=mlf, 
                 modeline=ml, 
                 eof=ef }) =
    mapM (uncurry fn) (zip [wn, mlf, ml, ef] [1..])
    where
        fn :: Style -> Int -> IO ((Curses.Color, Curses.Color), Pair)
        fn (Style fg bg) p = do let (_,fgc) = fg2attr fg
                                    (_,bgc) = bg2attr bg
                                handle (\_ -> return ()) $ 
                                    initPair (Pair p) fgc bgc
                                return ((fgc,bgc), (Pair p))

--
-- | Getting from nice abstract colours to ncurses-settable values
--
uiAttr :: Style -> IO (Curses.Attr, Curses.Pair)
uiAttr (Style fg bg) = do
    let (a, fgc) = fg2attr fg
        (b, bgc) = bg2attr bg
    pair <- lookupPair (fgc, bgc)
    return (a `attrPlus` b, pair)

--
-- | retrieve a mapping from the pair map
--
lookupPair :: (Curses.Color, Curses.Color) -> IO (Curses.Pair)
lookupPair p = do pm <- readIORef pairMap
                  return $ case lookup p pm of
                        Nothing   -> Pair 0 -- default settings
                        Just pair -> pair

--
-- | map of Curses.Color pairs to ncurses terminal Pair settings
--
pairMap :: IORef [ ((Curses.Color, Curses.Color), Pair) ]
pairMap = unsafePerformIO $ newIORef []
{-# NOINLINE pairMap #-}

-- ---------------------------------------------------------------------
--
-- | mapping abstract colours to ncurses attributes and colours
--

bg2attr :: BackgroundColor -> (Curses.Attr, Curses.Color)
bg2attr c = case c of
    BlackB      -> (nullA, black)
    DarkRedB    -> (nullA, red)
    DarkGreenB  -> (nullA, green)
    BrownB      -> (nullA, yellow)
    DarkBlueB   -> (nullA, blue)
    PurpleB     -> (nullA, magenta)
    DarkCyanB   -> (nullA, cyan)
    WhiteB      -> (nullA, white)
    DefaultB    -> (nullA, defaultColor)

fg2attr :: ForegroundColor -> (Curses.Attr, Curses.Color)
fg2attr c = case c of
    BlackF       -> (nullA, black)
    GreyF        -> (boldA, black)
    DarkRedF     -> (nullA, red)
    RedF         -> (boldA, red)
    DarkGreenF   -> (nullA, green)
    GreenF       -> (boldA, green)
    BrownF       -> (nullA, yellow)
    YellowF      -> (boldA, yellow)
    DarkBlueF    -> (nullA, blue)
    BlueF        -> (boldA, blue)
    PurpleF      -> (nullA, magenta)
    MagentaF     -> (boldA, magenta)
    DarkCyanF    -> (nullA, cyan)
    CyanF        -> (boldA, cyan)
    WhiteF       -> (nullA, white)
    BrightWhiteF -> (boldA, white)
    DefaultF     -> (nullA, defaultColor)

------------------------------------------------------------------------
--
-- Basic (ncurses) colours.
--
defaultColor :: Curses.Color
defaultColor = fromJust $ Curses.color "default"

black, red, green, yellow, blue, magenta, cyan, white :: Curses.Color
black     = fromJust $ Curses.color "black"
red       = fromJust $ Curses.color "red"
green     = fromJust $ Curses.color "green"
yellow    = fromJust $ Curses.color "yellow"
blue      = fromJust $ Curses.color "blue"
magenta   = fromJust $ Curses.color "magenta"
cyan      = fromJust $ Curses.color "cyan"
white     = fromJust $ Curses.color "white"

--
-- Combine attribute with another attribute
--
setBoldA :: Curses.Attr -> Curses.Attr
setBoldA = flip Curses.setBold True

-- setUnderlineA, setDimA, setReverseA :: Curses.Attr -> Curses.Attr
-- setUnderlineA = flip Curses.setUnderline True
-- setDimA       = flip Curses.setDim       True
-- setReverseA   = flip Curses.setReverse   True

--
-- | Some attribute constants
--
boldA, nullA :: Curses.Attr
nullA       = Curses.attr0
boldA       = setBoldA      nullA

-- underlineA, dimA, reverseA :: Curses.Attr
-- underlineA  = setUnderlineA nullA
-- dimA        = setDimA       nullA
-- reverseA    = setReverseA   nullA

