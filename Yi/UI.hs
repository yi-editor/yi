{-# OPTIONS -#include YiUtils.h #-}
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
        drawWindow,
        drawLine,

        -- * Drawing
        refresh,

        module Yi.Curses   -- UIs need to export the symbolic key names

  )   where

-- TODO the above api should be redesigned. Consider the vi screen api
-- to ncurses for a nice well thought out editor api.

import Yi.Buffer        ( Buffer(ptrToLnsB) )
import Yi.Editor
import Yi.Window
import Yi.Style

import Yi.Curses hiding ( refresh, Window )
import qualified Yi.Curses as Curses

import Data.Maybe                   ( isJust, fromJust )
import Data.List
import Data.IORef
import Control.Monad                ( when )
import qualified Control.Exception  ( catch )
import System.IO.Unsafe             ( unsafePerformIO )

--
-- | how to initialise the ui
--
start :: IO ()
start = do
    Curses.initCurses                   -- initialise the screen
    sty <- readEditor uistyle
    pairs <- initUiColors sty
    writeIORef pairMap pairs
    uiAttr (window sty) >>= \(_,p) -> bkgrndSet nullA p
    Curses.keypad Curses.stdScr True    -- grab the keyboard

--
-- | Clean up and go home
--
end :: IO ()
end = Curses.endWin

--
-- | Find the current screen height and width.
--
screenSize :: IO (Int, Int)
screenSize = Curses.scrSize

--
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
 
--
-- | Redraw the entire terminal from the UI state
-- Optimised.
--
redraw :: IO ()
redraw = withEditor $ \e ->
    
    case getWindows e     of { ws  ->
    case cmdline e        of { cl  ->
    case uistyle e        of { sty ->
    case getWindowOf e    of { w   ->
    case getWindowIndOf e of { Nothing -> return () ; (Just i) -> do

    gotoTop
    mapM_ (drawWindow e w sty) ws               -- draw all windows
    withStyle (window sty) $ drawCmdLine cl     -- draw cmd line

    -- work out origin of current window from index of that window in win list
    -- still grubby because we aren't using the /origin/ field of 'Window'
    -- _sigh_ assumes bottom window has rem
    when (isJust w) $ do
        (h,_)  <- screenSize
        case i * (fst $! getY h (length ws)) of { o_y ->
            drawCursor (o_y,0) $ cursor $ fromJust w
        }

    }}}}}

-- ---------------------------------------------------------------------
-- PRIVATE:

--
-- | Draw a screen to the screen
--
-- This function does most of the allocs, and needs to be optimised to
-- bits
--
-- Arity: 5 Strictness: S(SAAAAAALLLLLLAA) L L U(LLLAALLAAAALA) L
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

    (ptr,len) <- ptrToLnsB b t h    -- ptr to chunk of buffer to draw

    -- draw our buffer segment straight out. no boxing :)
    (y,_) <- getYX Curses.stdScr
    withStyle wsty $ throwIfErr_ "waddnstr" $
            waddnstr Curses.stdScr ptr (fromIntegral len)

    -- and any eof markers
    withStyle eofsty $ do
        (y',_) <- getYX Curses.stdScr
        mapM_ (drawLine w) $ take (h - 1  - (y' - y)) $ repeat "~"

    -- draw modeline
    fn <- return $! case mwin of
            Just win' | win' == win -> modeline_focused 
            _         -> modeline
    withStyle (fn sty) $! drawLine w m

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
        fn (Style fg bg) p = let (_,fgc) = fg2attr fg
                                 (_,bgc) = bg2attr bg
                             in do initPair (Pair p) fgc bgc
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

