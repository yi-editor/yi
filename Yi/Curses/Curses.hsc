{-# OPTIONS -fffi -fglasgow-exts #-}
--
-- glaexts needed for newtype deriving 
--
-- -*- haskell -*-
--
-- Copyright (c) 2002-2004 John Meacham (john at repetae dot net)
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- 
-- arch-tag: b25d31d1-0529-4b27-87e3-01618e25c135
--

--
-- | Binding to the [wn]curses library. From the ncurses man page:
--
-- >      The curses library routines give the user a terminal-inde-
-- >      pendent method of updating character screens with  reason-
-- >      able  optimization.
-- 
-- Sections of the quoted documentation are from the OpenBSD man pages, which
-- are distributed under a BSD license.
--
-- A useful reference is: 
--        /Writing Programs with NCURSES/, by Eric S. Raymond and Zeyd
--        M. Ben-Halim, <http://dickey.his.com/ncurses/>
--
-- N.B attrs don't work with Irix curses.h. This should be fixed.
--

module Yi.Curses.Curses {-(

    --------------------------------------------------------------------
    
    Window,             -- data Window deriving Eq
    stdScr,             -- :: Window
    initScr,            -- :: IO Window
    cBreak,             -- :: Bool -> IO ()
    raw,                -- :: Bool -> IO ()
    echo,               -- :: Bool -> IO ()
    nl,                 -- :: Bool -> IO ()
    intrFlush,          -- :: Bool -> IO ()
    keypad,             -- :: Window -> Bool -> IO ()
    noDelay,            -- :: Window -> Bool -> IO ()
    initCurses,         -- :: IO ()
    useDefaultColors,   -- :: IO ()
    endWin,             -- :: IO ()
    resizeTerminal,
    
    clearOk,
    leaveOk,
    
    --------------------------------------------------------------------
    
    scrSize,          -- :: IO (Int, Int)
    refresh,          -- :: IO ()
    
    --------------------------------------------------------------------
    
    hasColors,      -- :: IO Bool
    startColor,     -- :: IO ()
    Pair(..),       -- newtype Pair = Pair Int deriving (Eq, Ord, Ix)
    colorPairs,     -- :: IO Int
    Color(..),      -- newtype Color = Color Int deriving (Eq, Ord, Ix)
    colors,         -- :: IO Int
    color,          -- :: String -> Maybe Color
--    black, red, green, yellow, blue, magenta, cyan, white, -- :: Color
    initPair,       -- :: Pair -> Color -> Color -> IO ()
    pairContent,    -- :: Pair -> IO (Color, Color)
    canChangeColor, -- :: IO Bool
    initColor,      -- :: Color -> (Int, Int, Int) -> IO ()
    colorContent,   -- :: Color -> IO (Int, Int, Int)
    
    --------------------------------------------------------------------
    
    Attr,  -- data Attr deriving Eq
    attr0, -- :: Attr
    
    isAltCharset, isBlink, isBold, isDim, isHorizontal, isInvis,
    isLeft, isLow, isProtect, isReverse, isRight, isStandout, isTop,
    isUnderline, isVertical,
        -- :: Attr -> Bool
    
    setAltCharset, setBlink, setBold, setDim, setHorizontal, setInvis,
    setLeft, setLow, setProtect, setReverse, setRight, setStandout,
    setTop, setUnderline, setVertical,
        -- :: Attr -> Bool -> Attr
    
    attrSet, -- :: Attr -> Pair -> IO ()
    attrOn, attrOff,
    
    --------------------------------------------------------------------

    wAddStr, 
    addLn,         -- :: IO ()
    mvWAddStr,
    wMove,
    getYX,
    
    --------------------------------------------------------------------
    
    bkgrndSet,      -- :: Attr -> Pair -> IO ()
    erase,          -- :: IO ()
    wclear,         -- :: Window -> IO ()
    clrToEol,       -- :: IO ()
    move,           -- :: Int -> Int -> IO ()

    -- Cursor Routines
    CursorVisibility(..), 
    withCursor,

    standout,standend,
    attrDim, attrBold,
    attrDimOn, attrDimOff,
    attrBoldOn, attrBoldOff,
    wAttrOn,
    wAttrOff, 
    touchWin,
    --------------------------------------------------------------------
    -- Mouse Routines
    withMouseEventMask,
    ButtonEvent(..),
    MouseEvent(..),
    
    --------------------------------------------------------------------
    
    getCh, 
    newPad, pRefresh, delWin, newWin,
    wClrToEol,
    withProgram,

    ulCorner, llCorner, urCorner, lrCorner, rTee, lTee, bTee, tTee, hLine,
    vLine, plus, s1, s9, diamond, ckBoard, degree, plMinus, bullet,
    lArrow, rArrow, dArrow, uArrow, board, lantern, block,
    s3, s7, lEqual, gEqual, pi, nEqual, sterling,

    beep, wAttrSet, wAttrGet,

#ifdef SIGWINCH
    cursesSigWinch,
#endif

    cursesTest
    
    --------------------------------------------------------------------
  )-} where 

#include <config.h>
#include <YiCurses.h>

#if HAVE_SIGNAL_H
# include <signal.h>
#endif

import Yi.Curses.CWString       ( withLCStringLen )

import Prelude hiding           ( pi )
import Data.Char
import Data.List
import Data.Ix                  ( Ix )
import Data.Maybe               ( isJust, fromJust )

import Control.Monad
import Control.Exception        ( bracket, bracket_ )

import Foreign
import CForeign

#if __GLASGOW_HASKELL__ < 603
import Data.Bits
#endif

#ifdef SIGWINCH
import System.Posix.Signals
#endif

--
-- If we have the SIGWINCH signal, we use that, with a custom handler,
-- to determine when to resize the screen. Otherwise, we use a similar
-- handler that looks for KEY_RESIZE in the input stream -- the result
-- is a less responsive update, however.
--

------------------------------------------------------------------------
--
-- | Start it all up
--
initCurses :: IO () -> IO ()
initCurses fn = do
    initScr
    b <- hasColors
    when b $ startColor >> useDefaultColors
    raw True    -- raw mode please
    echo False
    nl False
    intrFlush True
    leaveOk False
    keypad stdScr True
    defineKey (#const KEY_UP) "\x1b[1;2A"
    defineKey (#const KEY_DOWN) "\x1b[1;2B"
    defineKey (#const KEY_SLEFT) "\x1b[1;2D"
    defineKey (#const KEY_SRIGHT) "\x1b[1;2C"
#ifdef SIGWINCH
    installHandler (fromJust cursesSigWinch) 
                   (Catch fn) Nothing >> return ()
#endif

------------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

throwIfErr :: Num a => String -> IO a -> IO a
--throwIfErr name act = do
--    res <- act
--    if res == (#const ERR)
--        then ioError (userError ("Curses: "++name++" failed"))
--        else return res
throwIfErr s = throwIf (== (#const ERR)) (\a -> "Curses[" ++ show a ++ "]:"  ++ s)

throwIfErr_ :: Num a => String -> IO a -> IO ()
throwIfErr_ name act = void $ throwIfErr name act

------------------------------------------------------------------------

type WindowTag = ()
type Window = Ptr WindowTag

--
-- | The standard screen
--
stdScr :: Window
stdScr = unsafePerformIO (peek stdscr)
foreign import ccall "static YiCurses.h &stdscr" 
    stdscr :: Ptr Window

--
-- | initscr is normally the first curses routine to call when
-- initializing a program. curs_initscr(3):
--
-- >     To initialize the routines, the routine initscr or newterm
-- >     must be called before any of the other routines that  deal
-- >     with  windows  and  screens  are used. 
--
-- >     The initscr code determines the terminal type and initial-
-- >     izes all curses data structures.  initscr also causes  the
-- >     first  call  to  refresh  to  clear the screen.  If errors
-- >     occur, initscr writes  an  appropriate  error  message  to
-- >     standard error and exits; otherwise, a pointer is returned
-- >     to stdscr.
--
initScr :: IO Window
initScr = throwIfNull "initscr" initscr
foreign import ccall unsafe "YiCurses.h initscr" initscr :: IO Window

--
-- | > The cbreak routine
--   > disables line buffering and erase/kill  character-process-
--   > ing  (interrupt  and  flow  control  characters  are unaf-
--   > fected), making characters typed by the  user  immediately
--   > available  to  the  program.  The nocbreak routine returns
--   > the terminal to normal (cooked) mode.
--
cBreak :: Bool -> IO ()
cBreak True  = throwIfErr_ "cbreak"   cbreak
cBreak False = throwIfErr_ "nocbreak" nocbreak

foreign import ccall unsafe "YiCurses.h cbreak"     cbreak :: IO CInt
foreign import ccall unsafe "YiCurses.h nocbreak" nocbreak :: IO CInt

--
-- |>    The  raw and noraw routines place the terminal into or out
-- >     of raw mode.  Raw mode is similar to cbreak mode, in  that
-- >     characters  typed  are  immediately  passed through to the
-- >     user program.  The differences are that in raw  mode,  the
-- >     interrupt,  quit, suspend, and flow control characters are
-- >     all passed through uninterpreted, instead of generating  a
-- >     signal.   The  behavior  of the BREAK key depends on other
-- >     bits in the tty driver that are not set by curses.
--
raw :: Bool -> IO ()
raw False = throwIfErr_ "noraw" noraw
raw True  = throwIfErr_ "raw"   raw_c

foreign import ccall unsafe "YiCurses.h noraw" noraw :: IO CInt
foreign import ccall unsafe "YiCurses.h raw"   raw_c :: IO CInt

--
-- |>      The  echo  and  noecho routines control whether characters
-- >       typed by the user are echoed by getch as they  are  typed.
-- >       Echoing  by  the  tty  driver is always disabled, but ini-
-- >       tially getch is in echo  mode,  so  characters  typed  are
-- >       echoed.  Authors of most interactive programs prefer to do
-- >       their own echoing in a controlled area of the  screen,  or
-- >       not  to  echo  at  all, so they disable echoing by calling
-- >       noecho.  [See curs_getch(3) for a discussion of how  these
-- >       routines interact with cbreak and nocbreak.]
-- >
--
echo :: Bool -> IO ()
echo False = throwIfErr_ "noecho" noecho
echo True  = throwIfErr_ "echo"   echo_c

foreign import ccall unsafe "YiCurses.h noecho" noecho :: IO CInt
foreign import ccall unsafe "YiCurses.h echo" echo_c :: IO CInt

--
-- |>       The  nl  and  nonl routines control whether the underlying
-- >        display device translates the return key into  newline  on
-- >        input,  and  whether it translates newline into return and
-- >        line-feed on output (in either case, the call  addch('\n')
-- >        does the equivalent of return and line feed on the virtual
-- >        screen).  Initially, these translations do occur.  If  you
-- >        disable  them using nonl, curses will be able to make bet-
-- >        ter use of the line-feed capability, resulting  in  faster
-- >        cursor  motion.   Also, curses will then be able to detect
-- >        the return key.
-- > 
nl :: Bool -> IO ()
nl True  = throwIfErr_ "nl"   nl_c
nl False = throwIfErr_ "nonl" nonl

foreign import ccall unsafe "YiCurses.h nl" nl_c :: IO CInt
foreign import ccall unsafe "YiCurses.h nonl" nonl :: IO CInt

-- |>       If  the intrflush option is enabled, (bf is TRUE), when an
-- >        interrupt key  is  pressed  on  the  keyboard  (interrupt,
-- >        break,  quit)  all  output in the tty driver queue will be
-- >        flushed, giving the  effect  of  faster  response  to  the
-- >        interrupt,  but  causing  curses to have the wrong idea of
-- >        what is on the  screen.   Disabling  (bf  is  FALSE),  the
-- >        option  prevents the flush.
-- > 
intrFlush :: Bool -> IO ()
intrFlush bf =
    throwIfErr_ "intrflush" $ intrflush stdScr (if bf then 1 else 0)
foreign import ccall unsafe "YiCurses.h intrflush" intrflush :: Window -> (#type bool) -> IO CInt

--
-- | Enable the keypad of the user's terminal.
--
keypad :: Window -> Bool -> IO ()
keypad win bf = throwIfErr_ "keypad" $ keypad_c win (if bf then 1 else 0)
foreign import ccall unsafe "YiCurses.h keypad" 
    keypad_c :: Window -> (#type bool) -> IO CInt

noDelay :: Window -> Bool -> IO ()
noDelay win bf =
    throwIfErr_ "nodelay" $ nodelay win (if bf then 1 else 0)

foreign import ccall unsafe "YiCurses.h nodelay" 
    nodelay :: Window -> (#type bool) -> IO CInt

--
-- |   Normally, the hardware cursor is left at the  location  of
--     the  window  cursor  being  refreshed.  The leaveok option
--     allows the cursor to be left wherever the  update  happens
--     to leave it.  It is useful for applications where the cur-
--     sor is not used, since it  reduces  the  need  for  cursor
--     motions.   If  possible, the cursor is made invisible when
--     this option is enabled.
--
leaveOk  :: Bool -> IO CInt
leaveOk True  = leaveok_c stdScr 1
leaveOk False = leaveok_c stdScr 0

foreign import ccall unsafe "YiCurses.h leaveok" 
    leaveok_c :: Window -> (#type bool) -> IO CInt

clearOk :: Bool -> IO CInt
clearOk True  = clearok_c stdScr 1
clearOk False = clearok_c stdScr 0

foreign import ccall unsafe "YiCurses.h clearok" 
    clearok_c :: Window -> (#type bool) -> IO CInt

------------------------------------------------------------------------

foreign import ccall unsafe "YiCurses.h use_default_colors" 
    useDefaultColors :: IO ()

defaultBackground, defaultForeground :: Color
defaultBackground = Color (-1)
defaultForeground = Color (-1)

------------------------------------------------------------------------

defineKey :: CInt -> String -> IO ()
defineKey k s =  withCString s (\s' -> define_key s' k) >> return ()

foreign import ccall unsafe "YiCurses.h define_key" 
    define_key :: Ptr CChar -> CInt -> IO ()

--
-- | >  The program must call endwin for each terminal being used before
--   >  exiting from curses.
--
endWin :: IO ()
endWin = throwIfErr_ "endwin" endwin
foreign import ccall unsafe "YiCurses.h endwin" endwin :: IO CInt

------------------------------------------------------------------------

--
-- | get the dimensions of the screen
--
scrSize :: IO (Int, Int)
scrSize = do
    lnes <- peek linesPtr
    cols <- peek colsPtr
    return (fromIntegral lnes, fromIntegral cols)

foreign import ccall "YiCurses.h &LINES" linesPtr :: Ptr CInt
foreign import ccall "YiCurses.h &COLS"  colsPtr  :: Ptr CInt

--
-- | refresh curses windows and lines. curs_refresh(3)
--
refresh :: IO ()
refresh = throwIfErr_ "refresh" refresh_c

foreign import ccall unsafe "YiCurses.h refresh" 
    refresh_c :: IO CInt

--
-- | Do an actual update. Used after endWin on linux to restore the terminal
--
update :: IO ()
update = throwIfErr_ "update" update_c

foreign import ccall unsafe "YiCurses.h doupdate" 
    update_c :: IO CInt

------------------------------------------------------------------------

hasColors :: IO Bool
hasColors = liftM (/= 0) has_colors
foreign import ccall unsafe "YiCurses.h has_colors" has_colors :: IO (#type bool)

--
-- | Initialise the color settings, also sets the screen to the
-- default colors (white on black)
--
startColor :: IO ()
startColor = throwIfErr_ "start_color" start_color
foreign import ccall unsafe start_color :: IO CInt

newtype Pair = Pair Int deriving (Eq, Ord, Ix)

--
-- | colorPairs defines the maximum number of color-pairs the terminal
-- can support). 
--
colorPairs :: IO Int
colorPairs = fmap fromIntegral $ peek colorPairsPtr

foreign import ccall "YiCurses.h &COLOR_PAIRS" 
        colorPairsPtr :: Ptr CInt

newtype Color = Color Int deriving (Eq, Ord, Ix)

colors :: IO Int
colors = liftM fromIntegral $ peek colorsPtr

foreign import ccall "YiCurses.h &COLORS" colorsPtr :: Ptr CInt

--black, red, green, yellow, blue, magenta, cyan, white :: Color

color :: String -> Maybe Color
color "default"  = Just $ Color (-1)
color "black"    = Just $ Color (#const COLOR_BLACK)
color "red"      = Just $ Color (#const COLOR_RED)
color "green"    = Just $ Color (#const COLOR_GREEN)
color "yellow"   = Just $ Color (#const COLOR_YELLOW)
color "blue"     = Just $ Color (#const COLOR_BLUE)
color "magenta"  = Just $ Color (#const COLOR_MAGENTA)
color "cyan"     = Just $ Color (#const COLOR_CYAN)
color "white"    = Just $ Color (#const COLOR_WHITE)
color _ =  Nothing

data Attribute = Attribute [String] String String
parseAttr :: String -> Attribute 
parseAttr s = Attribute as fg bg where
    rs = filter (not . f . head) $ groupBy (\x y -> f x && f y) (map toLower s) 
    as = filter (`elem` attributes) rs
    col x = if isJust (color x) then return x else Nothing
    fg = fromJust $ msum (map (cGet "fg") rs)  `mplus` msum (map col rs) `mplus` return "default"
    bg = fromJust $ msum (map (cGet "bg") rs) `mplus` return "default"
    f ',' = True
    f c | isSpace c = True
    f _ = False
    cGet p r | (p ++ ":") `isPrefixOf` r = col (drop (length p + 1) r) 
    cGet _ _ = Nothing
    attributes = ["normal", "bold", "blink", "dim", "reverse", "underline" ]

--
-- |   curses support color attributes  on  terminals  with  that
--     capability.   To  use  these  routines start_color must be
--     called, usually right after initscr.   Colors  are  always
--     used  in pairs (referred to as color-pairs).  A color-pair
--     consists of a foreground  color  (for  characters)  and  a
--     background color (for the blank field on which the charac-
--     ters are displayed).  A programmer  initializes  a  color-
--     pair  with  the routine init_pair.  After it has been ini-
--     tialized, COLOR_PAIR(n), a macro  defined  in  <curses.h>,
--     can be used as a new video attribute.
--
--     If  a  terminal  is capable of redefining colors, the pro-
--     grammer can use the routine init_color to change the defi-
--     nition   of   a   color.
--
--     The init_pair routine changes the definition of  a  color-
--     pair.   It takes three arguments: the number of the color-
--     pair to be changed, the foreground color number,  and  the
--     background color number.  For portable applications:
--
--     -    The value of the first argument must be between 1 and
--          COLOR_PAIRS-1.
--
--     -    The value of the second and third arguments  must  be
--          between  0  and  COLORS (the 0 color pair is wired to
--          white on black and cannot be changed).
--
--
initPair :: Pair -> Color -> Color -> IO ()
initPair (Pair p) (Color f) (Color b) =
    throwIfErr_ "init_pair" $
        init_pair (fromIntegral p) (fromIntegral f) (fromIntegral b)

foreign import ccall unsafe 
    init_pair :: CShort -> CShort -> CShort -> IO CInt


pairContent :: Pair -> IO (Color, Color)
pairContent (Pair p) =
    alloca $ \fPtr ->
    alloca $ \bPtr -> do
        throwIfErr "pair_content" $ pair_content (fromIntegral p) fPtr bPtr
        f <- peek fPtr
        b <- peek bPtr
        return (Color (fromIntegral f), Color (fromIntegral b))
foreign import ccall unsafe pair_content :: CShort -> Ptr CShort -> Ptr CShort -> IO CInt

canChangeColor :: IO Bool
canChangeColor = liftM (/= 0) can_change_color
foreign import ccall unsafe can_change_color :: IO (#type bool)

initColor :: Color -> (Int, Int, Int) -> IO ()
initColor (Color c) (r, g, b) =
    throwIfErr_ "init_color" $
        init_color (fromIntegral c) (fromIntegral r) (fromIntegral g) (fromIntegral b)
foreign import ccall unsafe init_color :: CShort -> CShort -> CShort -> CShort -> IO CInt

colorContent :: Color -> IO (Int, Int, Int)
colorContent (Color c) =
    alloca $ \rPtr ->
    alloca $ \gPtr ->
    alloca $ \bPtr -> do
        throwIfErr "color_content" $ color_content (fromIntegral c) rPtr gPtr bPtr
        r <- peek rPtr
        g <- peek gPtr
        b <- peek bPtr
        return (fromIntegral r, fromIntegral g, fromIntegral b)
foreign import ccall unsafe color_content :: CShort -> Ptr CShort -> Ptr CShort -> Ptr CShort -> IO CInt

foreign import ccall unsafe "YiCurses.h hs_curses_color_pair" 
    colorPair :: Pair -> (#type chtype)
#def inline chtype hs_curses_color_pair (HsInt pair) {return COLOR_PAIR (pair);}

-------------
-- Attributes 
-------------

foreign import ccall unsafe "YiCurses.h attr_set" 
    attr_set :: Attr -> CShort -> Ptr a -> IO Int

-- foreign import ccall unsafe "YiCurses.h attr_get" :: Attr -> CShort -> Ptr a -> IO Int

foreign import ccall unsafe "YiCurses.h wattr_set" 
    wattr_set :: Window -> Attr -> CInt -> Ptr a -> IO CInt

foreign import ccall unsafe "YiCurses.h wattr_get" 
    wattr_get :: Window -> Ptr Attr -> Ptr CShort -> Ptr a -> IO CInt

foreign import ccall "YiCurses.h attr_on" attr_on :: (#type attr_t) -> Ptr a -> IO Int
foreign import ccall "YiCurses.h attr_off" attr_off :: (#type attr_t) -> Ptr a -> IO Int
foreign import ccall "YiCurses.h attron" attron :: Int -> IO Int
foreign import ccall "YiCurses.h attroff" attroff :: Int -> IO Int
foreign import ccall unsafe "YiCurses.h wattron" wattron :: Window -> CInt -> IO CInt
foreign import ccall unsafe "YiCurses.h wattroff" wattroff :: Window -> CInt -> IO CInt
foreign import ccall standout :: IO Int
foreign import ccall standend :: IO Int

--
-- |
--
wAttrSet :: Window -> (Attr,Pair) -> IO ()
wAttrSet w (a,(Pair p)) = throwIfErr_ "wattr_set" $ 
    wattr_set w a (fromIntegral p) nullPtr

--
-- | manipulate the current attributes of the named window. see curs_attr(3)
--
wAttrGet :: Window -> IO (Attr,Pair)
wAttrGet w =
    alloca $ \pa -> 
        alloca $ \pp -> do
            throwIfErr_ "wattr_get" $ wattr_get w pa pp nullPtr
            a <- peek pa
            p <- peek pp
            return (a,Pair $ fromIntegral p)


newtype Attr = Attr (#type attr_t) deriving (Eq,Storable,Bits, Num, Show)

--
-- | Normal display (no highlight)
--
attr0 :: Attr
attr0 = Attr (#const WA_NORMAL)

isAltCharset, isBlink, isBold, isDim, isHorizontal, isInvis, isLeft,
    isLow, isProtect, isReverse, isRight, isStandout, isTop,
    isUnderline, isVertical :: Attr -> Bool

isAltCharset = isAttr (#const WA_ALTCHARSET)
isBlink      = isAttr (#const WA_BLINK)
isBold       = isAttr (#const WA_BOLD)
isDim        = isAttr (#const WA_DIM)
isHorizontal = isAttr (#const WA_HORIZONTAL)
isInvis      = isAttr (#const WA_INVIS)
isLeft       = isAttr (#const WA_LEFT)
isLow        = isAttr (#const WA_LOW)
isProtect    = isAttr (#const WA_PROTECT)
isReverse    = isAttr (#const WA_REVERSE)
isRight      = isAttr (#const WA_RIGHT)
isStandout   = isAttr (#const WA_STANDOUT)
isTop        = isAttr (#const WA_TOP)
isUnderline  = isAttr (#const WA_UNDERLINE)
isVertical   = isAttr (#const WA_VERTICAL)

isAttr :: (#type attr_t) -> Attr -> Bool
isAttr b (Attr a) = a .&. b /= 0

--
-- | Setting attributes
--
setAltCharset, setBlink, setBold, setDim, setHorizontal, setInvis,
    setLeft, setLow, setProtect, setReverse, setRight, setStandout,
    setTop, setUnderline, setVertical :: Attr -> Bool -> Attr

setAltCharset = setAttr (#const WA_ALTCHARSET)
setBlink      = setAttr (#const WA_BLINK)
setBold       = setAttr (#const WA_BOLD)
setDim        = setAttr (#const WA_DIM)
setHorizontal = setAttr (#const WA_HORIZONTAL)
setInvis      = setAttr (#const WA_INVIS)
setLeft       = setAttr (#const WA_LEFT)
setLow        = setAttr (#const WA_LOW)
setProtect    = setAttr (#const WA_PROTECT)
setReverse    = setAttr (#const WA_REVERSE)
setRight      = setAttr (#const WA_RIGHT)
setStandout   = setAttr (#const WA_STANDOUT)
setTop        = setAttr (#const WA_TOP)
setUnderline  = setAttr (#const WA_UNDERLINE)
setVertical   = setAttr (#const WA_VERTICAL)

setAttr :: (#type attr_t) -> Attr -> Bool -> Attr
setAttr b (Attr a) False = Attr (a .&. complement b)
setAttr b (Attr a) True  = Attr (a .|.            b)

attrPlus :: Attr -> Attr -> Attr
attrPlus (Attr a) (Attr b) = Attr (a .|. b)

attrSet :: Attr -> Pair -> IO ()
attrSet attr (Pair p) = throwIfErr_ "attrset" $
    attr_set attr (fromIntegral p) nullPtr

attrOn :: Attr -> IO ()
attrOn (Attr attr) = throwIfErr_ "attr_on" $
    attr_on attr nullPtr


attrOff :: Attr -> IO ()
attrOff (Attr attr) = throwIfErr_ "attr_off" $
    attr_off attr nullPtr



wAttrOn :: Window -> Int -> IO ()
wAttrOn w x = throwIfErr_ "wattron" $ wattron w (fi x)

wAttrOff :: Window -> Int -> IO ()
wAttrOff w x = throwIfErr_ "wattroff" $ wattroff w (fi x)

attrDimOn :: IO ()
attrDimOn  = throwIfErr_ "attron A_DIM" $
    attron (#const A_DIM) 

attrDimOff :: IO ()
attrDimOff = throwIfErr_ "attroff A_DIM" $
    attroff (#const A_DIM) 

attrBoldOn :: IO ()
attrBoldOn  = throwIfErr_ "attron A_BOLD" $
    attron (#const A_BOLD) 

attrBoldOff :: IO ()
attrBoldOff = throwIfErr_ "attroff A_BOLD" $
    attroff (#const A_BOLD) 


attrDim :: Int
attrDim = (#const A_DIM)
attrBold :: Int
attrBold = (#const A_BOLD)

------------------------------------------------------------------------

mvWAddStr :: Window -> Int -> Int -> String -> IO ()
mvWAddStr w y x str = wMove w y x >> wAddStr w str 

addLn :: IO ()
addLn = wAddStr stdScr "\n" 

--
-- | normalise the string, stripping \\r and making control chars
-- printable. Called over all output(?)

{-
normalise :: String -> String
normalise []        = []
normalise ('\r':cs) = normalise cs
normalise (c:cs) | isControl c   = '@' : normalise cs
                 | otherwise     = c   : normalise cs
{-# INLINE normalise #-}
-}

{-
normalise s = map f . filter (/= '\r') s
    where
        f c | isPrint c  = c
        f c = '@'
-}

------------------------------------------------------------------------

#if defined(CF_WCHAR_SUPPORT) && defined(HAVE_WADDNWSTR)

--wAddStr :: Window -> String -> IO ()
--wAddStr w str = throwIfErr_ ("waddnwstr: " ++ show str) $ withCWStringLen (normalise str) (\(ws,len) -> waddnwstr w ws (fi len))
    
foreign import ccall unsafe 
    waddnwstr :: Window -> CWString -> CInt -> IO CInt

foreign import ccall unsafe 
    waddch :: Window -> (#type chtype) -> IO CInt

wAddStr :: Window -> String -> IO ()
wAddStr win str = do
    let
        convStr f = case f [] of
            [] -> return ()
            s  -> throwIfErr_ "waddnstr" $
                withCWStringLen  (s) (\(ws,len) ->  (waddnwstr win ws (fi len)))
        loop []        acc = convStr acc
        loop (ch:str') acc = recognize
            ch
            (loop str' (acc . (ch:)))
            (\ch' -> do
                convStr acc
                throwIfErr "waddch" $ waddch win ch'
                loop str' id)
    loop str id 

#else

--
-- This is heavily called, and does a lot of allocs.  We walk over all
-- the string accumulating a list of characters to be drawn.
--
-- Got it down to:
--
--      wAddStr Yi.Curses 20.0   38.1
--      wAddStr Yi.Curses 10.0   32.5
-- 
-- TODO make this way less expensive. That accum sucks.
-- use difference lists for O(1) append
--
wAddStr :: Window -> [Char] -> IO ()
wAddStr _   [] = return ()
wAddStr win s  = throwIfErr_ "waddnstr" $
    withLCStringLen (s) (\(ws,len) -> waddnstr win ws (fi len))

{-
wAddStr :: Window -> String -> IO ()
wAddStr win str = do
    let convStr f = case f [] of
            [] -> return ()
            s  -> case normalise s of
                    s' -> throwIfErr_ "waddnstr" $ 
                        withLCStringLen s' (\(ws,len) -> 
                            waddnstr win ws (fi len))   -- write to screen

    let loop []     acc = convStr acc
        loop (c:cs) acc = 
            recognize c 
                (loop cs $ acc . (c:))
                (\c' -> do convStr acc                 -- draw accumulated chars
                           throwIfErr "waddch" $ waddch win c' -- draw this char
                           loop cs id )
    loop str id 
-}

foreign import ccall threadsafe
    waddnstr :: Window -> CString -> CInt -> IO CInt

foreign import ccall threadsafe
    waddch :: Window -> (#type chtype) -> IO CInt

foreign import ccall threadsafe
    vline  :: Char -> Int -> IO ()

foreign import ccall threadsafe 
    waddchnstr :: Window -> CString -> CInt -> IO CInt

#endif

{-

wAddStr :: Window -> String -> IO ()
wAddStr w str =  withLCStringLen (normalise str) (\(ws,len) -> throwIfErr_ ("waddnstr: " ++ show len ++ " " ++ show str) $ waddnstr w ws (fi len))
foreign import ccall unsafe waddch :: Window -> (#type chtype) -> IO CInt

wAddStr :: Window -> String -> IO ()
wAddStr win str = do
    let
        convStr f = case f [] of
            [] -> return ()
            s  -> throwIfErr_ "waddnstr" $
                withLCString  (normalise s) (\(ws,len) ->  (waddnstr win ws (fi len)))
        loop []        acc = convStr acc
        loop (ch:str') acc = recognize
            ch
            (loop str' (acc . (ch:)))
            (\ch' -> do
                convStr acc
                throwIfErr "waddch" $ waddch win ch'
                loop str' id)
    loop str id 
-}
------------------------------------------------------------------------

--
-- what ?
--

#let translate_attr attr =                              \
    "(if a .&. %lu /= 0 then %lu else 0) .|.",          \
    (unsigned long) WA_##attr, (unsigned long) A_##attr

bkgrndSet :: Attr -> Pair -> IO ()
bkgrndSet (Attr a) p = bkgdset $
    fromIntegral (ord ' ') .|.
    #translate_attr ALTCHARSET
    #translate_attr BLINK
    #translate_attr BOLD
    #translate_attr DIM
    #translate_attr INVIS
    #translate_attr PROTECT
    #translate_attr REVERSE
    #translate_attr STANDOUT
    #translate_attr UNDERLINE
    colorPair p

foreign import ccall unsafe bkgdset :: (#type chtype) -> IO ()

erase :: IO ()
erase = throwIfErr_ "erase" $ werase_c  stdScr
foreign import ccall unsafe "werase" werase_c :: Window -> IO CInt

wclear :: Window -> IO ()
wclear w = throwIfErr_ "wclear" $ wclear_c  w
foreign import ccall unsafe "wclear" wclear_c :: Window -> IO CInt

clrToEol :: IO ()
clrToEol = throwIfErr_ "clrtoeol" clrtoeol
foreign import ccall unsafe clrtoeol :: IO CInt

--
-- | >    move the cursor associated with the window
--   >    to line y and column x.  This routine does  not  move  the
--   >    physical  cursor  of the terminal until refresh is called.
--   >    The position specified is relative to the upper  left-hand
--   >    corner of the window, which is (0,0).
--
-- Note that 'move_c' may be a macro.
--
move :: Int -> Int -> IO ()
move y x = throwIfErr_ "move" $ move_c (fromIntegral y) (fromIntegral x)

foreign import ccall unsafe "move" 
    move_c :: CInt -> CInt -> IO CInt

--
-- | >    move the cursor associated with the window
--   >    to line y and column x.  This routine does  not  move  the
--   >    physical  cursor  of the terminal until refresh is called.
--   >    The position specified is relative to the upper  left-hand
--   >    corner of the window, which is (0,0).
--
wMove :: Window -> Int -> Int -> IO ()
wMove w y x = throwIfErr_ "wmove" $ wmove w (fi y) (fi x)

foreign import ccall unsafe  
    wmove :: Window -> CInt -> CInt -> IO CInt

------------------
-- Cursor routines
------------------

data CursorVisibility = CursorInvisible | CursorVisible | CursorVeryVisible

vis_c :: CursorVisibility -> CInt
vis_c vis = case vis of
    CursorInvisible   -> 0
    CursorVisible     -> 1
    CursorVeryVisible -> 2
    

--
-- | Set the cursor state
--
-- >       The curs_set routine sets  the  cursor  state  is  set  to
-- >       invisible, normal, or very visible for visibility equal to
-- >       0, 1, or 2 respectively.  If  the  terminal  supports  the
-- >       visibility   requested,   the  previous  cursor  state  is
-- >       returned; otherwise, ERR is returned.
--
cursSet :: CInt -> IO CInt
cursSet 0 = leaveOk True  >> curs_set 0
cursSet n = leaveOk False >> curs_set n 

foreign import ccall unsafe "YiCurses.h curs_set" 
    curs_set :: CInt -> IO CInt

--
-- | set the cursor, and do action
--
withCursor :: CursorVisibility -> IO a -> IO a
withCursor nv action = 
    Control.Exception.bracket 
        (cursSet (vis_c nv))            -- before
        (\v -> case v of                -- after
                (#const ERR) -> return 0
                x            -> cursSet x) 
        (\_ -> action)                  -- do this

-- 
-- | Get the current cursor coordinates
--
getYX :: Window -> IO (Int, Int)
getYX w =
    alloca $ \py ->                 -- allocate two ints on the stack
        alloca $ \px -> do
            nomacro_getyx w py px   -- writes current cursor coords
            y <- peek py
            x <- peek px
            return (fromIntegral y, fromIntegral x)

--
-- | Get the current cursor coords, written into the two argument ints.
--
-- >    The getyx macro places the current cursor position of the given
-- >    window in the two integer variables y and x.
--
--      void getyx(WINDOW *win, int y, int x);
--
foreign import ccall unsafe "YiUtils.h nomacro_getyx" 
        nomacro_getyx :: Window -> Ptr CInt -> Ptr CInt -> IO ()

------------------------------------------------------------------------


touchWin :: Window -> IO ()
touchWin w = throwIfErr_ "touchwin" $ touchwin w
foreign import ccall touchwin :: Window -> IO CInt

newPad :: Int -> Int -> IO Window
newPad nlines ncols = throwIfNull "newpad" $ 
    newpad (fromIntegral nlines) (fromIntegral ncols)

pRefresh :: Window -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
pRefresh pad pminrow pmincol sminrow smincol smaxrow smaxcol = 
    throwIfErr_ "prefresh" $
        prefresh pad (fromIntegral pminrow) 
                     (fromIntegral pmincol) 
                     (fromIntegral sminrow) 
                     (fromIntegral smincol) 
                     (fromIntegral smaxrow) 
                     (fromIntegral smaxcol)

delWin :: Window -> IO ()
delWin w = throwIfErr_ "delwin" $ delwin w
    
foreign import ccall unsafe 
    prefresh :: Window -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe 
    newpad :: CInt -> CInt -> IO Window

foreign import ccall unsafe 
    delwin :: Window -> IO CInt

newWin :: Int -> Int -> Int -> Int -> IO Window
newWin nlines ncolumn begin_y begin_x = throwIfNull "newwin" $ 
    newwin (fi nlines) (fi ncolumn) (fi begin_y) (fi begin_x)

foreign import ccall unsafe 
    newwin :: CInt -> CInt -> CInt -> CInt -> IO Window

wClrToEol :: Window -> IO ()
wClrToEol w = throwIfErr_ "wclrtoeol" $ wclrtoeol w

foreign import ccall unsafe wclrtoeol :: Window -> IO CInt

--
-- | >      The getch, wgetch, mvgetch and mvwgetch, routines read a
--   >      character  from the window.
--
foreign import ccall threadsafe getch :: IO CInt

--foreign import ccall unsafe def_prog_mode :: IO CInt
--foreign import ccall unsafe reset_prog_mode :: IO CInt
foreign import ccall unsafe flushinp :: IO CInt


withProgram :: IO a -> IO a
withProgram action = withCursor CursorVisible $ 
    Control.Exception.bracket_ (endWin) (flushinp) action

--withProgram action = withCursor CursorVisible $ Control.Exception.bracket_ ({-def_prog_mode >> -}endWin) (return ()){-reset_prog_mode-} action


foreign import ccall unsafe "YiCurses.h beep" c_beep :: IO CInt
foreign import ccall unsafe "YiCurses.h flash" c_flash :: IO CInt

beep :: IO ()
beep = do
    br <- c_beep
    when (br /= (#const OK)) (c_flash >> return ()) 

------------------------------------------------------------------------
--
-- | Map curses keys to real chars. The lexer will like this.
--
decodeKey :: CInt -> Char
decodeKey = chr . fromIntegral
{-# INLINE decodeKey #-}

--
-- | Some constants for easy symbolic manipulation.
-- NB we don't map keys to an abstract type anymore, as we can't use
-- Alex lexers then.
--
keyBreak :: Char
keyBreak        = chr (#const KEY_BREAK)
keyDown :: Char
keyDown         = chr (#const KEY_DOWN)
keyUp :: Char
keyUp           = chr (#const KEY_UP)
keyLeft :: Char
keyLeft         = chr (#const KEY_LEFT)
keyRight :: Char
keyRight        = chr (#const KEY_RIGHT)
keyHome :: Char
keyHome         = chr (#const KEY_HOME)
keyBackspace :: Char
keyBackspace    = chr (#const KEY_BACKSPACE)
keyDL :: Char
keyDL           = chr (#const KEY_DL)
keyIL :: Char
keyIL           = chr (#const KEY_IL)
keyDC :: Char
keyDC           = chr (#const KEY_DC)
keyIC :: Char
keyIC           = chr (#const KEY_IC)
keyEIC :: Char
keyEIC          = chr (#const KEY_EIC)
keyClear :: Char
keyClear        = chr (#const KEY_CLEAR)
keyEOS :: Char
keyEOS          = chr (#const KEY_EOS)
keyEOL :: Char
keyEOL          = chr (#const KEY_EOL)
keySF :: Char
keySF           = chr (#const KEY_SF)
keySR :: Char
keySR           = chr (#const KEY_SR)
keyNPage :: Char
keyNPage        = chr (#const KEY_NPAGE)
keyPPage :: Char
keyPPage        = chr (#const KEY_PPAGE)
keySTab :: Char
keySTab         = chr (#const KEY_STAB)
keyCTab :: Char
keyCTab         = chr (#const KEY_CTAB)
keyCATab :: Char
keyCATab        = chr (#const KEY_CATAB)
keyEnter :: Char
keyEnter        = chr (#const KEY_ENTER)
keySReset :: Char
keySReset       = chr (#const KEY_SRESET)
keyReset :: Char
keyReset        = chr (#const KEY_RESET)
keyPrint :: Char
keyPrint        = chr (#const KEY_PRINT)
keyLL :: Char
keyLL           = chr (#const KEY_LL)
keyA1 :: Char
keyA1           = chr (#const KEY_A1)
keyA3 :: Char
keyA3           = chr (#const KEY_A3)
keyB2 :: Char
keyB2           = chr (#const KEY_B2)
keyC1 :: Char
keyC1           = chr (#const KEY_C1)
keyC3 :: Char
keyC3           = chr (#const KEY_C3)
keyBTab :: Char
keyBTab         = chr (#const KEY_BTAB)
keyBeg :: Char
keyBeg          = chr (#const KEY_BEG)
keyCancel :: Char
keyCancel       = chr (#const KEY_CANCEL)
keyClose :: Char
keyClose        = chr (#const KEY_CLOSE)
keyCommand :: Char
keyCommand      = chr (#const KEY_COMMAND)
keyCopy :: Char
keyCopy         = chr (#const KEY_COPY)
keyCreate :: Char
keyCreate       = chr (#const KEY_CREATE)
keyEnd :: Char
keyEnd          = chr (#const KEY_END)
keyExit :: Char
keyExit         = chr (#const KEY_EXIT)
keyFind :: Char
keyFind         = chr (#const KEY_FIND)
keyHelp :: Char
keyHelp         = chr (#const KEY_HELP)
keyMark :: Char
keyMark         = chr (#const KEY_MARK)
keyMessage :: Char
keyMessage      = chr (#const KEY_MESSAGE)
keyMove :: Char
keyMove         = chr (#const KEY_MOVE)
keyNext :: Char
keyNext         = chr (#const KEY_NEXT)
keyOpen :: Char
keyOpen         = chr (#const KEY_OPEN)
keyOptions :: Char
keyOptions      = chr (#const KEY_OPTIONS)
keyPrevious :: Char
keyPrevious     = chr (#const KEY_PREVIOUS)
keyRedo :: Char
keyRedo         = chr (#const KEY_REDO)
keyReference :: Char
keyReference    = chr (#const KEY_REFERENCE)
keyRefresh :: Char
keyRefresh      = chr (#const KEY_REFRESH)
keyReplace :: Char
keyReplace      = chr (#const KEY_REPLACE)
keyRestart :: Char
keyRestart      = chr (#const KEY_RESTART)
keyResume :: Char
keyResume       = chr (#const KEY_RESUME)
keySave :: Char
keySave         = chr (#const KEY_SAVE)
keySBeg :: Char
keySBeg         = chr (#const KEY_SBEG)
keySCancel :: Char
keySCancel      = chr (#const KEY_SCANCEL)
keySCommand :: Char
keySCommand     = chr (#const KEY_SCOMMAND)
keySCopy :: Char
keySCopy        = chr (#const KEY_SCOPY)
keySCreate :: Char
keySCreate      = chr (#const KEY_SCREATE)
keySDC :: Char
keySDC          = chr (#const KEY_SDC)
keySDL :: Char
keySDL          = chr (#const KEY_SDL)
keySelect :: Char
keySelect       = chr (#const KEY_SELECT)
keySEnd :: Char
keySEnd         = chr (#const KEY_SEND)
keySEOL :: Char
keySEOL         = chr (#const KEY_SEOL)
keySExit :: Char
keySExit        = chr (#const KEY_SEXIT)
keySFind :: Char
keySFind        = chr (#const KEY_SFIND)
keySHelp :: Char
keySHelp        = chr (#const KEY_SHELP)
keySHome :: Char
keySHome        = chr (#const KEY_SHOME)
keySIC :: Char
keySIC          = chr (#const KEY_SIC)
keySLeft :: Char
keySLeft        = chr (#const KEY_SLEFT)
keySMessage :: Char
keySMessage     = chr (#const KEY_SMESSAGE)
keySMove :: Char
keySMove        = chr (#const KEY_SMOVE)
keySNext :: Char
keySNext        = chr (#const KEY_SNEXT)
keySOptions :: Char
keySOptions     = chr (#const KEY_SOPTIONS)
keySPrevious :: Char
keySPrevious    = chr (#const KEY_SPREVIOUS)
keySPrint :: Char
keySPrint       = chr (#const KEY_SPRINT)
keySRedo :: Char
keySRedo        = chr (#const KEY_SREDO)
keySReplace :: Char
keySReplace     = chr (#const KEY_SREPLACE)
keySRight :: Char
keySRight       = chr (#const KEY_SRIGHT)
keySRsume :: Char
keySRsume       = chr (#const KEY_SRSUME)
keySSave :: Char
keySSave        = chr (#const KEY_SSAVE)
keySSuspend :: Char
keySSuspend     = chr (#const KEY_SSUSPEND)
keySUndo :: Char
keySUndo        = chr (#const KEY_SUNDO)
keySuspend :: Char
keySuspend      = chr (#const KEY_SUSPEND)
keyUndo :: Char
keyUndo         = chr (#const KEY_UNDO)
#ifdef KEY_RESIZE
keyResize :: Char
keyResize       = chr (#const KEY_RESIZE)
#endif
#ifdef KEY_MOUSE
keyMouse :: Char
keyMouse        = chr (#const KEY_MOUSE)
#endif

--
-- | is a char a function key?
--
isFKey :: Char -> Bool
isFKey c = case fromIntegral $ ord c :: CInt of
        key -> key >= (#const KEY_F0) && key <= (#const KEY_F(63))

-- ---------------------------------------------------------------------
-- get char
--

--getCh :: IO Key
--getCh = threadWaitRead 0 >> (liftM decodeKey $ throwIfErr "getch" getch)

--getCh :: IO Key
--getCh = liftM decodeKey $ throwIfErr "getch" getch

-- getCh :: IO Key
-- getCh = do
--     nodelay stdScr 1
--     --halfdelay 1
--     v <- getch
--     case v of
--              (#const ERR) -> yield >> getCh 
--              x -> return $ decodeKey x

--
-- | read a character from the window
--
getCh :: IO (Maybe Char)
getCh = do
    v <- getch
    case v of
        -- we won't get ^C otherwise..
         (#const ERR) -> do getch {-discard-} ; return (Just $ '\^C') -- hack
         k            -> return (Just $ decodeKey k)


resizeTerminal :: Int -> Int -> IO ()

#ifdef HAVE_RESIZETERM
resizeTerminal a b = throwIfErr_ "resizeterm"  $ resizeterm (fi a) (fi b)

foreign import ccall unsafe "YiCurses.h resizeterm" 
    resizeterm :: CInt -> CInt -> IO CInt
#else
resizeTerminal _ _ = return ()
#endif

#ifdef SIGWINCH
cursesSigWinch :: Maybe Signal
cursesSigWinch = Just (#const SIGWINCH)
#endif

------------
-- Test case
------------

cursesTest :: IO ()
cursesTest = do
    initScr
    hc <- hasColors 
    when hc startColor
    ccc <- canChangeColor
    (ys,xs) <- scrSize
    cp <- colorPairs
    cs <- colors
    endWin
    putStrLn $ "ScreenSize: " ++ show (xs,ys) 
    putStrLn $ "hasColors: " ++ show hc
    putStrLn $ "canChangeColor: " ++ show ccc
    putStrLn $ "colorPairs: " ++ show cp
    putStrLn $ "colors: " ++ show cs

    


-----------------
-- Mouse Routines
-----------------

data MouseEvent = MouseEvent {
    mouseEventId :: Int, 
    mouseEventX :: Int, 
    mouseEventY :: Int, 
    mouseEventZ :: Int, 
    mouseEventButton :: [ButtonEvent]
   } deriving(Show)

data ButtonEvent = ButtonPressed Int | ButtonReleased Int | ButtonClicked Int | 
    ButtonDoubleClicked Int | ButtonTripleClicked Int | ButtonShift | ButtonControl | ButtonAlt 
                deriving(Eq,Show)

withMouseEventMask :: [ButtonEvent] -> IO a -> IO a

#ifdef KEY_MOUSE

foreign import ccall unsafe "YiCurses.h mousemask" 
    mousemask :: (#type mmask_t) -> Ptr (#type mmask_t) -> IO (#type mmask_t)

withMouseEventMask bes action = do
    ov <- alloca (\a ->  mousemask (besToMouseMask bes) a >> peek a) 
    r <- action 
    mousemask ov nullPtr 
    return r

besToMouseMask :: [ButtonEvent] -> (#type mmask_t)
besToMouseMask bes = foldl' (.|.) 0 (map cb bes) where
    cb (ButtonPressed 1) = (#const BUTTON1_PRESSED)
    cb (ButtonPressed 2) = (#const BUTTON2_PRESSED)
    cb (ButtonPressed 3) = (#const BUTTON3_PRESSED)
    cb (ButtonPressed 4) = (#const BUTTON4_PRESSED)
    cb (ButtonReleased 1) = (#const BUTTON1_RELEASED)
    cb (ButtonReleased 2) = (#const BUTTON2_RELEASED)
    cb (ButtonReleased 3) = (#const BUTTON3_RELEASED)
    cb (ButtonReleased 4) = (#const BUTTON4_RELEASED)
    cb (ButtonClicked 1) = (#const BUTTON1_CLICKED)
    cb (ButtonClicked 2) = (#const BUTTON2_CLICKED)
    cb (ButtonClicked 3) = (#const BUTTON3_CLICKED)
    cb (ButtonClicked 4) = (#const BUTTON4_CLICKED)
    cb ButtonShift = (#const BUTTON_SHIFT)
    cb ButtonAlt = (#const BUTTON_ALT)
    cb ButtonControl = (#const BUTTON_CTRL)
    cb _ = 0


#else
withMouseEventMask _ a = a

#endif




ulCorner, llCorner, urCorner, lrCorner, rTee, lTee, bTee, tTee, hLine,
    vLine, plus, s1, s9, diamond, ckBoard, degree, plMinus, bullet,
    lArrow, rArrow, dArrow, uArrow, board, lantern, block,
    s3, s7, lEqual, gEqual, pi, nEqual, sterling
    :: Char

ulCorner = chr 0x250C
llCorner = chr 0x2514
urCorner = chr 0x2510
lrCorner = chr 0x2518
rTee     = chr 0x2524
lTee     = chr 0x251C
bTee     = chr 0x2534
tTee     = chr 0x252C
hLine    = chr 0x2500
vLine    = chr 0x2502
plus     = chr 0x253C
s1       = chr 0x23BA -- was: 0xF800
s9       = chr 0x23BD -- was: 0xF804
diamond  = chr 0x25C6
ckBoard  = chr 0x2592
degree   = chr 0x00B0
plMinus  = chr 0x00B1
bullet   = chr 0x00B7
lArrow   = chr 0x2190
rArrow   = chr 0x2192
dArrow   = chr 0x2193
uArrow   = chr 0x2191
board    = chr 0x2591
lantern  = chr 0x256C
block    = chr 0x2588
s3       = chr 0x23BB -- was: 0xF801
s7       = chr 0x23BC -- was: 0xF803
lEqual   = chr 0x2264
gEqual   = chr 0x2265
pi       = chr 0x03C0
nEqual   = chr 0x2260
sterling = chr 0x00A3

{-
-- haddock doesn't like these commented out with --
   #if defined(__STDC_ISO_10646__)  && defined(HAVE_WADDNWSTR)
   #else 
-}

recognize :: Char -> IO a -> ((#type chtype) -> IO a) -> IO a
recognize _ch noConvert _convert = noConvert -- Handle the most common case first.

{-

recognize :: Char -> IO a -> ((#type chtype) -> IO a) -> IO a
recognize ch noConvert convert
    | ch <= '\x7F'   = noConvert -- Handle the most common case first.
    | ch <= '\x7F'   = noConvert -- Handle the most common case first.
    | ch == ulCorner = convert =<< hs_curses_acs_ulcorner
    | ch == llCorner = convert =<< hs_curses_acs_llcorner
    | ch == urCorner = convert =<< hs_curses_acs_urcorner
    | ch == lrCorner = convert =<< hs_curses_acs_lrcorner
    | ch == rTee     = convert =<< hs_curses_acs_rtee
    | ch == lTee     = convert =<< hs_curses_acs_ltee
    | ch == bTee     = convert =<< hs_curses_acs_btee
    | ch == tTee     = convert =<< hs_curses_acs_ttee
    | ch == hLine    = convert =<< hs_curses_acs_hline
    | ch == vLine    = convert =<< hs_curses_acs_vline
    | ch == plus     = convert =<< hs_curses_acs_plus
    | ch == s1       = convert =<< hs_curses_acs_s1
    | ch == s9       = convert =<< hs_curses_acs_s9
    | ch == diamond  = convert =<< hs_curses_acs_diamond
    | ch == ckBoard  = convert =<< hs_curses_acs_ckboard
    | ch == degree   = convert =<< hs_curses_acs_degree
    | ch == plMinus  = convert =<< hs_curses_acs_plminus
    | ch == bullet   = convert =<< hs_curses_acs_bullet
    | ch == lArrow   = convert =<< hs_curses_acs_larrow
    | ch == rArrow   = convert =<< hs_curses_acs_rarrow
    | ch == dArrow   = convert =<< hs_curses_acs_darrow
    | ch == uArrow   = convert =<< hs_curses_acs_uarrow
    | ch == board    = convert =<< hs_curses_acs_board
    | ch == lantern  = convert =<< hs_curses_acs_lantern
    | ch == block    = convert =<< hs_curses_acs_block
#  ifdef ACS_S3
    | ch == s3       = convert =<< hs_curses_acs_s3
    | ch == s7       = convert =<< hs_curses_acs_s7
    | ch == lEqual   = convert =<< hs_curses_acs_lequal
    | ch == gEqual   = convert =<< hs_curses_acs_gequal
    | ch == pi       = convert =<< hs_curses_acs_pi
    | ch == nEqual   = convert =<< hs_curses_acs_nequal
    | ch == sterling = convert =<< hs_curses_acs_sterling
#  endif
    | otherwise      = noConvert

foreign import ccall unsafe hs_curses_acs_ulcorner :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_llcorner :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_urcorner :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_lrcorner :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_rtee     :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_ltee     :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_btee     :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_ttee     :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_hline    :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_vline    :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_plus     :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_s1       :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_s9       :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_diamond  :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_ckboard  :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_degree   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_plminus  :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_bullet   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_larrow   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_rarrow   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_darrow   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_uarrow   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_board    :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_lantern  :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_block    :: IO (#type chtype)
#  ifdef ACS_S3
foreign import ccall unsafe hs_curses_acs_s3       :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_s7       :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_lequal   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_gequal   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_pi       :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_nequal   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_sterling :: IO (#type chtype)
#  endif

#def inline chtype hs_curses_acs_ulcorner (void) {return ACS_ULCORNER;}
#def inline chtype hs_curses_acs_llcorner (void) {return ACS_LLCORNER;}
#def inline chtype hs_curses_acs_urcorner (void) {return ACS_URCORNER;}
#def inline chtype hs_curses_acs_lrcorner (void) {return ACS_LRCORNER;}
#def inline chtype hs_curses_acs_rtee     (void) {return ACS_RTEE;}
#def inline chtype hs_curses_acs_ltee     (void) {return ACS_LTEE;}
#def inline chtype hs_curses_acs_btee     (void) {return ACS_BTEE;}
#def inline chtype hs_curses_acs_ttee     (void) {return ACS_TTEE;}
#def inline chtype hs_curses_acs_hline    (void) {return ACS_HLINE;}
#def inline chtype hs_curses_acs_vline    (void) {return ACS_VLINE;}
#def inline chtype hs_curses_acs_plus     (void) {return ACS_PLUS;}
#def inline chtype hs_curses_acs_s1       (void) {return ACS_S1;}
#def inline chtype hs_curses_acs_s9       (void) {return ACS_S9;}
#def inline chtype hs_curses_acs_diamond  (void) {return ACS_DIAMOND;}
#def inline chtype hs_curses_acs_ckboard  (void) {return ACS_CKBOARD;}
#def inline chtype hs_curses_acs_degree   (void) {return ACS_DEGREE;}
#def inline chtype hs_curses_acs_plminus  (void) {return ACS_PLMINUS;}
#def inline chtype hs_curses_acs_bullet   (void) {return ACS_BULLET;}
#def inline chtype hs_curses_acs_larrow   (void) {return ACS_LARROW;}
#def inline chtype hs_curses_acs_rarrow   (void) {return ACS_RARROW;}
#def inline chtype hs_curses_acs_darrow   (void) {return ACS_DARROW;}
#def inline chtype hs_curses_acs_uarrow   (void) {return ACS_UARROW;}
#def inline chtype hs_curses_acs_board    (void) {return ACS_BOARD;}
#def inline chtype hs_curses_acs_lantern  (void) {return ACS_LANTERN;}
#def inline chtype hs_curses_acs_block    (void) {return ACS_BLOCK;}
#  ifdef ACS_S3
#def inline chtype hs_curses_acs_s3       (void) {return ACS_S3;}
#def inline chtype hs_curses_acs_s7       (void) {return ACS_S7;}
#def inline chtype hs_curses_acs_lequal   (void) {return ACS_LEQUAL;}
#def inline chtype hs_curses_acs_gequal   (void) {return ACS_GEQUAL;}
#def inline chtype hs_curses_acs_pi       (void) {return ACS_PI;}
#def inline chtype hs_curses_acs_nequal   (void) {return ACS_NEQUAL;}
#def inline chtype hs_curses_acs_sterling (void) {return ACS_STERLING;}
#  endif

-}

-- ---------------------------------------------------------------------
-- code graveyard
--

{-

addStr :: String -> IO ()
addStr str =
    throwIfErr_ "addstr" $
    withCStringConv (readIORef cursesOutConv) str addstr
foreign import ccall unsafe addstr :: Ptr CChar -> IO CInt

addStrLn :: String -> IO ()
addStrLn str = addStr str >> addLn

--
-- | add a string of characters to a curses window and advance cursor
-- curs_addstr(3)
--
wAddStr :: Window -> String -> IO ()
wAddStr w str = throwIfErr_ "waddstr" $
    withCStringConv (readIORef cursesOutConv) str (waddstr w)

foreign import ccall unsafe waddstr :: Window -> Ptr CChar -> IO CInt


addGraphStr :: String -> IO ()
addGraphStr str = do
    conv <- readIORef cursesOutConv
    let
        convStr f = case f [] of
            [] -> return ()
            s  -> throwIfErr_ "addstr" $
                withCStringConv (return conv) s addstr
        loop []        acc = convStr acc
        loop (ch:str') acc = recognize
            ch
            (loop str' (acc . (ch:)))
            (\ch' -> do
                convStr acc
                throwIfErr "addch" $ addch ch'
                loop str' id)
    loop str id

addGraphStrLn :: String -> IO ()
addGraphStrLn str = do addGraphStr str; addLn

-}

-- vim: sw=4 ts=4
