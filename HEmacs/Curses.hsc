-- arch-tag: b25d31d1-0529-4b27-87e3-01618e25c135

module HEmacs.Curses (

    --------------------------------------------------------------------
    
    Window,      -- data Window deriving Eq
    stdScr,      -- :: Window
    initScr,     -- :: IO Window
    cBreak,      -- :: Bool -> IO ()
    raw,         -- :: Bool -> IO ()
    echo,        -- :: Bool -> IO ()
    nl,          -- :: Bool -> IO ()
    intrFlush,   -- :: Bool -> IO ()
    keypad,      -- :: Window -> Bool -> IO ()
    noDelay,     -- :: Window -> Bool -> IO ()
    initCurses,  -- :: IO ()
    useDefaultColors, -- :: IO ()
    endWin,      -- :: IO ()
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
    
    Key(..),
    getCh, 
    newPad, pRefresh, delWin, newWin,
    wClrToEol,
    withProgram,

    ulCorner, llCorner, urCorner, lrCorner, rTee, lTee, bTee, tTee, hLine,
    vLine, plus, s1, s9, diamond, ckBoard, degree, plMinus, bullet,
    lArrow, rArrow, dArrow, uArrow, board, lantern, block,
    s3, s7, lEqual, gEqual, pi, nEqual, sterling,

    beep, wAttrSet, wAttrGet,

    cursesSigWinch,
    cursesTest
    ) 
    
    --------------------------------------------------------------------
    where

import HEmacs.GenUtil(foldl')
import HEmacs.CWString

import Prelude hiding (pi)
import Monad 
import Char           (chr, ord, isPrint, isSpace, toLower)
import Ix             (Ix)

import Data.Bits
import Control.Concurrent
import Foreign
import CForeign

import System.IO.Unsafe
import Control.Exception hiding(block)

import System.Posix.Signals
import List 
import Monad
import Maybe

#include <my_curses.h>

------------------------------------------------------------------------

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

data WindowTag 
type Window = Ptr WindowTag

stdScr :: Window
stdScr = unsafePerformIO (peek stdscr)
foreign import ccall "static my_curses.h &stdscr" stdscr :: Ptr Window


initScr :: IO Window
initScr = throwIfNull "initscr" initscr
foreign import ccall unsafe "my_curses.h initscr" initscr :: IO Window

cBreak :: Bool -> IO ()
cBreak True  = throwIfErr_ "cbreak"   cbreak
cBreak False = throwIfErr_ "nocbreak" nocbreak
foreign import ccall unsafe "my_curses.h cbreak" cbreak :: IO CInt
foreign import ccall unsafe "my_curses.h nocbreak" nocbreak :: IO CInt

raw :: Bool -> IO ()
raw False = throwIfErr_ "noraw" noraw
raw True  = throwIfErr_ "raw"   raw_c
foreign import ccall unsafe "my_curses.h noraw" noraw :: IO CInt
foreign import ccall unsafe "my_curses.h raw" raw_c :: IO CInt

echo :: Bool -> IO ()
echo False = throwIfErr_ "noecho" noecho
echo True  = throwIfErr_ "echo"   echo_c
foreign import ccall unsafe "my_curses.h noecho" noecho :: IO CInt
foreign import ccall unsafe "my_curses.h echo" echo_c :: IO CInt

nl :: Bool -> IO ()
nl True  = throwIfErr_ "nl"   nl_c
nl False = throwIfErr_ "nonl" nonl
foreign import ccall unsafe "my_curses.h nl" nl_c :: IO CInt
foreign import ccall unsafe "my_curses.h nonl" nonl :: IO CInt

intrFlush :: Bool -> IO ()
intrFlush bf =
    throwIfErr_ "intrflush" $ intrflush stdScr (if bf then 1 else 0)
foreign import ccall unsafe "my_curses.h intrflush" intrflush :: Window -> (#type bool) -> IO CInt

keypad :: Window -> Bool -> IO ()
keypad win bf =
    throwIfErr_ "keypad" $ keypad_c win (if bf then 1 else 0)
foreign import ccall unsafe "my_curses.h keypad" keypad_c :: Window -> (#type bool) -> IO CInt

noDelay :: Window -> Bool -> IO ()
noDelay win bf =
    throwIfErr_ "nodelay" $ nodelay win (if bf then 1 else 0)
foreign import ccall unsafe "my_curses.h nodelay" nodelay :: Window -> (#type bool) -> IO CInt

foreign import ccall unsafe "my_curses.h leaveok" leaveok_c :: Window -> (#type bool) -> IO CInt

leaveOk True = leaveok_c stdScr 1
leaveOk False = leaveok_c stdScr 0


foreign import ccall unsafe "my_curses.h clearok" clearok_c :: Window -> (#type bool) -> IO CInt

clearOk True = clearok_c stdScr 1
clearOk False = clearok_c stdScr 0

#if 1
--HAVE_USE_DEFAULT_COLORS
foreign import ccall unsafe "my_curses.h use_default_colors" useDefaultColors :: IO ()

defaultBackground = Color (-1)
defaultForeground = Color (-1)

foreign import ccall unsafe "my_curses.h define_key" define_key :: Ptr CChar -> CInt -> IO ()
defineKey k s =  withCString s (\s -> define_key s k) >> return ()

#else

useDefaultColors :: IO ()
useDefaultColors = return ()

defaultBackground = black
defaultForeground = white 

defineKey k s = return ()

#endif

initCurses :: IO ()
initCurses = do
    initScr
    b <- hasColors
    when b startColor
    --when b useDefaultColors
    cBreak True
    echo False
    nl False
    leaveOk True
    intrFlush False
    keypad stdScr True
    defineKey (#const KEY_UP) "\x1b[1;2A"
    defineKey (#const KEY_DOWN) "\x1b[1;2B"
    defineKey (#const KEY_SLEFT) "\x1b[1;2D"
    defineKey (#const KEY_SRIGHT) "\x1b[1;2C"

endWin :: IO ()
endWin = throwIfErr_ "endwin" endwin
foreign import ccall unsafe "my_curses.h endwin" endwin :: IO CInt

------------------------------------------------------------------------

scrSize :: IO (Int, Int)
scrSize = do
    lines <- peek linesPtr
    cols  <- peek colsPtr
    return (fromIntegral lines, fromIntegral cols)

foreign import ccall "my_curses.h &LINES" linesPtr :: Ptr CInt
foreign import ccall "my_curses.h &COLS" colsPtr :: Ptr CInt


refresh :: IO ()
refresh = throwIfErr_ "refresh" refresh_c
foreign import ccall unsafe "my_curses.h refresh" refresh_c :: IO CInt

------------------------------------------------------------------------

hasColors :: IO Bool
hasColors = liftM (/= 0) has_colors
foreign import ccall unsafe "my_curses.h has_colors" has_colors :: IO (#type bool)

startColor :: IO ()
startColor = throwIfErr_ "start_color" start_color
foreign import ccall unsafe start_color :: IO CInt

newtype Pair = Pair Int deriving (Eq, Ord, Ix)

colorPairs :: IO Int
colorPairs = fmap fromIntegral $ peek colorPairsPtr


foreign import ccall "my_curses.h &COLOR_PAIRS" colorPairsPtr :: Ptr CInt

newtype Color = Color Int deriving (Eq, Ord, Ix)

colors :: IO Int
colors = liftM fromIntegral $ peek colorsPtr

foreign import ccall "my_curses.h &COLORS" colorsPtr :: Ptr CInt

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


initPair :: Pair -> Color -> Color -> IO ()
initPair (Pair p) (Color f) (Color b) =
    throwIfErr_ "init_pair" $
        init_pair (fromIntegral p) (fromIntegral f) (fromIntegral b)
foreign import ccall unsafe init_pair :: CShort -> CShort -> CShort -> IO CInt

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

foreign import ccall unsafe "my_curses.h hs_curses_color_pair" colorPair :: Pair -> (#type chtype)
#def inline chtype hs_curses_color_pair (HsInt pair) {return COLOR_PAIR (pair);}

-------------
-- Attributes 
-------------

foreign import ccall unsafe "my_curses.h attr_set" attr_set :: Attr -> CShort -> Ptr a -> IO Int
-- foreign import ccall unsafe "my_curses.h attr_get" :: Attr -> CShort -> Ptr a -> IO Int

foreign import ccall unsafe "my_curses.h wattr_set" wattr_set :: Window -> Attr -> CInt -> Ptr a -> IO CInt
foreign import ccall unsafe "my_curses.h wattr_get" wattr_get :: Window -> Ptr Attr -> Ptr CShort -> Ptr a -> IO CInt

foreign import ccall "my_curses.h attr_on" attr_on :: (#type attr_t) -> Ptr a -> IO Int
foreign import ccall "my_curses.h attr_off" attr_off :: (#type attr_t) -> Ptr a -> IO Int
foreign import ccall "my_curses.h attron" attron :: Int -> IO Int
foreign import ccall "my_curses.h attroff" attroff :: Int -> IO Int
foreign import ccall unsafe "my_curses.h wattron" wattron :: Window -> CInt -> IO CInt
foreign import ccall unsafe "my_curses.h wattroff" wattroff :: Window -> CInt -> IO CInt
foreign import ccall standout :: IO Int
foreign import ccall standend :: IO Int

wAttrSet :: Window -> (Attr,Pair) -> IO ()
wAttrSet w (a,(Pair p)) = throwIfErr_ "wattr_set" $ wattr_set w a (fromIntegral p) nullPtr
wAttrGet :: Window -> IO (Attr,Pair)
wAttrGet w =  alloca $ \pa -> alloca $ \pp -> (throwIfErr_ "wattr_get" $ wattr_get w pa pp nullPtr) >> (peek pa >>=  \a -> peek pp >>=  \p -> return (a,Pair (fromIntegral p)))


newtype Attr = Attr (#type attr_t) deriving (Eq,Storable,Bits, Num, Show)

attr0 :: Attr
attr0 = Attr (#const WA_NORMAL)

isAltCharset, isBlink, isBold, isDim, isHorizontal, isInvis, isLeft,
    isLow, isProtect, isReverse, isRight, isStandout, isTop,
    isUnderline, isVertical
    :: Attr -> Bool
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
isAttr bit (Attr a) = a .&. bit /= 0

setAltCharset, setBlink, setBold, setDim, setHorizontal, setInvis,
    setLeft, setLow, setProtect, setReverse, setRight, setStandout,
    setTop, setUnderline, setVertical
    :: Attr -> Bool -> Attr
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
setAttr bit (Attr a) False = Attr (a .&. complement bit)
setAttr bit (Attr a) True  = Attr (a .|.            bit)

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

sanifyOutput :: String -> String
sanifyOutput = map f . filter (/= '\r') where
    f c | isPrint c  = c
    f c = '~'

#if defined(CF_WCHAR_SUPPORT) && defined(HAVE_WADDNWSTR)

--wAddStr :: Window -> String -> IO ()
--wAddStr w str = throwIfErr_ ("waddnwstr: " ++ show str) $ withCWStringLen (sanifyOutput str) (\(ws,len) -> waddnwstr w ws (fi len))
    
foreign import ccall unsafe waddnwstr :: Window -> CWString -> CInt -> IO CInt
foreign import ccall unsafe waddch :: Window -> (#type chtype) -> IO CInt

wAddStr :: Window -> String -> IO ()
wAddStr win str = do
    let
        convStr f = case f [] of
            [] -> return ()
            s  -> throwIfErr_ "waddnstr" $
                withCWStringLen  (sanifyOutput s) (\(ws,len) ->  (waddnwstr win ws (fi len)))
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


foreign import ccall unsafe waddnstr :: Window -> CString -> CInt -> IO CInt
foreign import ccall unsafe waddch :: Window -> (#type chtype) -> IO CInt

wAddStr :: Window -> String -> IO ()
wAddStr win str = do
    let
        convStr f = case f [] of
            [] -> return ()
            s  -> throwIfErr_ "waddnstr" $
                withLCStringLen  (sanifyOutput s) (\(ws,len) ->  (waddnstr win ws (fi len)))
        loop []        acc = convStr acc
        loop (ch:str') acc = recognize
            ch
            (loop str' (acc . (ch:)))
            (\ch' -> do
                convStr acc
                throwIfErr "waddch" $ waddch win ch'
                loop str' id)
    loop str id 
#endif
{-

wAddStr :: Window -> String -> IO ()
wAddStr w str =  withLCStringLen (sanifyOutput str) (\(ws,len) -> throwIfErr_ ("waddnstr: " ++ show len ++ " " ++ show str) $ waddnstr w ws (fi len))
foreign import ccall unsafe waddch :: Window -> (#type chtype) -> IO CInt

wAddStr :: Window -> String -> IO ()
wAddStr win str = do
    let
        convStr f = case f [] of
            [] -> return ()
            s  -> throwIfErr_ "waddnstr" $
                withLCString  (sanifyOutput s) (\(ws,len) ->  (waddnstr win ws (fi len)))
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

move :: Int -> Int -> IO ()
move y x =
    throwIfErr_ "move" $ move_c (fromIntegral y) (fromIntegral x)
foreign import ccall unsafe "move" move_c :: CInt -> CInt -> IO CInt

wMove :: Window -> Int -> Int -> IO ()
wMove w y x = throwIfErr_ "wmove" $ wmove w (fi y) (fi x)
foreign import ccall unsafe  wmove :: Window -> CInt -> CInt -> IO CInt

------------------
-- Cursor routines
------------------

data CursorVisibility = CursorInvisible | CursorVisible | CursorVeryVisible

vis_c :: CursorVisibility -> CInt
vis_c vis = case vis of
    CursorInvisible   -> 0
    CursorVisible     -> 1
    CursorVeryVisible -> 2
    
foreign import ccall unsafe "my_curses.h curs_set" curs_set :: CInt -> IO CInt

cursSet 0 = leaveOk True >> curs_set 0
cursSet n = leaveOk False >> curs_set n 

withCursor :: CursorVisibility -> IO a -> IO a
withCursor nv action = Control.Exception.bracket (cursSet (vis_c nv)) (\v -> case v of 
		(#const ERR) -> return 0
		x -> cursSet x) (\_ -> action)

foreign import ccall unsafe "nomacro.h nomacro_getyx" nomacro_getyx :: Window -> Ptr CInt -> Ptr CInt -> IO ()
getYX :: Window -> IO (Int, Int)
getYX w =  alloca $ \py -> alloca $ \px -> (nomacro_getyx w py px)
           >> (peek py >>=  \y -> peek px >>=  \x -> return (fromIntegral y, fromIntegral x))

------------------------------------------------------------------------


touchWin :: Window -> IO ()
touchWin w = throwIfErr_ "touchwin" $ touchwin w
foreign import ccall touchwin :: Window -> IO CInt

newPad :: Int -> Int -> IO Window
newPad nlines ncols = throwIfNull "newpad" $ newpad (fromIntegral nlines) (fromIntegral ncols)

pRefresh :: Window -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
pRefresh pad pminrow pmincol sminrow smincol smaxrow smaxcol = throwIfErr_ "prefresh" $
    prefresh pad (fromIntegral pminrow) (fromIntegral pmincol) (fromIntegral sminrow) (fromIntegral smincol) (fromIntegral smaxrow) (fromIntegral smaxcol)

delWin :: Window -> IO ()
delWin w = throwIfErr_ "delwin" $ delwin w
    
foreign import ccall unsafe prefresh :: Window -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe newpad :: CInt -> CInt -> IO Window
foreign import ccall unsafe delwin :: Window -> IO CInt


newWin :: Int -> Int -> Int -> Int -> IO Window
newWin nlines ncolumn begin_y begin_x = throwIfNull "newwin" $ newwin (fi nlines) (fi ncolumn) (fi begin_y) (fi begin_x)

foreign import ccall unsafe newwin :: CInt -> CInt -> CInt -> CInt -> IO Window


wClrToEol :: Window -> IO ()
wClrToEol w = throwIfErr_ "wclrtoeol" $ wclrtoeol w
foreign import ccall unsafe wclrtoeol :: Window -> IO CInt




foreign import ccall threadsafe getch :: IO CInt


--foreign import ccall unsafe def_prog_mode :: IO CInt
--foreign import ccall unsafe reset_prog_mode :: IO CInt
foreign import ccall unsafe flushinp :: IO CInt


withProgram :: IO a -> IO a
withProgram action = withCursor CursorVisible $ Control.Exception.bracket_ (endWin) (flushinp) action
--withProgram action = withCursor CursorVisible $ Control.Exception.bracket_ ({-def_prog_mode >> -}endWin) (return ()){-reset_prog_mode-} action


foreign import ccall unsafe "my_curses.h beep" c_beep :: IO CInt
foreign import ccall unsafe "my_curses.h flash" c_flash :: IO CInt

beep :: IO ()
beep = do
    br <- c_beep
    when (br /= (#const OK)) (c_flash >> return ()) 


---------------
-- Key Routines
---------------

data Key
    = KeyChar Char | KeyBreak | KeyDown | KeyUp | KeyLeft | KeyRight
    | KeyHome | KeyBackspace | KeyF Int | KeyDL | KeyIL | KeyDC
    | KeyIC | KeyEIC | KeyClear | KeyEOS | KeyEOL | KeySF | KeySR
    | KeyNPage | KeyPPage | KeySTab | KeyCTab | KeyCATab | KeyEnter
    | KeySReset | KeyReset | KeyPrint | KeyLL | KeyA1 | KeyA3
    | KeyB2 | KeyC1 | KeyC3 | KeyBTab | KeyBeg | KeyCancel | KeyClose
    | KeyCommand | KeyCopy | KeyCreate | KeyEnd | KeyExit | KeyFind
    | KeyHelp | KeyMark | KeyMessage | KeyMove | KeyNext | KeyOpen
    | KeyOptions | KeyPrevious | KeyRedo | KeyReference | KeyRefresh
    | KeyReplace | KeyRestart | KeyResume | KeySave | KeySBeg
    | KeySCancel | KeySCommand | KeySCopy | KeySCreate | KeySDC
    | KeySDL | KeySelect | KeySEnd | KeySEOL | KeySExit | KeySFind
    | KeySHelp | KeySHome | KeySIC | KeySLeft | KeySMessage | KeySMove
    | KeySNext | KeySOptions | KeySPrevious | KeySPrint | KeySRedo
    | KeySReplace | KeySRight | KeySRsume | KeySSave | KeySSuspend
    | KeySUndo | KeySuspend | KeyUndo | KeyResize | KeyMouse | KeyUnknown Int
    deriving (Eq,Show,Ord)

decodeKey :: CInt -> Key
decodeKey key = case key of
    _ | key >= 0 && key <= 255 -> KeyChar (chr (fromIntegral key))
    (#const KEY_BREAK)         -> KeyBreak
    (#const KEY_DOWN)          -> KeyDown
    (#const KEY_UP)            -> KeyUp
    (#const KEY_LEFT)          -> KeyLeft
    (#const KEY_RIGHT)         -> KeyRight
    (#const KEY_HOME)          -> KeyHome
    (#const KEY_BACKSPACE)     -> KeyBackspace
    _ | key >= (#const KEY_F0) && key <= (#const KEY_F(63))
                               -> KeyF (fromIntegral (key - #const KEY_F0))
    (#const KEY_DL)            -> KeyDL
    (#const KEY_IL)            -> KeyIL
    (#const KEY_DC)            -> KeyDC
    (#const KEY_IC)            -> KeyIC
    (#const KEY_EIC)           -> KeyEIC
    (#const KEY_CLEAR)         -> KeyClear
    (#const KEY_EOS)           -> KeyEOS
    (#const KEY_EOL)           -> KeyEOL
    (#const KEY_SF)            -> KeySF
    (#const KEY_SR)            -> KeySR
    (#const KEY_NPAGE)         -> KeyNPage
    (#const KEY_PPAGE)         -> KeyPPage
    (#const KEY_STAB)          -> KeySTab
    (#const KEY_CTAB)          -> KeyCTab
    (#const KEY_CATAB)         -> KeyCATab
    (#const KEY_ENTER)         -> KeyEnter
    (#const KEY_SRESET)        -> KeySReset
    (#const KEY_RESET)         -> KeyReset
    (#const KEY_PRINT)         -> KeyPrint
    (#const KEY_LL)            -> KeyLL
    (#const KEY_A1)            -> KeyA1
    (#const KEY_A3)            -> KeyA3
    (#const KEY_B2)            -> KeyB2
    (#const KEY_C1)            -> KeyC1
    (#const KEY_C3)            -> KeyC3
    (#const KEY_BTAB)          -> KeyBTab
    (#const KEY_BEG)           -> KeyBeg
    (#const KEY_CANCEL)        -> KeyCancel
    (#const KEY_CLOSE)         -> KeyClose
    (#const KEY_COMMAND)       -> KeyCommand
    (#const KEY_COPY)          -> KeyCopy
    (#const KEY_CREATE)        -> KeyCreate
    (#const KEY_END)           -> KeyEnd
    (#const KEY_EXIT)          -> KeyExit
    (#const KEY_FIND)          -> KeyFind
    (#const KEY_HELP)          -> KeyHelp
    (#const KEY_MARK)          -> KeyMark
    (#const KEY_MESSAGE)       -> KeyMessage
    (#const KEY_MOVE)          -> KeyMove
    (#const KEY_NEXT)          -> KeyNext
    (#const KEY_OPEN)          -> KeyOpen
    (#const KEY_OPTIONS)       -> KeyOptions
    (#const KEY_PREVIOUS)      -> KeyPrevious
    (#const KEY_REDO)          -> KeyRedo
    (#const KEY_REFERENCE)     -> KeyReference
    (#const KEY_REFRESH)       -> KeyRefresh
    (#const KEY_REPLACE)       -> KeyReplace
    (#const KEY_RESTART)       -> KeyRestart
    (#const KEY_RESUME)        -> KeyResume
    (#const KEY_SAVE)          -> KeySave
    (#const KEY_SBEG)          -> KeySBeg
    (#const KEY_SCANCEL)       -> KeySCancel
    (#const KEY_SCOMMAND)      -> KeySCommand
    (#const KEY_SCOPY)         -> KeySCopy
    (#const KEY_SCREATE)       -> KeySCreate
    (#const KEY_SDC)           -> KeySDC
    (#const KEY_SDL)           -> KeySDL
    (#const KEY_SELECT)        -> KeySelect
    (#const KEY_SEND)          -> KeySEnd
    (#const KEY_SEOL)          -> KeySEOL
    (#const KEY_SEXIT)         -> KeySExit
    (#const KEY_SFIND)         -> KeySFind
    (#const KEY_SHELP)         -> KeySHelp
    (#const KEY_SHOME)         -> KeySHome
    (#const KEY_SIC)           -> KeySIC
    (#const KEY_SLEFT)         -> KeySLeft
    (#const KEY_SMESSAGE)      -> KeySMessage
    (#const KEY_SMOVE)         -> KeySMove
    (#const KEY_SNEXT)         -> KeySNext
    (#const KEY_SOPTIONS)      -> KeySOptions
    (#const KEY_SPREVIOUS)     -> KeySPrevious
    (#const KEY_SPRINT)        -> KeySPrint
    (#const KEY_SREDO)         -> KeySRedo
    (#const KEY_SREPLACE)      -> KeySReplace
    (#const KEY_SRIGHT)        -> KeySRight
    (#const KEY_SRSUME)        -> KeySRsume
    (#const KEY_SSAVE)         -> KeySSave
    (#const KEY_SSUSPEND)      -> KeySSuspend
    (#const KEY_SUNDO)         -> KeySUndo
    (#const KEY_SUSPEND)       -> KeySuspend
    (#const KEY_UNDO)          -> KeyUndo
#ifdef KEY_RESIZE
    (#const KEY_RESIZE)        -> KeyResize
#endif
#ifdef KEY_MOUSE
    (#const KEY_MOUSE)        -> KeyMouse
#endif
    _                          -> KeyUnknown (fromIntegral key)





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
-- 	(#const ERR) -> yield >> getCh 
-- 	x -> return $ decodeKey x

getCh :: IO (Maybe Key)
getCh = do
    v <- getch
    return $ case v of
                 (#const ERR) -> Nothing
                 k_ -> Just $ decodeKey k_

resizeTerminal :: Int -> Int -> IO ()

#ifdef HAVE_RESIZETERM

resizeTerminal a b = throwIfErr_ "resizeterm"  $ resizeterm (fi a) (fi b)

foreign import ccall unsafe "my_curses.h resizeterm" resizeterm :: CInt -> CInt -> IO CInt

#else

resizeTerminal _ _ = return ()

#endif


cursesSigWinch :: Maybe Signal

#ifdef SIGWINCH

cursesSigWinch = Just (#const SIGWINCH)

#else

cursesSigWinch = Nothing

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

foreign import ccall unsafe "my_curses.h mousemask" mousemask :: (#type mmask_t) -> Ptr (#type mmask_t) -> IO (#type mmask_t)

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

-- #if defined(__STDC_ISO_10646__)  && defined(HAVE_WADDNWSTR)
-- #else 

#if 1

recognize :: Char -> IO a -> ((#type chtype) -> IO a) -> IO a
recognize ch noConvert convert
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

#endif 

-------------------------
-- code graveyard
-------------------------

 


#if 0

addStr :: String -> IO ()
addStr str =
    throwIfErr_ "addstr" $
    withCStringConv (readIORef cursesOutConv) str addstr
foreign import ccall unsafe addstr :: Ptr CChar -> IO CInt

addStrLn :: Strin -> IO ()
addStrLn str = do addStr str; addLn

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

#endif
