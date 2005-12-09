--
-- Copyright (c) 2002-2004 John Meacham (john at repetae dot net)
-- Copyright (c) 2004-2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

--
-- | Binding to the [wn]curses library. From the ncurses man page:
--
-- >      The curses library routines give the user a terminal-inde-
-- >      pendent method of updating character screens with  reason-
-- >      able  optimization.
-- 
-- Sections of the quoted documentation are from the OpenBSD man pages,
-- which are distributed under a BSD license.
--
-- A useful reference is: 
--        /Writing Programs with NCURSES/, by Eric S. Raymond and Zeyd
--        M. Ben-Halim, <http://dickey.his.com/ncurses/>
--
-- attrs dont work with Irix curses.h. This should be fixed.
--

#include "YiUtils.h"

module Yi.Curses (

    initCurses,     -- :: IO () -> IO ()
    resetParams,    -- :: IO ()

    stdScr,         -- :: Window
    endWin,         -- :: IO ()

    keypad,         -- :: Window -> Bool -> IO ()
    scrSize,        -- :: IO (Int, Int)
    refresh,        -- :: IO ()
    getCh,          -- :: IO Char

    -- * Line drawing
    waddnstr,       -- :: Window -> CString -> CInt -> IO CInt
    bkgrndSet,      -- :: Attr -> Pair -> IO ()
    clrToEol,       -- :: IO ()
    wMove,          -- :: Window -> Int -> Int -> IO ()

    -- * Key codes
    keyBreak, keyDown, keyUp, keyLeft, keyRight, keyHome,
    keyBackspace, keyDL, keyIL, keyDC, keyIC, keyEIC, keyClear,
    keyEOS, keyEOL, keySF, keySR, keyNPage, keyPPage, keySTab,
    keyCTab, keyCATab, keyEnter, keySReset, keyReset, keyPrint,
    keyLL, keyA1, keyA3, keyB2, keyC1, keyC3, keyBTab, keyBeg,
    keyCancel, keyClose, keyCommand, keyCopy, keyCreate, keyEnd,
    keyExit, keyFind, keyHelp, keyMark, keyMessage, keyMove, keyNext,
    keyOpen, keyOptions, keyPrevious, keyRedo, keyReference, keyRefresh,
    keyReplace, keyRestart, keyResume, keySave, keySBeg, keySCancel,
    keySCommand, keySCopy, keySCreate, keySDC, keySDL, keySelect, keySEnd,
    keySEOL, keySExit, keySFind, keySHelp, keySHome, keySIC, keySLeft,
    keySMessage, keySMove, keySNext, keySOptions, keySPrevious, keySPrint,
    keySRedo, keySReplace, keySRight, keySRsume, keySSave, keySSuspend,
    keySUndo, keySuspend, keyUndo,
#ifdef KEY_RESIZE
    keyResize,
#endif

    -- * Cursor
    CursorVisibility(..),
    cursSet,        -- :: CInt -> IO CInt
    getYX,          -- :: Window -> IO (Int, Int)
    withCursor,     -- :: CursorVisibility -> IO a -> IO a

    -- * Colours
    Pair(..), Color,
    initPair,           -- :: Pair -> Color -> Color -> IO ()
    color,              -- :: String -> Maybe Color
    hasColors,          -- :: IO Bool

    -- * Attributes
    Attr,
    attr0, setBold, setReverse,
    attrSet,
    attrPlus,           -- :: Attr -> Attr -> Attr

    -- * error handling
    throwIfErr_,    -- :: Num a => String -> IO a -> IO ()
    
  ) where 

#if HAVE_SIGNAL_H
# include <signal.h>
#endif

import qualified Data.FastPackedString as P

import Prelude hiding       (pi)
import Data.Char            (ord, chr)

import Control.Monad        (liftM, when)
import Control.Concurrent   (yield, threadWaitRead)
import Control.Exception    (bracket)

import Foreign.C.Types      (CInt, CShort)
import Foreign.C.String     (CString)
import Foreign

#ifdef SIGWINCH
import System.Posix.Signals (installHandler, Signal, Handler(Catch))
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
    resetParams
#ifdef SIGWINCH
    -- does this still work?
    installHandler cursesSigWinch (Catch fn) Nothing >> return ()
#endif

-- | A bunch of settings we need
--
resetParams :: IO ()
resetParams = do
    cBreak True
    echo False          -- don't echo to the screen
    nl True             -- always translate enter to \n
    leaveOk True        -- not ok to leave cursor wherever it is
    meta stdScr True    -- ask for 8 bit chars, so we can get Meta
    keypad stdScr True  -- enable the keypad, so things like ^L (refresh) work
    noDelay stdScr False  -- blocking getCh, no #ERR
    return ()

-- not needed, if keypad is True:
--  defineKey (#const KEY_UP) "\x1b[1;2A"
--  defineKey (#const KEY_DOWN) "\x1b[1;2B"
--  defineKey (#const KEY_SLEFT) "\x1b[1;2D"
--  defineKey (#const KEY_SRIGHT) "\x1b[1;2C"

------------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

------------------------------------------------------------------------
-- 
-- Error handling, packed to save on all those strings
--

-- | Like throwIf, but for packed error messages
throwPackedIf :: (a -> Bool) -> P.FastString -> (IO a) -> (IO a)
throwPackedIf p msg action = do
    v <- action
    if p v then (fail . P.unpack $ msg) else return v
{-# INLINE throwPackedIf #-}

-- | Arbitrary test 
throwIfErr :: Num a => P.FastString -> IO a -> IO a
throwIfErr = throwPackedIf (== (#const ERR))
{-# INLINE throwIfErr #-}

-- | Discard result
throwIfErr_ :: Num a => P.FastString -> IO a -> IO ()
throwIfErr_ a b = void $ throwIfErr a b
{-# INLINE throwIfErr_ #-}

-- | packed throwIfNull
throwPackedIfNull :: P.FastString -> IO (Ptr a) -> IO (Ptr a)
throwPackedIfNull = throwPackedIf (== nullPtr)
{-# INLINE throwPackedIfNull #-}

------------------------------------------------------------------------

type WindowTag = ()
type Window = Ptr WindowTag

--
-- | The standard screen
--
stdScr :: Window
stdScr = unsafePerformIO (peek stdscr)

foreign import ccall "static &stdscr" 
    stdscr :: Ptr Window

--
-- | initscr is normally the first curses routine to call when
-- initializing a program. curs_initscr(3):
--
-- > To initialize the routines, the routine initscr or newterm
-- > must be called before any of the other routines that  deal
-- > with  windows  and  screens  are used. 
--
-- > The initscr code determines the terminal type and initial-
-- > izes all curses data structures.  initscr also causes  the
-- > first  call  to  refresh  to  clear the screen.  If errors
-- > occur, initscr writes  an  appropriate  error  message  to
-- > standard error and exits; otherwise, a pointer is returned
-- > to stdscr.
--
initScr :: IO Window
initScr = throwPackedIfNull (P.packAddress "initscr"##) c_initscr

foreign import ccall unsafe "initscr" 
    c_initscr :: IO Window

--
-- |> The cbreak routine
-- > disables line buffering and erase/kill  character-process-
-- > ing  (interrupt  and  flow  control  characters  are unaf-
-- > fected), making characters typed by the  user  immediately
-- > available  to  the  program.  The nocbreak routine returns
-- > the terminal to normal (cooked) mode.
--
cBreak :: Bool -> IO ()
cBreak True  = throwIfErr_ (P.packAddress "cbreak"##)   cbreak
cBreak False = throwIfErr_ (P.packAddress "nocbreak"##) nocbreak

foreign import ccall unsafe "cbreak"     cbreak :: IO CInt
foreign import ccall unsafe "nocbreak" nocbreak :: IO CInt

--
-- |> The  echo  and  noecho routines control whether characters
-- > typed by the user are echoed by getch as they  are  typed.
-- > Echoing  by  the  tty  driver is always disabled, but ini-
-- > tially getch is in echo  mode,  so  characters  typed  are
-- > echoed.  Authors of most interactive programs prefer to do
-- > their own echoing in a controlled area of the  screen,  or
-- > not  to  echo  at  all, so they disable echoing by calling
-- > noecho.  [See curs_getch(3) for a discussion of how  these
-- > routines interact with cbreak and nocbreak.]
--
echo :: Bool -> IO ()
echo False = throwIfErr_ (P.packAddress "noecho"##) noecho
echo True  = throwIfErr_ (P.packAddress "echo"##)   echo_c

foreign import ccall unsafe "noecho" noecho :: IO CInt
foreign import ccall unsafe "echo"   echo_c :: IO CInt

--
-- |> The  nl  and  nonl routines control whether the underlying
-- > display device translates the return key into  newline  on
-- > input,  and  whether it translates newline into return and
-- > line-feed on output (in either case, the call  addch('\n')
-- > does the equivalent of return and line feed on the virtual
-- > screen).  Initially, these translations do occur.  If  you
-- > disable  them using nonl, curses will be able to make bet-
-- > ter use of the line-feed capability, resulting  in  faster
-- > cursor  motion.   Also, curses will then be able to detect
-- > the return key.
-- > 
nl :: Bool -> IO ()
nl True  = throwIfErr_ (P.packAddress "nl"##) nl_c
nl False = throwIfErr_ (P.packAddress "nonl"##) nonl

foreign import ccall unsafe "nl" nl_c :: IO CInt
foreign import ccall unsafe "nonl" nonl :: IO CInt

--
-- | Enable the keypad of the user's terminal.
--
keypad :: Window -> Bool -> IO ()
keypad win bf = throwIfErr_ (P.packAddress "keypad"##) $ 
    keypad_c win (if bf then 1 else 0)

foreign import ccall unsafe "keypad" 
    keypad_c :: Window -> (#type bool) -> IO CInt

-- |> The nodelay option causes getch to be a non-blocking call.
-- > If  no input is ready, getch returns ERR.  If disabled (bf
-- > is FALSE), getch waits until a key is pressed.
--
noDelay :: Window -> Bool -> IO ()
noDelay win bf = throwIfErr_ (P.packAddress "nodelay"##) $ 
    nodelay win (if bf then 1 else 0)

foreign import ccall unsafe nodelay 
    :: Window -> (#type bool) -> IO CInt

--
-- |> Normally, the hardware cursor is left at the  location  of
-- > the  window  cursor  being  refreshed.  The leaveok option
-- > allows the cursor to be left wherever the  update  happens
-- > to leave it.  It is useful for applications where the cur-
-- > sor is not used, since it  reduces  the  need  for  cursor
-- > motions.   If  possible, the cursor is made invisible when
-- > this option is enabled.
--
leaveOk  :: Bool -> IO CInt
leaveOk bf = leaveok_c stdScr (if bf then 1 else 0)

foreign import ccall unsafe "leaveok" 
    leaveok_c :: Window -> (#type bool) -> IO CInt

------------------------------------------------------------------------

-- | The use_default_colors() and assume_default_colors() func-
--   tions are extensions to the curses library.  They are used
--   with terminals that support ISO 6429 color, or equivalent.
--
--  use_default_colors() tells the  curses library  to  assign terminal
--  default foreground/background colors to color number  -1.
--
#if defined(HAVE_USE_DEFAULT_COLORS)
foreign import ccall unsafe "use_default_colors" 
    useDefaultColors :: IO ()
#else
useDefaultColors :: IO ()
useDefaultColors = return ()
#endif

------------------------------------------------------------------------

--
-- |> The program must call endwin for each terminal being used before
-- > exiting from curses.
--
endWin :: IO ()
endWin = throwIfErr_ (P.packAddress "endwin"##) endwin

foreign import ccall unsafe "endwin" 
    endwin :: IO CInt

------------------------------------------------------------------------

--
-- | get the dimensions of the screen
--
scrSize :: IO (Int, Int)
scrSize = do
    lnes <- peek linesPtr
    cols <- peek colsPtr
    return (fi lnes, fi cols)

foreign import ccall "&LINES" linesPtr :: Ptr CInt
foreign import ccall "&COLS"  colsPtr  :: Ptr CInt

--
-- | refresh curses windows and lines. curs_refresh(3)
--
refresh :: IO ()
refresh = throwIfErr_ (P.packAddress "refresh"##) refresh_c

foreign import ccall unsafe "refresh" 
    refresh_c :: IO CInt

------------------------------------------------------------------------

hasColors :: IO Bool
hasColors = liftM (/= 0) has_colors

foreign import ccall unsafe "has_colors" 
    has_colors :: IO (#type bool)

--
-- | Initialise the color settings, also sets the screen to the
-- default colors (white on black)
--
startColor :: IO ()
startColor = throwIfErr_ (P.packAddress "start_color"##) start_color

foreign import ccall unsafe start_color :: IO CInt

newtype Pair  = Pair Int
newtype Color = Color Int

color :: String -> Maybe Color
#if defined(HAVE_USE_DEFAULT_COLORS)
color "default"  = Just $ Color (-1)
#endif
color "black"    = Just $ Color (#const COLOR_BLACK)
color "red"      = Just $ Color (#const COLOR_RED)
color "green"    = Just $ Color (#const COLOR_GREEN)
color "yellow"   = Just $ Color (#const COLOR_YELLOW)
color "blue"     = Just $ Color (#const COLOR_BLUE)
color "magenta"  = Just $ Color (#const COLOR_MAGENTA)
color "cyan"     = Just $ Color (#const COLOR_CYAN)
color "white"    = Just $ Color (#const COLOR_WHITE)
color _          = Just $ Color (#const COLOR_BLACK)    -- NB

--
-- |> curses support color attributes  on  terminals  with  that
-- > capability.   To  use  these  routines start_color must be
-- > called, usually right after initscr.   Colors  are  always
-- > used  in pairs (referred to as color-pairs).  A color-pair
-- > consists of a foreground  color  (for  characters)  and  a
-- > background color (for the blank field on which the charac-
-- > ters are displayed).  A programmer  initializes  a  color-
-- > pair  with  the routine init_pair.  After it has been ini-
-- > tialized, COLOR_PAIR(n), a macro  defined  in  <curses.h>,
-- > can be used as a new video attribute.
--
-- > If  a  terminal  is capable of redefining colors, the pro-
-- > grammer can use the routine init_color to change the defi-
-- > nition   of   a   color.
--
-- > The init_pair routine changes the definition of  a  color-
-- > pair.   It takes three arguments: the number of the color-
-- > pair to be changed, the foreground color number,  and  the
-- > background color number.  For portable applications:
--
-- > -  The value of the first argument must be between 1 and
-- >    COLOR_PAIRS-1.
--
-- > -  The value of the second and third arguments  must  be
-- >    between  0  and  COLORS (the 0 color pair is wired to
-- >    white on black and cannot be changed).
--
--
initPair :: Pair -> Color -> Color -> IO ()
initPair (Pair p) (Color f) (Color b) =
    throwIfErr_ (P.packAddress "init_pair"##) $
        init_pair (fi p) (fi f) (fi b)

foreign import ccall unsafe 
    init_pair :: CShort -> CShort -> CShort -> IO CInt

-- ---------------------------------------------------------------------
-- Attributes. Keep this as simple as possible for maximum portability

foreign import ccall unsafe "attrset"
    c_attrset :: CInt -> IO CInt

attrSet :: Attr -> Pair -> IO ()
attrSet (Attr attr) (Pair p) = do
    throwIfErr_ (P.packAddress "attrset"##)   $ c_attrset (attr .|. fi (colorPair p))

------------------------------------------------------------------------

newtype Attr = Attr CInt

attr0   :: Attr
attr0   = Attr (#const A_NORMAL)

setBold :: Attr -> Bool -> Attr
setBold = setAttr (Attr #const A_BOLD)

setReverse :: Attr -> Bool -> Attr
setReverse = setAttr (Attr #const A_REVERSE)

-- | bitwise combination of attributes
setAttr :: Attr -> Attr -> Bool -> Attr
setAttr (Attr b) (Attr a) False = Attr (a .&. complement b)
setAttr (Attr b) (Attr a) True  = Attr (a .|.            b)

attrPlus :: Attr -> Attr -> Attr
attrPlus (Attr a) (Attr b) = Attr (a .|. b)

------------------------------------------------------------------------

#let translate_attr attr =                              \
    "(if a .&. %lu /= 0 then %lu else 0) .|.",          \
    (unsigned long) A_##attr, (unsigned long) A_##attr

bkgrndSet :: Attr -> Pair -> IO ()
bkgrndSet (Attr a) (Pair p) = bkgdset $
    fi (ord ' ') .|.
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

foreign import ccall unsafe "get_color_pair" 
    colorPair :: Int -> (#type chtype)

foreign import ccall unsafe bkgdset :: (#type chtype) -> IO ()

------------------------------------------------------------------------

foreign import ccall threadsafe
    waddnstr :: Window -> CString -> CInt -> IO CInt

clrToEol :: IO ()
clrToEol = throwIfErr_ (P.packAddress "clrtoeol"##) c_clrtoeol

foreign import ccall unsafe "clrtoeol" c_clrtoeol :: IO CInt

--
-- | >    move the cursor associated with the window
--   >    to line y and column x.  This routine does  not  move  the
--   >    physical  cursor  of the terminal until refresh is called.
--   >    The position specified is relative to the upper  left-hand
--   >    corner of the window, which is (0,0).
--
wMove :: Window -> Int -> Int -> IO ()
wMove w y x = throwIfErr_ (P.packAddress "wmove"##) $ wmove w (fi y) (fi x)

foreign import ccall unsafe  
    wmove :: Window -> CInt -> CInt -> IO CInt

-- ---------------------------------------------------------------------
-- Cursor routines

data CursorVisibility = CursorInvisible | CursorVisible | CursorVeryVisible

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

foreign import ccall unsafe "curs_set" 
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

vis_c :: CursorVisibility -> CInt
vis_c vis = case vis of
    CursorInvisible   -> 0
    CursorVisible     -> 1
    CursorVeryVisible -> 2

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
            return (fi y, fi x)

--
-- | Get the current cursor coords, written into the two argument ints.
--
-- >    The getyx macro places the current cursor position of the given
-- >    window in the two integer variables y and x.
--
--      void getyx(WINDOW *win, int y, int x);
--
foreign import ccall unsafe "nomacro_getyx" 
        nomacro_getyx :: Window -> Ptr CInt -> Ptr CInt -> IO ()

--
-- | >      The getch, wgetch, mvgetch and mvwgetch, routines read a
--   >      character  from the window.
--
foreign import ccall threadsafe getch :: IO CInt

------------------------------------------------------------------------
--
-- | Map curses keys to real chars. The lexer will like this.
--
decodeKey :: CInt -> Char
decodeKey = chr . fi
{-# INLINE decodeKey #-}

--
-- | Some constants for easy symbolic manipulation.
-- NB we don't map keys to an abstract type anymore, as we can't use
-- Alex lexers then.
--
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

-- ---------------------------------------------------------------------
-- try to set the upper bits

meta :: Window -> Bool -> IO ()
meta win bf = throwIfErr_ (P.packAddress "meta"##) $
    c_meta win (if bf then 1 else 0)

foreign import ccall unsafe "meta" 
    c_meta :: Window -> CInt -> IO CInt

------------------------------------------------------------------------
--
-- | read a character from the window
--
-- When 'ESC' followed by another key is pressed before the ESC timeout,
-- that second character is not returned until a third character is
-- pressed. wtimeout, nodelay and timeout don't appear to change this
-- behaviour.
-- 
-- On emacs, we really would want Alt to be our meta key, I think.
--
-- Be warned, getCh will block the whole process without noDelay
--
getCh :: IO Char
getCh = do
    threadWaitRead 0
    v <- getch
    case v of
        (#const ERR) -> yield >> getCh
        x            -> return $ decodeKey x

------------------------------------------------------------------------

#ifdef SIGWINCH
cursesSigWinch :: Signal
cursesSigWinch = #const SIGWINCH
#endif

