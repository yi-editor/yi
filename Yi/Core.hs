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
--       Copyright (c) Tuomo Valkonen 2004.
-- 
-- Released under the same license.
-- 

--
-- | The core actions of yi. This module is the link between the editor
-- machine defined in 'Yi.Editor', and the real world defined in
-- 'Yi.UI'. The instructions defined here manipulate the editor state,
-- and control the screen through the UI.  Key bindings, and libraries
-- should manipulate Yi through the interface defined here.
--

module Yi.Core {-(

        -- * Type of core functions
        Action,

        -- * Construction and destruction
        startE,
        endE,

        -- * Getting down to business (soon to be replaced by key lexer) 
        eventLoop,

        -- * Editor actions

        -- ** Global editor actions
        quitE,
        refreshE,
        nopE,
        getcE,
        msgE,
        msgClrE,
        infoE,

        -- ** Window manipulation
        nextE,
        prevE,

        -- ** File-based actions
        fnewE,
        freadE,
        fwriteE,

        -- ** Buffer point movement
        leftE,
        rightE,
        solE,
        eolE,
        downE,
        upE,
        leftOrSolE,
        rightOrEolE,
        topE,
        botE,

        -- ** Buffer editing
        readE,
        writeE,
        insertE,
        deleteE,
        killE,

        -- ** For now, export the symbolic key names from the ui
        module Yi.UI

   )-} where

import Yi.MkTemp
import Yi.Buffer
import Yi.Window
import Yi.Regex

import Yi.UI
import qualified Yi.UI     as UI

import Yi.Editor
import qualified Yi.Editor as Editor

import Data.Maybe
import Data.Char            ( isLatin1 )
import System.IO
import System.Directory     ( doesFileExist )
import System.Exit
import Control.Monad
import Control.Exception    ( ioErrors, catchJust, handleJust )

import GHC.Base

-- ---------------------------------------------------------------------
-- | The type of user-bindable functions
--
type Action = IO ()

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
-- TODO should be in keybind
--
startE :: Editor.Config -> Maybe [FilePath] -> IO ()
startE confs mfs = do
    UI.start
    Editor.setUserSettings confs
    sz <- UI.screenSize
    modifyEditor_ $ \e -> return $ e { scrsize = sz }
    handleJust (ioErrors) (\e -> msgE (show e)) $ do
        case mfs of
            Just fs -> mapM_ fnewE fs
            Nothing -> do               -- vi-like behaviour, just for now.
                mf <- mkstemp "/tmp/yi.XXXXXXXXXX" 
                case mf of
                    Nothing    -> error "Core.startE: mkstemp failed"
                    Just (f,h) -> hClose h >> fnewE f

    refreshE

--
-- | shutdown the editor
--
endE :: IO ()
endE = UI.end

-- ---------------------------------------------------------------------
-- | How to read from standard input
--
getcE :: IO Char
getcE = UI.getKey UI.refresh

-- ---------------------------------------------------------------------
-- | The editor main loop. Read key strokes from the ui and interpret
-- them using the current key map. Keys are bound to core actions.
-- The state is threaded explicitly at the moment.
--
eventLoop :: IO ()
eventLoop = do
    dflt <- Editor.getKeyBinds
    let mainloop km@(Keymap fn) = do 
            c   <- getcE
            km' <- catchJust (ioErrors) (fn c) (handler km)
            UI.refresh
            mainloop km'
    mainloop (Keymap dflt)

    where handler = \km e -> msgE (show e) >> return km

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit
quitE :: Action
quitE = exitWith ExitSuccess

-- | Refresh the screen
refreshE :: Action
refreshE = UI.refresh

-- | Do nothing
nopE :: Action
nopE = return ()

-- ---------------------------------------------------------------------
-- Movement operations

-- | Move cursor to origin
topE :: Action
topE = withWindow_ $ moveToW 0

-- | Move cursor to end of buffer
botE :: Action
botE = withWindow_ $ \w b -> do
            n <- sizeB b
            moveToW (n-1) w b

-- | Move cursor to start of line
solE :: Action
solE = withWindow_ moveToSolW

-- | Move cursor to end of line
eolE :: Action
eolE = withWindow_ moveToEolW

-- | Move cursor down 1 line
downE :: Action
downE = withWindow_ moveDownW

-- | Move cursor up to the same point on the previous line
upE :: Action
upE = withWindow_ moveUpW

-- | Go to line number @n@
gotoLnE :: Int -> Action
gotoLnE n = withWindow_ (gotoLnW n)

-- | Go to line @n@ offset from current line
gotoLnFromE :: Int -> Action
gotoLnFromE n = withWindow_ (gotoLnFromW n)

------------------------------------------------------------------------

-- | Is the point at the start of the line
atSolE :: IO Bool
atSolE = withWindow $ \w b -> atSol b >>= \x -> return (w,x)

-- | Is the point at the end of the line
atEolE :: IO Bool
atEolE = withWindow $ \w b -> atEol b >>= \x -> return (w,x)

-- | Is the point at the start of the file
atSofE :: IO Bool
atSofE = withWindow $ \w b -> atSof b >>= \x -> return (w,x)

-- | Is the point at the end of the file
atEofE :: IO Bool
atEofE = withWindow $ \w b -> atEof b >>= \x -> return (w,x)

------------------------------------------------------------------------

-- | Scroll up 1 screen
upScreenE :: Action
upScreenE = do
    (Just w) <- getWindow
    withWindow_ (gotoLnFromW (- (height w - 1)))
    solE 

-- | Scroll up n screens
upScreensE :: Int -> Action
upScreensE n = do
    (Just w) <- getWindow
    withWindow_ (gotoLnFromW (- (n * (height w - 1))))
    solE 

-- | Scroll down 1 screen
downScreenE :: Action
downScreenE = do
    (Just w) <- getWindow
    withWindow_ (gotoLnFromW (height w - 1))

-- | Scroll down n screens
downScreensE :: Int -> Action
downScreensE n = do
    (Just w) <- getWindow
    withWindow_ (gotoLnFromW (n * (height w - 1)))

-- ---------------------------------------------------------------------

-- | Move left @x@ or to start of line
leftOrSolE :: Int -> Action
leftOrSolE x = withWindow_ $ moveXorSolW x

-- | Move right @x@ or to end of line
rightOrEolE :: Int -> Action
rightOrEolE x = withWindow_ $ moveXorEolW x

-- ---------------------------------------------------------------------
-- Window based operations
--

{-
-- | scroll window up
scrollUpE :: Action
scrollUpE = withWindow_ scrollUpW

-- | scroll window down
scrollDownE :: Action
scrollDownE = withWindow_ scrollDownW
-}

------------------------------------------------------------------------

-- | Insert new character
insertE :: Char -> Action
insertE c = withWindow_ $ insertW c

-- | Delete character under cursor
deleteE :: Action
deleteE = withWindow_ deleteW

-- | Kill to end of line
killE :: Action
killE = withWindow_ deleteToEolW -- >>= Buffer.prevXorLn 1

-- | Read the char under the cursor
readE :: IO Char
readE = withWindow $ \w b -> readB b >>= \c -> return (w,c)

-- | Read the line the cursor is on
readLnE :: IO String
readLnE = withWindow $ \w b -> do
            p <- pointB b
            i <- indexOfSol b 
            j <- indexOfEol b
            s <- nelemsB b (j-i) i
            moveTo b p 
            return (w,s)

-- | Write char to point
writeE :: Char -> Action
writeE c = withWindow_ $ \w b -> do
            if isLatin1 c then writeB b c else nopE -- TODO
            return w

-- ---------------------------------------------------------------------
-- registers

-- | Put string into yank register
setRegE :: String -> Action
setRegE s = modifyEditor_ $ \e -> return e { yreg = s }

-- | Return the contents of the yank register
getRegE :: IO String
getRegE = readEditor yreg

-- | Put regex into regex 'register'
setRegexE :: Regex -> Action
setRegexE re = modifyEditor_ $ \e -> return e { regex = Just re }

-- Return contents of regex register
getRegexE :: IO (Maybe Regex)
getRegexE = readEditor regex
 
-- ---------------------------------------------------------------------

-- | Global searching. Search for regex and move point to that position.
-- Nothing means reuse the last regular expression. Just s means use @s@
-- as the new regular expression.
--
searchE :: (Maybe String) -> Action
searchE (Just []) = nopE
searchE (Just re) = regcomp re regExtended >>= searchE'
searchE Nothing   = do 
    mre <- getRegexE 
    case mre of
        Nothing -> nopE
        Just re -> searchE' re

-- Internal.
searchE' :: Regex -> Action
searchE' c_re = do
    setRegexE c_re      -- store away for later use
    mp <- withWindow $ \w b -> do
            rightB b
            mp <- regexB b c_re
            when (isNothing mp) (leftB b)   -- go home
            return (w,mp)
    case mp of
        Just (p,_) -> withWindow_ $ moveToW p
        Nothing    -> msgE "Pattern not found"  -- TODO

------------------------------------------------------------------------

-- | Search and replace /on current line/. Should return status.
-- TODO too complex.
--
searchAndRepLocal :: String -> String -> Action
searchAndRepLocal [] _ = nopE
searchAndRepLocal re str = do
    c_re <- regcomp re regExtended
    setRegexE c_re      -- store away for later use

    mp <- withWindow $ \w b -> do   -- find the regex
            mp <- regexB b c_re
            return (w, mp)
    status <- case mp of
        Just (i,j) -> withWindow $ \w b -> do
                p  <- pointB b      -- all buffer-level atm
                moveToEol b
                ep <- pointB b      -- eol point of current line
                moveTo b i
                moveToEol b
                eq <- pointB b      -- eol of matched line
                moveTo b p          -- go home. sub doesn't move
                if (ep /= eq)       -- then match isn't on current line
                    then return (w, Nothing)
                    else do         -- do the replacement
                moveTo b i
                deleteN b (j - i)
                insertN b str
                moveTo b p          -- and back to where we were!
                return (w, Just ()) -- signal success
        Nothing -> return Nothing

    when (isNothing status) $ 
        msgE ("Pattern not found: "++re) -- TODO vi spec.

------------------------------------------------------------------------

-- | Draw message at bottom of screen
msgE :: String -> Action
msgE s = do modifyEditor_ $ \e -> return e { cmdline = s }
            UI.drawCmdLine s -- immediately draw

-- | Clear the message line at bottom of screen
msgClrE  :: Action
msgClrE = do modifyEditor_ $ \e -> return e { cmdline = [] } 
             UI.drawCmdLine [] -- immediately draw

-- | File info, size in chars, line no, col num, char num, percent 
-- TODO more info, better data structure
bufInfoE :: IO (FilePath, Int, Int, Int, Int, String)
bufInfoE = withWindow $ \w b -> do
    s <- sizeB b
    p <- pointB b
    let x = snd $ cursor w
    return (w, ( nameB b,
                 s, 
                 lineno w, 
                 x+1, 
                 p, 
                 getPercent p s) )

------------------------------------------------------------------------
-- | Window manipulation

-- | edit the next buffer in the buffer list
nextBufW :: Action
nextBufW = do
    w <- getWindow
    b <- Editor.nextBuffer
    deleteWindow w         -- !! don't delete window before getting the next buffer
    w' <- Editor.newWindow b
    Editor.setWindow w'

-- | edit the previous buffer in the buffer list
prevBufW :: Action
prevBufW = do
    b <- Editor.prevBuffer
    getWindow >>= deleteWindow
    w' <- newWindow b
    setWindow w'

-- | Read file into buffer and open up a new window
fnewE  :: FilePath -> Action
fnewE f = do
    e  <- doesFileExist f
    b  <- if e then hNewBuffer f else stringToNewBuffer f []
    getWindow >>= deleteWindow
    w <- newWindow b
    Editor.setWindow w

-- | Write current buffer to disk
fwriteE :: Action
fwriteE = withWindow_ $ \w b -> hPutB b (nameB b) >> return w

-- | Split a second window onto this buffer :)
splitE :: Action
splitE = do
    i     <- sizeWindows
    (y,_) <- screenSize
    let (sy,r) = getY y i
    if sy + r <= 4  -- min window size
        then msgE "Not enough room to split"
        else do
    mw <- getWindow
    case mw of 
        Nothing -> nopE
        Just w  -> do b <- getBufferWith (bufkey w)
                      w' <- newWindow b
                      Editor.setWindow w'

-- | close current window. quit if that is all the windows
closeE :: Action
closeE = do getWindow >>= deleteWindow
            i <- sizeWindows
            if i == 0 then quitE else nopE

-- | Shift focus to next window
nextWinE :: Action
nextWinE = Editor.nextWindow

-- | Shift focus to prev window
prevWinE :: Action
prevWinE = Editor.prevWindow

