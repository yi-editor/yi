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
--       Copyright (c) Tuomo Valkonen 2004.
-- 
-- Released under the same license.
-- 

--
-- | The core actions of yi. This module is the link between the editor
-- and the UI. Key bindings, and libraries should manipulate Yi through
-- the interface defined here.
--

module Yi.Core (

        -- * Construction and destruction
        startE,         -- :: Editor.Config -> Int -> Maybe [FilePath] -> IO ()
        quitE,          -- :: Action
        eventLoop,

        -- * Global editor actions
        getcE,          -- :: IO Char
        nopE,           -- :: Action
        msgE,           -- :: String -> Action
        errorE,         -- :: String -> Action
        msgClrE,        -- :: Action
        getMsgE,        -- :: IO String
        bufInfoE,       -- :: IO (FilePath,Int,Int,Int,Int,String)

        -- * Window manipulation
        nextBufW,       -- :: Action
        prevBufW,       -- :: Action
        nextWinE,       -- :: Action
        prevWinE,       -- :: Action
        splitE,         -- :: Action
        closeE,         -- :: Action

        -- * File-based actions
        fnewE,          -- :: FilePath -> Action
        fwriteE,        -- :: Action

        -- * Buffer point movement
        topE,           -- :: Action
        botE,           -- :: Action
        solE,           -- :: Action
        eolE,           -- :: Action
        downE,          -- :: Action
        upE,            -- :: Action
        leftE,          -- :: Action
        rightE,         -- :: Action
        leftOrSolE,     -- :: Int -> Action
        rightOrEolE,    -- :: Int -> Action
        firstNonSpaceE, -- :: Action
        gotoLnE,        -- :: Int -> Action
        gotoLnFromE,    -- :: Int -> Action
        gotoPointE,     -- :: Int -> Action
        getPointE,      -- :: IO Int

        atSolE,         -- :: IO Bool
        atEolE,         -- :: IO Bool
        atSofE,         -- :: IO Bool
        atEofE,         -- :: IO Bool

        -- * Window-based movement
        upScreenE,      -- :: Action
        upScreensE,     -- :: Int -> Action
        downScreenE,    -- :: Action
        downScreensE,   -- :: Int -> Action
        downFromTosE,   -- :: Int -> Action
        upFromBosE,     -- :: Int -> Action
        middleE,        -- :: Action

        -- * Buffer editing
        insertE,        -- :: Char -> Action
        deleteE,        -- :: Action
        deleteNE,       -- :: Int -> Action
        killE,          -- :: Action
        readE,          -- :: IO Char
        readLnE,        -- :: IO String
        readRestOfLnE,  -- :: IO String
        writeE,         -- :: Char -> Action

        -- * Basic registers
        setRegE,        -- :: String -> Action
        getRegE,        -- :: IO String
        setRegexE,      -- :: Regex -> Action
        getRegexE,      -- :: IO (Maybe Regex)

        -- * Regular expression and searching
        searchE,                -- :: (Maybe String) -> Action
        searchAndRepLocal,      -- :: String -> String -> IO Bool

   ) where

import Yi.MkTemp            ( mkstemp )
import Yi.Buffer
import Yi.Window
import Yi.Regex             ( regcomp, regExtended, Regex )
import Yi.Editor
import qualified Yi.Editor as Editor
import qualified Yi.UI     as UI ( refresh, start, screenSize, getKey, end )

import Data.Maybe           ( isNothing )
import Data.Char            ( isLatin1 )

import System.IO            ( hClose )
import System.Directory     ( doesFileExist )
import System.Exit          ( exitWith, ExitCode(ExitSuccess) )

import Control.Monad
import Control.Exception    ( ioErrors, handleJust )
import Control.Concurrent   ( threadWaitRead, takeMVar, forkIO )
import Control.Concurrent.Chan

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
-- TODO should be in keybind
--
startE :: Editor.Config -> Int -> Maybe [FilePath] -> IO ()
startE confs ln mfs = do
    Editor.setUserSettings confs
    UI.start
    sz <- UI.screenSize
    modifyEditor_ $ \e -> return $ e { scrsize = sz }

    t <- forkIO refreshLoop -- fork UI thread
    modifyEditor_ $ \e -> return $ e { threads = t : threads e }

    -- read in any files
    handleJust ioErrors (errorE . show) $ do
        case mfs of
            Just fs -> mapM_ fnewE fs
            Nothing -> do               -- vi-like behaviour, just for now.
                mf <- mkstemp "/tmp/yi.XXXXXXXXXX" 
                case mf of
                    Nothing    -> error "Core.startE: mkstemp failed"
                    Just (f,h) -> hClose h >> fnewE f
    gotoLnE ln

    -- fork input-reading thread. important to block *thread* on getcE
    -- otherwise all threads will block waiting for input
    ch <- newChan   
    t' <- forkIO $ getcLoop ch
    modifyEditor_ $ \e -> return $ e { threads = t' : threads e, input = ch }

    where
        --
        -- | Action to read characters into a channel
        -- We block our thread waiting for input on stdin
        --
        getcLoop :: Chan Char -> IO ()
        getcLoop ch = repeatM_ $ threadWaitRead 0 >> getcE >>= writeChan ch

        --
        -- | When the editor state isn't being modified, refresh, then wait for
        -- it to be modified again.
        --
        refreshLoop :: IO ()
        refreshLoop = repeatM_ $ takeMVar editorModified >> UI.refresh

-- ---------------------------------------------------------------------
-- | How to read from standard input
--
getcE :: IO Char
getcE = UI.getKey (return ())

-- 
-- | The editor main loop. Read key strokes from the ui and interpret
-- them using the current key map. Keys are bound to core actions.
--
eventLoop :: IO ()
eventLoop = do
    fn <- Editor.getKeyBinds 
    ch <- readEditor input
    repeatM_ $ handleJust ioErrors (errorE . show) $ 
        sequence_ . fn =<< getChanContents ch

-- TODO if there is an exception, the key bindings will be reset...

{-
--
-- Channel-based event loop has exactly the same semantics
--
eventLoop :: IO ()
eventLoop = Editor.getKeyBinds >>= loop
    where 
        loop f = do handleJust ioErrors handler (sequence_ . f =<< lazyRead)
                    loop f
        handler = errorE . show

--
-- | Lazily read all input from the user. A big magic.
-- Identical to getChanContents
--
lazyRead :: IO String
lazyRead = unsafeInterleaveIO $ do 
                c  <- getcE
                cs <- lazyRead
                return (c : cs)
-}

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit
quitE :: Action
quitE = do
    UI.end
    Editor.shutdown
    exitWith ExitSuccess

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

-- | Go to a particular point. ToDo don't reset unless we wander off the screen
gotoPointE :: Int -> Action
gotoPointE p = withWindow_ $ moveToW p

-- | Get the current point
getPointE :: IO Int
getPointE = withWindow $ \w b -> pointB b >>= \p -> return (w,p)

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

-- | Move to @n@ lines down from top of screen
downFromTosE :: Int -> Action
downFromTosE n = do
    (i,fn) <- withWindow $ \w _ -> do
                    let y  = fst $ cursor w
                        n' = min n (height w - 1 - 1)
                        d  = n' - y
                    return (w, (abs d, if d < 0 then upE else downE))
    replicateM_ i fn

-- | Move to @n@ lines up from the bottom of the screen
upFromBosE :: Int -> Action
upFromBosE n = (withWindow $ \w _ -> return (w, height w -1 -1 - n)) >>= downFromTosE

-- | Move to middle line in screen
middleE :: Action
middleE = (withWindow $ \w _ -> return (w, (height w -1-1) `div` 2)) >>= downFromTosE

-- ---------------------------------------------------------------------

-- | move the point left (backwards) in the buffer. may need to scroll
leftE :: Action
leftE = withWindow_ $ \w b -> do
            e <- atSol b
            if not e then moveXorSolW 1 w b
                     else moveUpW w b >>= flip moveToEolW b

-- | move the point right (forwards) in the buffer. may need to scroll
rightE :: Action
rightE = withWindow_ $ \w b -> do
            e <- atEol b
            if not e then moveXorEolW 1 w b
                     else moveDownW w b >>= flip moveToSolW b

-- | Move left @x@ or to start of line
leftOrSolE :: Int -> Action
leftOrSolE x = withWindow_ $ moveXorSolW x

-- | Move right @x@ or to end of line
rightOrEolE :: Int -> Action
rightOrEolE x = withWindow_ $ moveXorEolW x

-- | Move to first non-space character in this line
firstNonSpaceE :: Action
firstNonSpaceE = withWindow_ firstNonSpaceW

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
insertE c = do
    withWindow_ $ \w b -> do
            s <- sizeB b
            if s == 0 then insertW '\n' w b else return w
    withWindow_ $ insertW c

-- | Delete character under cursor
deleteE :: Action
deleteE = withWindow_ $ \w b -> deleteNW w b 1

-- | Delete @n@ characters from under the cursor
deleteNE :: Int -> Action
deleteNE i = withWindow_ $ \w b -> deleteNW w b i

-- | Kill to end of line
killE :: Action
killE = withWindow_ deleteToEolW -- >>= Buffer.prevXorLn 1

-- | Read the char under the cursor
readE :: IO Char
readE = withWindow $ \w b -> readB b >>= \c -> return (w,c)

-- | Read the line the cursor is on
readLnE :: IO String
readLnE = withWindow $ \w b -> do
    i <- indexOfSol b 
    j <- indexOfEol b
    s <- nelemsB b (j-i) i
    return (w,s)

-- | Read from point to end of line
readRestOfLnE :: IO String
readRestOfLnE = withWindow $ \w b -> do
    p <- pointB b
    j <- indexOfEol b
    s <- nelemsB b (j-p) p
    return (w,s)

-- | Write char to point
writeE :: Char -> Action
writeE c = withWindow_ $ \w b -> do
            case c of
                '\r' -> writeB b '\n'
                _ | isLatin1 c -> writeB b c 
                  | otherwise  -> nopE          -- TODO
            return w

-- ---------------------------------------------------------------------
-- registers (TODO these may be redundant now that it is easy to thread
-- state in key bindings, or maybe not.
--

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
        Nothing    -> errorE "Pattern not found"  -- TODO

------------------------------------------------------------------------

-- | Search and replace /on current line/. Returns Bool indicating
-- success or failure
--
-- TODO too complex.
--
searchAndRepLocal :: String -> String -> IO Bool
searchAndRepLocal [] _ = return False   -- hmm...
searchAndRepLocal re str = do
    c_re <- regcomp re regExtended
    setRegexE c_re      -- store away for later use

    mp <- withWindow $ \w b -> do   -- find the regex
            mp <- regexB b c_re
            return (w, mp)
    case mp of
        Just (i,j) -> withWindow $ \w b -> do
                p  <- pointB b      -- all buffer-level atm
                moveToEol b
                ep <- pointB b      -- eol point of current line
                moveTo b i
                moveToEol b
                eq <- pointB b      -- eol of matched line
                moveTo b p          -- go home. sub doesn't move
                if (ep /= eq)       -- then match isn't on current line
                    then return (w, False)
                    else do         -- do the replacement
                moveTo b i
                deleteN b (j - i)
                insertN b str
                moveTo b p          -- and back to where we were!
                return (w, True) -- signal success
        Nothing -> return False

------------------------------------------------------------------------

-- | Set the cmd buffer, and draw message at bottom of screen
msgE :: String -> Action
msgE s = modifyEditor_ $ \e -> return e { cmdline = s }

-- | Set the cmd buffer, and draw a pretty error message
errorE :: String -> Action
errorE s = modifyEditor_ $ \e -> return e { cmdline = s }

-- | Clear the message line at bottom of screen
msgClrE :: Action
msgClrE = modifyEditor_ $ \e -> return e { cmdline = [] } 

-- | Get the current cmd buffer
getMsgE :: IO String
getMsgE = readEditor cmdline 

------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------
-- Window manipulation

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
    (y,_) <- UI.screenSize
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

-- | close current window.
-- ToDo If this is the last window onto this buffer, free and close.
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

------------------------------------------------------------------------

repeatM_ :: Monad m => m a -> m ()
repeatM_ a = a >> repeatM_ a
{-# SPECIALIZE repeatM_ :: IO a -> IO () #-}
{-# INLINE repeatM_ #-}
