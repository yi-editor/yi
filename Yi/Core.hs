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
        rebootE,        -- :: Action
        reloadE,        -- :: Action
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
        cmdlineFocusE,  -- :: Action
        cmdlineUnFocusE,-- :: Action
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

        nextNParagraphs,    -- :: Int -> Action
        prevNParagraphs,    -- :: Int -> Action

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
        writeE,         -- :: Char -> Action

        -- * Read parts of the buffer
        readE,          -- :: IO Char
        readLnE,        -- :: IO String
        readNM,         -- :: Int -> Int -> IO String
        readRestOfLnE,  -- :: IO String
        readWordE,      -- :: IO (String,Int,Int)
        readWordLeftE,  -- :: IO (String,Int,Int)

        -- * Basic registers
        setRegE,        -- :: String -> Action
        getRegE,        -- :: IO String
        setRegexE,      -- :: Regex -> Action
        getRegexE,      -- :: IO (Maybe Regex)

        -- * Regular expression and searching
        findE,                  -- :: String -> IO (Maybe (Int,Int))
        searchE,                -- :: (Maybe String) -> Action
        searchAndRepLocal,      -- :: String -> String -> IO Bool

        -- * higher level ops
        mapRangeE,              -- :: Int -> Int -> (Buffer' -> Action) -> Action
        moveWhileE,             -- :: (Char -> Bool) -> Bool -> Action

        -- * Word completion
        wordCompleteE,          -- :: Action
        resetCompleteE,         -- :: Action

   ) where

import Yi.MkTemp            ( mkstemp )
import Yi.Buffer
import Yi.Window
import Yi.Regex             ( regcomp, regExtended, Regex )
import Yi.Editor
import qualified Yi.Editor as Editor

import Data.Maybe           ( isNothing, isJust )
import Data.Char            ( isLatin1, isAlphaNum )
import Data.FiniteMap

import System.IO            ( hClose )
import System.Directory     ( doesFileExist )
import System.Exit          ( exitWith, ExitCode(ExitSuccess) )

import Control.Monad
import Control.Exception    ( ioErrors, handle, throwIO, handleJust, assert )
import Control.Concurrent   ( threadWaitRead, takeMVar, forkIO )
import Control.Concurrent.Chan

import GHC.Exception hiding ( throwIO )

import qualified Yi.Curses.UI as UI

import Data.IORef
import System.IO.Unsafe     ( unsafePerformIO )

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
-- TODO should be in keybind
--
startE :: (Editor.Config, IO (), IO Editor.Config ) 
       -> Int 
       -> Maybe [FilePath] 
       -> IO ()

startE (confs,fn,fn') ln mfs = do

    Editor.setUserSettings confs fn fn'

    UI.start
    sz <- UI.screenSize
    modifyEditor_ $ \e -> return $ e { scrsize = sz }

    t <- forkIO refreshLoop -- fork UI thread
    modifyEditor_ $ \e -> return $ e { threads = [t] }

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
    repeatM_ $ handle handler (sequence_ . fn =<< getChanContents ch)

    where
      handler e | isJust (ioErrors e) = errorE (show e)
                | isExitCall e        = throwIO e
                | otherwise           = errorE (show e)

      isExitCall (ExitException _) = True
      isExitCall _ = False

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

--
-- | Reboot (!). Reboot the entire editor, reloading the Yi core.
-- Should store all the buffers somewhere, and reload them on returning.
--
rebootE :: Action
rebootE = do
    fn <- readEditor reboot
    Editor.shutdown
--  UI.end
    fn

-- | Recompile and reload the user's config files
reloadE :: Action
reloadE = do
    fn <- readEditor reload
    modifyEditor_ $ \e -> do
        Config km sty <- fn
        return e { curkeymap = km, uistyle = sty }
    UI.initcolours

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

-- ---------------------------------------------------------------------
-- Slightly higher level operations
-- Uncommitted to whether these should exist or not. Are they non-core?

-- | Move to first non-space character in this line
firstNonSpaceE :: Action
firstNonSpaceE = withWindow_ firstNonSpaceW

-- | Move down next @n@ paragraphs
nextNParagraphs :: Int -> Action
nextNParagraphs n = do
    withWindow_ $ \w b -> do
        eof <- sizeB b
        let loop = do
                p <- pointB b
                when (p < eof-1) $ do
                    moveWhile_ (/= '\n') Right w b
                    p' <- pointB b
                    when (p' < eof-1) $ do
                        rightB b
                        x <- readB b
                        when (x /= '\n') loop
        replicateM_ n loop
        return w
    getPointE >>= gotoPointE

-- | Move up prev @n@ paragraphs
prevNParagraphs :: Int -> Action
prevNParagraphs n = do
    withWindow_ $ \w b -> do
        let loop = do
                p <- pointB b
                when (p > 0) $ do
                    leftB b
                    moveWhile_ (/= '\n') Left w b
                    p' <- pointB b
                    when (p' > 0) $ do
                        leftB b
                        x <- readB b
                        if x == '\n'
                            then rightB b
                            else loop
        replicateM_ n loop
        return w
    getPointE >>= gotoPointE

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

-- | Read from - to
readNM :: Int -> Int -> IO String
readNM i j = withWindow $ \w b -> nelemsB b (j-i) i >>= \s -> return (w,s)

-- | Read from point to end of line
readRestOfLnE :: IO String
readRestOfLnE = withWindow $ \w b -> do
    p <- pointB b
    j <- indexOfEol b
    s <- nelemsB b (j-p) p
    return (w,s)

-- | Read word to the left of the cursor
readWordLeftE :: IO (String,Int,Int)
readWordLeftE = withWindow $ \w b -> readWordLeft_ w b >>= \s -> return (w,s)

-- Core-internal worker, not threadsafe.
readWordLeft_ :: Window -> Buffer' -> IO (String,Int,Int)
readWordLeft_ w b = do
    p <- pointB b
    c <- readB b 
    when (not $ isAlphaNum c) $ leftB b
    moveWhile_ isAlphaNum Left w b
    sof <- atSof b
    c'  <- readB b 
    when (not sof || not (isAlphaNum c')) $ rightB b
    q <- pointB b
    s <- nelemsB b (p-q) q
    moveTo b p
    return (s,q,p)

-- | Read word under cursor
readWordE :: IO (String,Int,Int)
readWordE = withWindow $ \w b -> readWord_ w b >>= \v -> return (w,v)

-- Internal, for readWordE, not threadsafe
readWord_ :: Window -> Buffer' -> IO (String,Int,Int)
readWord_ w b = do
    p <- pointB b
    c <- readB b 
    if not (isAlphaNum c) then leftB b 
                          else moveWhile_ isAlphaNum Right w b >> leftB b
    y <- pointB b   -- end point
    moveWhile_ isAlphaNum Left w b
    sof <- atSof b
    c'  <- readB b 
    when (not sof || not (isAlphaNum c')) $ rightB b
    x <- pointB b
    s <- nelemsB b (y-x+1) x
    moveTo b p
    return (s,x,y)

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
-- | Global searching for simple strings.
-- Return the index of the next occurence of @s@

findE :: String -> IO (Maybe (Int,Int))
findE [] = return Nothing
findE s = withWindow $ \w b -> do
    re <- regcomp s regExtended
    is <- regexB b re
    return (w,is)

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

-- | Shift focus to command line window
cmdlineFocusE :: Action
cmdlineFocusE = modifyEditor_ $ \e -> return e { cmdlinefocus = True }

-- | Return focus to normal window
cmdlineUnFocusE :: Action
cmdlineUnFocusE = modifyEditor_ $ \e -> return e { cmdlinefocus = False }

------------------------------------------------------------------------
--
-- Map a char function over a range of the buffer.
--
mapRangeE :: Int -> Int -> (Char -> Char) -> Action
mapRangeE from to fn
    | from < 0  = nopE
    | otherwise = do
        withWindow_ $ \w b -> do
            eof <- sizeB b
            when (to < eof) $ do
                let loop j | j <= 0    = return ()
                           | otherwise = do
                                readB b >>= return . fn >>= writeB b
                                rightB b
                                loop (j-1)
                loop (max 0 (to - from))
            moveToW from w b
            return w

------------------------------------------------------------------------
--
-- Shift the point, until predicate is true, leaving point at final
-- location. Direction is either False=left, True=right
--
-- N.B. we're using partially applied Left and Right as well-named Bools.
--
-- Maybe this shouldn't refresh?
--
moveWhileE :: (Char -> Bool) -> (() -> Either () ()) -> Action
moveWhileE f d = do withWindow_ (moveWhile_ f d)
                    getPointE >>= gotoPointE

--
-- internal moveWhile function to avoid unnec. ui updates
-- not for external consumption
--
moveWhile_ :: (Char -> Bool)
           -> (() -> Either () ())
           -> Window
           -> Buffer'
           -> IO Window

moveWhile_ f dir w b = do
    eof <- sizeB b
    let loop = case dir () of  {
        Right _ -> let loop' = do p <- pointB b
                                  when (p < eof - 1) $ do
                                  x <- readB b
                                  when (f x) $ rightB b >> loop'
                   in loop' ;
        Left  _ -> let loop' = do p <- pointB b
                                  when (p > 0) $ do
                                  x <- readB b
                                  when (f x) $ leftB b >> loop 
                   in loop'
    }
    loop
    return w

-- ---------------------------------------------------------------------
-- | keyword completion
--
-- when doing keyword completion, we need to keep track of the word
-- we're trying to complete. Finding this word is an IO action.
--

-- remember the word, if any, we're trying to complete, previous matches
-- we've seen, and the point in the search we are up to.
type Completion = (String,FiniteMap String (),Int)

-- This could go in the single editor state, I suppose. Esp. if we want
-- to do hardcore persistence at some point soon.
--
completions :: IORef (Maybe Completion)
completions = unsafePerformIO $ newIORef Nothing

--
-- | Switch out of completion mode.
--
resetCompleteE :: Action
resetCompleteE = writeIORef completions Nothing

--
-- The word-completion action, down the buffer
--
wordCompleteE :: Action
wordCompleteE = do
    withWindow_ $ \win buf -> do
        readIORef completions >>= loop win buf >>= writeIORef completions
        return win
    getPointE >>= gotoPointE

  where
    --
    -- work out where to start our next search
    --
    loop :: Window -> Buffer' -> (Maybe Completion) -> IO (Maybe Completion)
    loop win buf (Just (w,fm,n)) = do
            p  <- pointB buf
            moveTo buf (n+1)        -- start where we left off
            doloop p win buf (w,fm)
    loop win buf Nothing = do 
            p  <- pointB buf
            (w,_,_) <- readWordLeft_ win buf 
            rightB buf  -- start past point
            doloop p win buf (w,unitFM w ())

    --
    -- actually do the search, and analyse the result
    --
    doloop :: Int -> Window -> Buffer' -> (String,FiniteMap String ()) 
           -> IO (Maybe Completion)

    doloop p win buf (w,fm) = do
            m' <- nextWordMatch win buf w
            moveTo buf p
            (_,j,_) <- readWord_ win buf
            case m' of
                Just (s,i) 
                    | j == i                -- seen entire file
                    -> do replaceLeftWith win buf w
                          return Nothing

                    | s `elemFM` fm         -- already seen
                    -> loop win buf (Just (w,fm,i))

                    | otherwise             -- new
                    -> do replaceLeftWith win buf s
                          return (Just (w,addToFM fm s (),i))

                Nothing -> loop win buf (Just (w,fm,(-1))) -- goto start of file

    --
    -- replace word under cursor with @s@
    --
    replaceLeftWith :: Window -> Buffer' -> String -> IO ()
    replaceLeftWith win buf s = do
        (_,b,a) <- readWordLeft_ win buf     -- back at start
        moveTo buf b
        deleteNW win buf (a-b)
        mapM_ (\c -> insertW c win buf) s

    --
    -- Return next match, and index of that match (to be used for later searches)
    -- Leaves the cursor at the next word.
    --
    nextWordMatch :: Window -> Buffer' -> String -> IO (Maybe (String,Int))
    nextWordMatch win b w = do
        let re = ("( |\t|\n|\r|^)"++w)
        re_c <- regcomp re regExtended
        mi   <- regexB b re_c
        case mi of 
            Nothing -> return Nothing
            Just (i,j) -> do 
                c <- readAtB b i
                let i' = if i == 0 && isAlphaNum c then 0 else i+1 -- for the space
                moveTo b i'
                (s,_,_) <- readWord_ win b
                assert (s /= [] && i /= j) $ return $ Just (s,i')

------------------------------------------------------------------------

repeatM_ :: Monad m => m a -> m ()
repeatM_ a = a >> repeatM_ a
{-# SPECIALIZE repeatM_ :: IO a -> IO () #-}
{-# INLINE repeatM_ #-}
