{-# OPTIONS -fglasgow-exts -#include YiUtils.h #-}
-- -fglasgow-exts for deriving Typeable
-- 
-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) Don Stewart 2004-5. http://www.cse.unsw.edu.au/~dons
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

--
-- | The core actions of yi. This module is the link between the editor
-- and the UI. Key bindings, and libraries should manipulate Yi through
-- the interface defined here.
--

module Yi.Core (

        -- * Construction and destruction
        startE,         -- :: a -> Editor.Config -> Int -> Maybe [FilePath] -> IO ()
        quitE,          -- :: Action
        rebootE,        -- :: Action
        reloadE,        -- :: Action
        refreshE,       -- :: Action
        suspendE,       -- :: Action

        -- * Main loop
        eventLoop,

        -- * Global editor actions
        getcE,          -- :: IO Char
        nopE,           -- :: Action
        msgE,           -- :: String -> Action
        errorE,         -- :: String -> Action
        msgClrE,        -- :: Action
        getMsgE,        -- :: IO String
        bufInfoE,       -- :: IO (FilePath,Int,Int,Int,Int,String)
        fileNameE,      -- :: IO (Maybe FilePath)
        bufNameE,       -- :: IO String
        setWindowFillE, -- :: Char -> Action

        -- * Window manipulation
        nextWinE,       -- :: Action
        prevWinE,       -- :: Action
        setWinE,        -- :: Window -> Action
        closeE,         -- :: Action
        closeOtherE,    -- :: Action
        splitE,         -- :: Action
        enlargeWinE,    -- :: Action
        shrinkWinE,     -- :: Action

        -- * Switching to the command line
        cmdlineFocusE,  -- :: Action
        cmdlineUnFocusE,-- :: Action


        -- * File-based actions
        fnewE,          -- :: FilePath -> Action
        fwriteE,        -- :: Action
        fwriteAllE,     -- :: Action
        fwriteToE,      -- :: String -> Action
        backupE,        -- :: FilePath -> Action

        -- * Buffer only stuff
        nextBufW,       -- :: Action
        prevBufW,       -- :: Action
        newBufferE,     -- :: String -> String -> Action
        listBuffersE,   -- :: Action
        closeBufferE,   -- :: String -> Action
        isUnchangedE,   -- :: IO Bool
        setUnchangedE,  -- :: Action

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
        insertNE,       -- :: String -> Action
        deleteE,        -- :: Action
        deleteNE,       -- :: Int -> Action
        killE,          -- :: Action
        deleteRegionE,  -- :: (Int,Int) -> Action
        writeE,         -- :: Char -> Action
        undoE,          -- :: Action
        redoE,          -- :: Action

        -- * Read parts of the buffer
        readE,          -- :: IO Char
        readRegionE,    -- :: (Int,Int) -> IO String
        readLnE,        -- :: IO String
        readNM,         -- :: Int -> Int -> IO String
        readRestOfLnE,  -- :: IO String
        readAllE,       -- :: IO String

        swapE,          -- :: Action

        -- * Basic registers
        setRegE,        -- :: String -> Action
        getRegE,        -- :: IO String
        setRegexE,      -- :: SearchExp -> Action
        getRegexE,      -- :: IO (Maybe SearchExp)

        -- * Marks
        setMarkE,        
        getMarkE,       
        exchangePointAndMarkE,

        -- * Dynamically extensible state
        getDynamic,
        setDynamic,

        -- * Regular expression and searching
        SearchF(..),        -- Basic | IgnoreCase | NoNewLine
        SearchResult,
        SearchMatch,
	SearchExp,
        searchAndRepLocal,  -- :: String -> String -> IO Bool
        searchE,            -- :: (Maybe String) -> [SearchF] 
                            -- -> (() -> Either () ()) -> Action
        searchInitE,        -- :: String
                            -- -> [SearchF]
                            -- -> IO SearchExp
        searchDoE,          -- :: SearchExp
                            -- -> (() -> Either () ())
                            -- -> IO SearchResult

        -- * higher level ops
        mapRangeE,              -- :: Int -> Int -> (Buffer' -> Action) -> Action

        -- * Modifying the current keymap
        metaM,                  -- :: ([Char] -> [Action]) -> IO ()

        -- * Interacting with external commands
        pipeE                   -- :: String -> IO String

   ) where

import Yi.MkTemp            ( mkstemp )
import Yi.Buffer
import Yi.Window
import Yi.Regex
import Yi.String
import Yi.Process           ( popen )
import Yi.Map as M          ( lookup, insert )
import Yi.Editor
import qualified Yi.Editor as Editor

import Data.Maybe
import Data.Char            ( isLatin1 )
import Data.Dynamic         


import System.IO            ( hClose )
import System.Directory     ( doesFileExist )
import System.Exit          ( exitWith, ExitCode(ExitSuccess) )

import Control.Monad
import Control.Exception
import Control.Concurrent   ( threadWaitRead, takeMVar, forkIO )
import Control.Concurrent.Chan

import GHC.Exception hiding ( throwIO )

import qualified Yi.Curses.UI as UI

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
--
startE :: Maybe Editor 
       -> (Editor.Config, (Maybe Editor) -> IO (), IO (Maybe Editor.Config) ) 
       -> Int 
       -> Maybe [FilePath] 
       -> IO ()

startE st (confs,fn,fn') ln mfs = do

    -- restore the old state
    case st of
        -- need to update default keymap to point to current code.
        Just e' -> modifyEditor_ $ const $ return e'
        Nothing -> return ()

    UI.start refreshE
    sz <- UI.screenSize
    modifyEditor_ $ \e -> return $ e { scrsize = sz }

    Editor.setUserSettings confs fn fn'

    t <- forkIO refreshLoop -- fork UI thread
    modifyEditor_ $ \e -> return $ e { threads = [t] }

    when (isNothing st) $ do -- read in any files if booting for the first time
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

    when (isJust st) refreshE -- and redraw

    where
        --
        -- | Action to read characters into a channel
        -- We block our thread waiting for input on stdin
        --
        getcLoop :: Chan Char -> IO ()
        getcLoop ch = repeatM_ $ threadWaitRead 0 >> getcE >>= writeChan ch

        --
        -- | When the editor state isn't being modified, refresh, then wait for
        -- it to be modified again. Also, check if the window got
        -- resized, and take the opportunity to refresh.
        --
        refreshLoop :: IO ()
        refreshLoop = repeatM_ $ do 
                        takeMVar editorModified
                        handleJust ioErrors (errorE.show) UI.refresh 

-- ---------------------------------------------------------------------
-- | How to read from standard input
--
getcE :: IO Char
getcE = UI.getKey refreshE -- only used if we don't have sigwinch

-- 
-- | The editor main loop. Read key strokes from the ui and interpret
-- them using the current key map. Keys are bound to core actions.
--
eventLoop :: IO ()
eventLoop = do
    fn <- Editor.getKeyBinds 
    ch <- readEditor input
    let run km = catchDyn (sequence_ . km =<< getChanContents ch)
                          (\(MetaActionException km') -> run km')
    repeatM_ $ handle handler (run fn)

    where
      handler e | isJust (ioErrors e) = errorE (show e)
                | isExitCall e        = throwIO e
                | otherwise           = errorE (show e)

      isExitCall (ExitException _) = True
      isExitCall _ = False

-- TODO if there is an exception, the key bindings will be reset...

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit.
quitE :: Action
quitE = exitWith ExitSuccess -- throws to top level

--
-- | Reboot (!). Reboot the entire editor, reloading the Yi core.
--
rebootE :: Action
rebootE = do
    cmdlineUnFocusE     -- return focus to buffer (seems natural)
    e  <- readEditor id
    fn <- readEditor reboot
    Editor.shutdown
--  UI.end
    fn (Just e)

-- | Recompile and reload the user's config files
reloadE :: Action
reloadE = do
    fn <- readEditor reload
    modifyEditor_ $ \e -> do
        v <- fn
        return $ case v of
            Nothing -> e
            Just (Config km sty) -> e { curkeymap = km, uistyle = sty }
    UI.initcolours

-- | Reset the size, and force a complete redraw
refreshE :: Action
refreshE = UI.resizeui >>= doResizeAll

-- | Do nothing
nopE :: Action
nopE = return ()

-- | Suspend the program
suspendE :: Action
suspendE = UI.suspend

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
             else do e' <- atSof b
                     if e' then return w
                           else moveUpW w b >>= flip moveToEolW b

-- | move the point right (forwards) in the buffer. may need to scroll
rightE :: Action
rightE = withWindow_ $ \w b -> do
    e <- atEol b
    if not e then moveXorEolW 1 w b
             else do e' <- atEof b
                     if e' then return w
                           else moveDownW w b >>= flip moveToSolW b

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
insertE c = do
    withWindow_ $ \w b -> do
            s <- sizeB b
            if s == 0 then insertW '\n' w b else return w
    withWindow_ $ insertW c

-- | Insert a string
insertNE :: String -> Action
insertNE = mapM_ insertE

-- | Delete character under cursor
deleteE :: Action
deleteE = withWindow_ $ \w b -> deleteNW w b 1

-- | Delete @n@ characters from under the cursor
deleteNE :: Int -> Action
deleteNE i = withWindow_ $ \w b -> deleteNW w b i

-- | Kill to end of line
killE :: Action
killE = withWindow_ deleteToEolW -- >>= Buffer.prevXorLn 1

-- | Delete an arbitrary part of the buffer
deleteRegionE :: (Int,Int) -> IO ()
deleteRegionE (from,to) = withWindow_ $ \w b -> do
    deleteNAtW  w b (to-from) from    

-- | Read the char under the cursor
readE :: IO Char
readE = withWindow $ \w b -> readB b >>= \c -> return (w,c)

-- | Read an arbitrary part of the buffer
readRegionE :: (Int,Int) -> IO String
readRegionE (from,to) = readNM from to

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

-- | Return the contents of the buffer as a string (note that this will
-- be very expensive on large (multi-megabyte) buffers)
readAllE :: IO String
readAllE = withWindow $ \w b -> do
        s <- elemsB b
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

-- | Transpose two characters, (the Emacs C-t action)
-- Note that mg and emacs only work on the current line, whereas this
-- transpose will operate over the entire buffer if required.
swapE :: Action
swapE = do c <- readE
           deleteE
           leftE
           insertE c
           rightE

-- ---------------------------------------------------------------------

undoE :: Action
undoE = (withWindow_ $ \w b -> undo b >> return w) >> getPointE >>= gotoPointE

redoE :: Action
redoE = (withWindow_ $ \w b -> redo b >> return w) >> getPointE >>= gotoPointE

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


-- ----------------------------------------------------
-- | Marks

-- | Set the current buffer mark
setMarkE :: Int -> Action
setMarkE pos = withWindow_ $ \w b -> (setMarkB b pos >> return w)

-- | Get the current buffer mark
getMarkE :: IO Int
getMarkE = withWindow $ \w b -> do pos <- getMarkB b
                                   return (w, pos)
-- | Exchange point & mark.
-- Maybe this is better put in Emacs\/Mg common file
exchangePointAndMarkE :: Action
exchangePointAndMarkE = do m <- getMarkE
                           p <- getPointE
                           setMarkE p
                           gotoPointE m

-- ---------------------------------------------------------------------
-- | Dynamically-extensible state components.
--
-- These hooks are used by keymaps to store values that result from
-- Actions (i.e. that restult from IO), as opposed to the pure values
-- they generate themselves, and can be stored internally.
--
-- The `dynamic' field is a type-indexed map.
-- 

-- | Retrieve the extensible state
{-
--
-- More info: GHC 6.2 and 6.4 vary quite greatly with this code, and it
-- seems like a bug, investigations continuing
--
getDynamic :: forall a. Initializable a => IO a
getDynamic = do
        ps <- readEditor dynamic
        case M.lookup (show $ typeOf (undefined :: a)) ps of
            Nothing -> initial
            Just x -> return $ fromJust $ fromDynamic x
-}

getDynamic :: Initializable a => IO a
getDynamic = getDynamic' (undefined :: a)
    where
        getDynamic' :: Initializable b => b -> IO b
        getDynamic' a = do
                ps <- readEditor dynamic
                case M.lookup (show $ typeOf a) ps of
                    Nothing -> initial
                    Just x -> return $ fromJust $ fromDynamic x

-- | Insert a value into the extensible state, keyed by its type
setDynamic :: Typeable a => a -> Action
setDynamic x = modifyEditor_ $ \e -> 
        return e { dynamic = M.insert (show $ typeOf x) (toDyn x) (dynamic e) }

-- ---------------------------------------------------------------------
-- Searching and substitutions with regular expressions
--
-- The most recent regex is held by the editor. You can get at it with
-- getRegeE. This is useful to determine if there was a previous
-- pattern.
--

-- | Put regex into regex 'register'
setRegexE :: SearchExp -> Action
setRegexE re = modifyEditor_ $ \e -> return e { regex = Just re }

-- Return contents of regex register
getRegexE :: IO (Maybe SearchExp)
getRegexE = readEditor regex
 
-- ---------------------------------------------------------------------
--
-- | Global searching. Search for regex and move point to that position.
-- @Nothing@ means reuse the last regular expression. @Just s@ means use
-- @s@ as the new regular expression. Direction of search can be
-- specified as either @Left@ (backwards) or @Right@ (forwards in the
-- buffer). Arguments to modify the compiled regular expression can be
-- supplied as well.
--

--
-- What would be interesting would be to implement our own general
-- mechanism to allow users to supply a regex function of any kind, and
-- search with that. This removes the restriction on strings be valid
-- under regex(3).
--

data SearchF = Basic        -- ^ Use non-modern (i.e. basic) regexes
             | IgnoreCase   -- ^ Compile for matching that ignores char case
             | NoNewLine    -- ^ Compile for newline-insensitive matching
    deriving Eq

type SearchMatch = (Int, Int)
type SearchResult = Maybe (Either SearchMatch SearchMatch)
type SearchExp = (String, Regex)

searchE :: (Maybe String)       -- ^ @Nothing@ means used previous
                                -- pattern, if any. Complain otherwise.
                                -- Use getRegexE to check for previous patterns
        -> [SearchF]            -- ^ Flags to modify the compiled regex
        -> (() -> Either () ()) -- ^ @Left@ means backwards, @Right@ means forward
        -> Action

searchE s fs d = 
     case s of
        Just re -> searchInitE re fs >>= (flip searchDoE) d >>= f
        Nothing -> do
	    mre <- getRegexE
            case mre of
                Nothing -> errorE "No previous search pattern" -- NB
                Just r -> searchDoE r d >>= f
    where
        f mp = case mp of
            Just (Right _) -> return ()
            Just (Left  _) -> msgE "Search wrapped"
            Nothing        -> errorE "Pattern not found"


searchDoE :: SearchExp
          -> (() -> Either () ())
          -> IO SearchResult

searchDoE _ d | isLeft (d ()) = do
        errorE "Backward searching is unimplemented"
	return Nothing
    where
        isLeft (Left _) = True
        isLeft _ = False

searchDoE (s, re) _ = searchF s re

--
-- Set up a search.
--
searchInitE :: String -> [SearchF] -> IO SearchExp
searchInitE re fs = do
    c_re <- regcomp re (extended + igcase + newline)
    let p = (re,c_re)
    setRegexE p
    return p

    where
        extended | Basic      `elem` fs = 0
                 | otherwise            = regExtended   -- extended regex dflt
        igcase   | IgnoreCase `elem` fs = regIgnoreCase
                 | otherwise            = 0             -- case insensitive dflt
        newline  | NoNewLine  `elem` fs = 0
                 | otherwise            = regNewline    -- newline is special


-- ---------------------------------------------------------------------
-- Internal

--
-- Do a forward search, placing cursor at first char of pattern, if found.
-- Keymaps may implement their own regex language. How do we provide for this?
-- Also, what's happening with ^ not matching sol?
--
searchF :: String -> Regex -> IO SearchResult
searchF _ c_re = do
    mp <- withWindow $ \w b -> do
            p   <- pointB b
            rightB b                  -- start immed. after cursor
            mp  <- regexB b c_re
            case fmap Right mp of
                x@(Just _) -> return (w,x)
                _ -> do moveTo b 0
                        np <- regexB b c_re
                        moveTo b p
                        return (w, fmap Left np)
    case mp of
        Just (Right (p,_)) -> gotoPointE p >> return mp
        Just (Left  (p,_)) -> gotoPointE p >> return mp
	_                  -> return mp

------------------------------------------------------------------------
-- Global search and replace
--


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
    setRegexE (re,c_re)     -- store away for later use

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
-- | Pipe a string through an external command, returning the stdout
-- chomp any trailing newline (is this desirable?)
--
-- Todo: varients with marks?
--
pipeE :: String -> String -> IO String
pipeE cmd inp = do
    let (f:args) = split " " cmd
    (out,_err,_) <- popen f args (Just inp)
    return (chomp "\n" out)

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

-- ---------------------------------------------------------------------
-- Buffer operations

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

-- | Maybe a file associated with this buffer
fileNameE :: IO (Maybe FilePath)
fileNameE = withWindow $ \w b -> getfileB b >>= \f -> return (w, f)

-- | Name of this buffer
bufNameE :: IO String
bufNameE = withWindow $ \w b -> return (w, nameB b)

-- | A character to fill blank lines in windows with. Usually '~' for
-- vi-like editors, ' ' for everything else
setWindowFillE :: Char -> Action
setWindowFillE c = modifyEditor_ $ \e -> return $ e { windowfill = c }

-- | Close the current window, attach the next buffer in the buffer list
-- to a new window, and open it up
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

-- | If file exists, read contents of file into a new buffer, otherwise
-- creating a new empty buffer. Replace the current window with a new
-- window onto the new buffer.
--
-- Need to clean up semantics for when buffers exist, and how to attach
-- windows to buffers.
--
fnewE  :: FilePath -> Action
fnewE f = do
    e  <- doesFileExist f
    b  <- if e then hNewBuffer f else stringToNewBuffer f []
    setfileB b f        -- and associate with file f
    deleteThisWindow
    w <- newWindow b
    Editor.setWindow w

-- | Like fnewE, create a new buffer filled with the String @s@, 
-- Open up a new window onto this buffer. Doesn't associated any file
-- with the buffer (unlike fnewE) and so is good for popup internal
-- buffers (like scratch or minibuffer)
newBufferE :: String -> String -> Action
newBufferE f s = do
    b <- stringToNewBuffer f s
    splitE
    deleteThisWindow
    w <- newWindow b
    Editor.setWindow w

-- | Write current buffer to disk, if this buffer is associated with a file
fwriteE :: Action
fwriteE = withWindow_ $ \w b -> do
        mf <- getfileB b
        case mf of
                Nothing -> error "buffer not associated with a file"
                Just f  -> hPutB b f >> return w

-- | Write current buffer to disk as @f@. If this buffer doesn't
-- currently have a file associated with it, the file is set to @f@
fwriteToE :: String -> Action
fwriteToE f = withWindow_ $ \w b -> do
        hPutB b f
        mf <- getfileB b
        case mf of
                Nothing -> setfileB b f
                Just _  -> return ()
        return w

-- | Write all open buffers
fwriteAllE :: Action
fwriteAllE = undefined

-- | Make a backup copy of file
backupE :: FilePath -> Action
backupE = undefined

-- | Return a list of all buffers, and their indicies
listBuffersE :: IO [(String,Int)]
listBuffersE = do
        bs  <- getBuffers
        return $ zip (map nameB bs) [0..]

-- | Release resources associated with buffer, close any windows open
-- onto buffer.
closeBufferE :: String -> Action
closeBufferE f = killBuffer f

------------------------------------------------------------------------

-- | Is the current buffer unmodifed? (currently buggy, we need
-- bounaries in the undo list)
isUnchangedE :: IO Bool
isUnchangedE = withWindow $ \w b -> isUnchangedB b >>= \v -> return (w,v)

-- | Set the current buffer to be unmodifed
setUnchangedE :: Action
setUnchangedE = undefined

------------------------------------------------------------------------
--
-- Window operations

-- | Make the next window (down the screen) the current window
nextWinE :: Action
nextWinE = Editor.nextWindow

-- | Make the previous window (up the screen) the current window
prevWinE :: Action
prevWinE = Editor.prevWindow

-- | Make window with key @k@ the current window
setWinE :: Window -> Action
setWinE = Editor.setWindowToThisWindow

-- | Split the current window, opening a second window onto this buffer.
-- Windows smaller than 3 lines cannot be split.
splitE :: Action
splitE = do
    i     <- sizeWindows
    (y,_) <- UI.screenSize -- bah
    let (sy,r) = getY y i
    if sy + r <= 4  -- min window size
        then errorE "Not enough room to split"
        else do
    mw <- getWindow
    case mw of 
        Nothing -> nopE
        Just w  -> do b <- getBufferWith (bufkey w)
                      w' <- newWindow b
                      Editor.setWindow w'

-- | Enlarge the current window
enlargeWinE :: Action
enlargeWinE = getWindow >>= enlargeWindow

-- | Shrink the current window
shrinkWinE :: Action
shrinkWinE = getWindow >>= shrinkWindow

-- | Close the current window. 
-- If this is the last window open, quit the program. TODO this
-- behaviour may be undesirable
closeE :: Action
closeE = do 
        deleteThisWindow
        i <- sizeWindows
        when (i == 0) quitE

-- | Make the current window the only window on the screen
closeOtherE :: Action
closeOtherE = do
        this   <- getWindow -- current window
        others <- modifyEditor $ \e -> do
                        let ws = getWindows e
                        return (e, (filter (/= this) (map Just ws)))
        mapM_ deleteWindow others

------------------------------------------------------------------------

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
-- Fold over a range is probably useful too..
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

-- ---------------------------------------------------------------------
-- | The metaM action. This is our mechanism for having Actions alter
-- the current keymap. It is similar to the Ctk lexer\'s meta action.
-- It takes a new keymap to use, throws a dynamic exception, which
-- interrupts the main loop, causing it to restart with the given
-- exception. An alternative would be to change all action types to
-- @IO (Maybe Keymap)@, and check the result of each action as it is
-- forced. Currently, my feeling is that metaM will be rare enough not
-- to bother with this solution. Also, the dynamic exception solution
-- changes only a couple of lines of code.
--

--
-- Our meta exception returns the next keymap to use
--
newtype MetaActionException = MetaActionException ([Char] -> [Action])
    deriving Typeable

--
-- | Given a keymap function, throw an exception to interrupt the main
-- loop, which will continue processing with the supplied keymap.  Use
-- this when you want to alter the keymap lexer based on the outcome of
-- some IO action. Altering the keymap based on the input to  the keymap
-- is achieved by threading a state variable in the keymap itself.
--
metaM :: ([Char] -> [Action]) -> IO ()
metaM km = throwDyn (MetaActionException km)

------------------------------------------------------------------------

repeatM_ :: forall m a. Monad m => m a -> m ()
repeatM_ a = a >> repeatM_ a
{-# SPECIALIZE repeatM_ :: IO a -> IO () #-}
{-# INLINE repeatM_ #-}
