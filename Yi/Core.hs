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

import Yi.UI
import qualified Yi.UI     as UI

import Yi.Editor
import qualified Yi.Editor as Editor

import Data.Char            ( isLatin1 )
import System.IO
import System.Directory     ( doesFileExist )
import System.Exit
import Control.Monad        ( when )
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
    Control.Exception.handleJust (ioErrors) (\e -> msgE (show e)) $ do
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
    km <- Editor.getKeyBinds
    let mainloop = do 
            c <- getcE
            Control.Exception.catchJust (ioErrors) (km c) (\e -> msgE (show e))
            UI.refresh
            mainloop
    mainloop

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit
quitE :: IO ()
quitE = exitWith ExitSuccess

-- | Refresh the screen
refreshE :: IO ()
refreshE = UI.refresh

-- | Do nothing
nopE :: IO ()
nopE = return ()

-- ---------------------------------------------------------------------
-- Movement operations

{-
--
-- | Move cursor to origin
--
topE :: IO ()
topE = withWindow_ $ moveToW 0

--
-- | Move cursor to end of buffer
--
botE :: IO ()
botE = withWindowDo_ $ \b -> sizeB b >>= \n -> moveTo b (n-1)
-}

-- | Move cursor to start of line
solE :: IO ()
solE = withWindow_ moveToSolW

-- | Move cursor to end of line
eolE :: IO ()
eolE = withWindow_ moveToEolW

-- | Move cursor down 1 line
downE :: IO ()
downE = withWindow_ moveDownW

-- | Move cursor up to the same point on the previous line
upE :: IO ()
upE = withWindow_ moveUpW

-- | Scroll up 1 screen
-- Inefficient
upScreenE :: IO ()
upScreenE = do
    (Just w) <- getWindow   -- better be a window open..
    mapM_ (\_ -> withWindow_ moveUpW)  [1 .. height w - 3]
    withWindow_ moveUpW

-- | Scroll down 1 screen
-- Inefficient
downScreenE :: IO ()
downScreenE = do
    (Just w) <- getWindow  -- better be a window open..
    mapM_ (\_ -> withWindow_ moveDownW)  [1 .. (height w - 2) * 2]
    ll <- withWindow $ \w b -> atLastLine b >>= \ll -> return (w,ll)
    when (not ll) $
        mapM_ (\_ -> withWindow_ moveUpW) [1 .. height w - 2]

-- | Move left @x@ or to start of line
leftOrSolE :: Int -> IO ()
leftOrSolE x = withWindow_ $ moveXorSolW x

-- | Move right @x@ or to end of line
rightOrEolE :: Int -> IO ()
rightOrEolE x = withWindow_ $ moveXorEolW x

-- ---------------------------------------------------------------------
-- Window based operations
--

{-
-- | scroll window up
scrollUpE :: IO ()
scrollUpE = withWindow_ scrollUpW

-- | scroll window down
scrollDownE :: IO ()
scrollDownE = withWindow_ scrollDownW
-}

------------------------------------------------------------------------

-- | Insert new character
insertE :: Char -> IO ()
insertE c = withWindow_ $ insertW c

-- | Delete character under cursor
deleteE :: IO ()
deleteE = withWindow_ deleteW

-- | Kill to end of line
killE :: IO ()
killE = withWindow_ deleteToEolW -- >>= Buffer.prevXorLn 1

-- | Read the char under the cursor
readE :: IO Char
readE = withWindow $ \w b -> readB b >>= \c -> return (w,c)

-- | Write char to point
writeE :: Char -> IO ()
writeE c = withWindow_ $ \w b -> do
            if isLatin1 c then writeB b c else nopE -- TODO
            return w

------------------------------------------------------------------------

-- | Draw message at bottom of screen
msgE :: String -> IO ()
msgE s = do modifyEditor_ $ \e -> return e { cmdline = s }
            UI.drawCmdLine s -- immediately draw

-- | Clear the message line at bottom of screen
msgClrE  :: IO ()
msgClrE = do modifyEditor_ $ \e -> return e { cmdline = [] } 
             UI.drawCmdLine [] -- immediately draw

-- | File info
bufInfoE :: IO (FilePath, Int, Int)
bufInfoE = withWindow $ \w b -> do
    s  <- sizeB b
    return (w, (nameB b, s, lineno w))

------------------------------------------------------------------------
-- | Window manipulation

-- | edit the next buffer in the buffer list
nextBufW :: IO ()
nextBufW = do
    w <- getWindow
    b <- Editor.nextBuffer
    deleteWindow w         -- !! don't delete window before getting the next buffer
    w' <- Editor.newWindow b
    Editor.setWindow w'

-- | edit the previous buffer in the buffer list
prevBufW :: IO ()
prevBufW = do
    b <- Editor.prevBuffer
    getWindow >>= deleteWindow
    w' <- newWindow b
    setWindow w'

-- | Read file into buffer and open up a new window
fnewE  :: FilePath -> IO ()
fnewE f = do
    e  <- doesFileExist f
    b  <- if e then hNewBuffer f else stringToNewBuffer f []
    getWindow >>= deleteWindow
    w <- newWindow b
    Editor.setWindow w

-- | Write current buffer to disk
fwriteE :: IO ()
fwriteE = withWindow_ $ \w b -> hPutB b (nameB b) >> return w

-- | Split a second window onto this buffer :)
splitE :: IO ()
splitE = do
    i     <- sizeWindows
    (y,_) <- screenSize
    let (sy,r) = getY y i
    if sy + r <= 4
        then msgE "Not enough room to split"
        else do
    mw <- getWindow
    case mw of 
        Nothing -> nopE
        Just w  -> do b <- getBufferWith (bufkey w)
                      w' <- newWindow b
                      Editor.setWindow w'

-- | close current window. quit if that is all the windows
closeE :: IO ()
closeE = do getWindow >>= deleteWindow
            i <- sizeWindows
            if i == 0 then quitE else nopE

-- | Shift focus to next window
nextWinE :: IO ()
nextWinE = Editor.nextWindow

-- | Shift focus to prev window
prevWinE :: IO ()
prevWinE = Editor.prevWindow

