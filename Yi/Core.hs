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

module Yi.Core (

        -- * Construction and destruction
        startE,
        endE,

        -- * Getting down to business (soon to be replaced by key lexer) 
        eventLoop,

        -- * Editor actions

        -- ** Global editor actions
        quitE,
        refreshE,
        noopE,
        nextE,
        prevE,
        newE,
        getcE,
        msgE,
        msgClrE,

        -- ** File-based actions
        readE,
        writeE,

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
        replaceE,
        insertE,
        deleteE,
        killE,

        -- ** For now, export the symbolic key names from the ui
        module Yi.UI

   ) where

import Yi.MkTemp
import Yi.Editor                            ( withBuffer )
import Yi.Buffer
import Yi.UI
import qualified Yi.UI     as UI
import qualified Yi.Editor as Editor hiding ( withBuffer )

import Data.Char    ( isLatin1 )

import System.IO
import System.Exit

import GHC.Base

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
--
startE :: Editor.Config -> Maybe [FilePath] -> IO ()
startE confs mfs = do
    UI.start
    Editor.setUserSettings confs
    case mfs of
        Just fs -> mapM_ newE fs

        -- vi-like behaviour, just for now.
        Nothing -> do 
            mf <- mkstemp "/tmp/yi.XXXXXXXXXX" 
            case mf of
                Nothing    -> error "Core.startE: mkstemp failed"
                Just (f,h) -> hClose h >> newE f
    refreshE

--
-- | shutdown the editor
--
endE :: IO ()
endE = UI.end

-- ---------------------------------------------------------------------

-- | How to read another character, for user key bindings
getcE :: IO Char
getcE = UI.getKey UI.refresh

-- ---------------------------------------------------------------------
-- | The editor main loop. Read key strokes from the ui and interpret
-- them using the current key map. Keys are bound to core actions.
-- The state is threaded explicitly at the moment.
--

eventLoop :: IO ()
eventLoop = do
    f <- Editor.getKeyMap
    let loop = do c <- getcE
                  f c
                  UI.refresh
                  loop
    loop

    -- handler :: Control.Exception.Exception -> IO ()
    -- handler = Control.Exception.throwIO

-- ---------------------------------------------------------------------

-- | Quit
--
quitE :: IO ()
quitE = exitWith ExitSuccess

-- | Refresh the screen
--
refreshE :: IO ()
refreshE = UI.refresh

-- | Do nothing
--
noopE :: IO ()
noopE = return ()

------------------------------------------------------------------------

--
-- | Move cursor left 1
--
leftE :: IO ()
leftE = withBuffer leftB

--
-- | Move cursor right 1
--
rightE :: IO ()
rightE = withBuffer rightB

--
-- | Move cursor to origin
--
topE :: IO ()
topE = withBuffer $ \b -> moveTo b 0

--
-- | Move cursor to end of buffer
--
botE :: IO ()
botE = withBuffer $ \b -> sizeB b >>= \n -> moveTo b (n-1)

--
-- | Move cursor to start of line
--
solE :: IO ()
solE = withBuffer moveToSol

--
-- | Move cursor to end of line
--
eolE :: IO ()
eolE = withBuffer moveToEol

--
-- | Move cursor down 1 line
--
downE :: IO ()
downE = withBuffer $ \b -> do
    x <- offsetFromSol b
    moveToEol b
    rightB b        -- now at start of next b
    moveXorEol b x  

--
-- | Move cursor up to the same point on the previous line
--
upE :: IO ()
upE = withBuffer $ \b -> do
    x <- offsetFromSol b
    moveToSol b
    leftB b
    moveToSol b
    moveXorEol b x  

--
-- | Move left @x@ or to start of line
--
leftOrSolE :: Int -> IO ()
leftOrSolE x = withBuffer $ \b -> moveXorSol b x

--
-- | Move right @x@ or to end of line
--
rightOrEolE :: Int -> IO ()
rightOrEolE x = withBuffer $ \b -> moveXorEol b x

--
-- | Read into a *new* buffer the contents of file.
-- TODO: change type.
--
newE  :: FilePath -> IO ()
newE f = readFile f >>= Editor.newBuffer f

{-
-- works. but if you move hClose *before* the call to newBuffer -- bad!
-- so readFile does exactly what we want, but nicely.
        hd <- openFile f ReadMode
        cs <- hGetContents hd
        Editor.newBuffer f cs       -- lazy for large files?
        hClose hd                   -- must come after we create the new buffer
-}

--
-- | Write current buffer to disk
--
writeE :: IO ()
writeE = withBuffer $ \b -> do
        let f = nameB b
        ss <- elemsB b
        h  <- openFile f WriteMode
        hPutStr h ss
        hClose h

--
-- | Read file into buffer starting a current point
--
readE :: IO ()
readE = error "readE is undefined"

------------------------------------------------------------------------

--
-- | Shift focus to next buffer
--
nextE :: IO ()
nextE = Editor.nextBuffer

--
-- | Shift focus to prev buffer
--
prevE :: IO ()
prevE = Editor.prevBuffer

------------------------------------------------------------------------

-- | Replace buffer at point with next char
replaceE :: IO ()
replaceE = withBuffer $ \b -> do
    c <- UI.getKey UI.refresh
    if isLatin1 c then writeB b c
                  else noopE        -- TODO

-- | Insert new character
insertE :: Char -> IO ()
insertE c = withBuffer $ \b -> do
    case c of
        '\13'          -> insertB b '\n'
        _ | isLatin1 c -> insertB b c
          | otherwise  -> noopE  -- TODO

-- | Delete character under cursor
deleteE :: IO ()
deleteE = withBuffer deleteB

-- | Kill to end of line
killE :: IO ()
killE = withBuffer deleteToEol -- >>= Buffer.prevXorLn 1

-- | Draw message at bottom of screen
msgE :: String -> IO ()
msgE = UI.drawCmd

-- | Clear the message line at bottom of screen
msgClrE  :: IO ()
msgClrE = UI.clearCmd
