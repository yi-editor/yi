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

--
-- | The core edsl and machine of hemacs. This module is the link
-- between the editor machine defined in 'HEmacs.Editor', and the real
-- world defined in 'HEmacs.UI'. The instructions defined here
-- manipulate the editor state, and control the screen through the UI.
-- Key bindings, and libraries should manipulate HEmacs through the
-- interface defined here.
--
module HEmacs.Core (

        -- * Main loop
        start,
        end,
        eventLoop,

        -- * Editor actions
        e_quit,
        e_refresh,
        e_noop,
        e_load,
        e_left,
        e_right,
        e_up,
        e_down,
        e_eol,
        e_sol,
        e_top,
        e_bot,

   ) where

import HEmacs.Editor
import qualified HEmacs.Editor as Editor
import qualified HEmacs.UI     as UI
import qualified HEmacs.Buffer as Buffer

import System.IO        ( openFile, hGetContents, IOMode(..), )
import System.Exit

import qualified Control.Exception ( catch, Exception, throwIO )

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
--
start :: Editor.Config -> Maybe [FilePath] -> IO ()
start confs mfs = do
    UI.start
    size <- UI.screenSize
    Editor.setScreenSize size
    Editor.setUserSettings confs
    case mfs of
        Nothing -> Editor.newBuffer "undefined" [[]]
        Just fs -> mapM_ e_load fs
    e_refresh

--
-- | shutdown the editor
--
end :: IO ()
end = UI.end

-- ---------------------------------------------------------------------
-- | The editor main loop. Read key strokes from the ui and interpret
-- them using the current key map. Keys are bound to core actions.
-- The state is threaded explicitly at the moment.
--
eventLoop :: IO ()
eventLoop = do 
    km <- Editor.getKeyMap
    eventLoop' km
--  UI.withInvisibleCursor $ eventLoop' km

-- 
-- | read a keystroke, and interpret it using the current key mappings.
-- The actions keys may be bound to are given in $events.
-- 
eventLoop' :: Editor.KeyMap -> IO ()
eventLoop' keyhandler = do
    k <- UI.getKey UI.refresh
    Control.Exception.catch (keyhandler k) (handler)
    UI.refresh
    eventLoop' keyhandler
  where
        handler :: Control.Exception.Exception -> IO ()
        handler = Control.Exception.throwIO

-- ---------------------------------------------------------------------
-- | Editor actions. This is the instruction set of the editor core.
--

--
-- | Quit
--
e_quit :: IO ()
e_quit = exitWith ExitSuccess

--
-- | Refresh the screen
--
e_refresh :: IO ()
e_refresh = UI.refresh

--
-- | Do nothing
--
e_noop :: IO ()
e_noop = return ()

--
-- | Move cursor left 1
--
e_left :: IO ()
e_left = withBuffer $ \b -> return $ Buffer.left b

--
-- | Move cursor right 1
--
e_right :: IO ()
e_right = withBuffer $ \b -> return $ Buffer.right b

--
-- | Move cursor up 1
--
e_up :: IO ()
e_up = withBuffer $ \b -> return $ Buffer.up b

--
-- | Move cursor down 1
--
e_down :: IO ()
e_down = withBuffer $ \b -> return $ Buffer.down b

--
-- | Move cursor to end of line
--
e_eol :: IO ()
e_eol = withBuffer $ \b -> 
            case Buffer.point b of 
                (_,y) -> return $ b `Buffer.moveto` (Buffer.width b, y)

--
-- | Move cursor to start of line
--
e_sol :: IO ()
e_sol = withBuffer $ \b -> 
            case Buffer.point b of (_,y) -> return $ b `Buffer.moveto` (0, y)

--
-- | Move cursor to origin
--
e_top :: IO ()
e_top = withBuffer $ \b -> return $ b `Buffer.moveto` (0, 0)

--
-- | Move cursor to origin
--
e_bot :: IO ()
e_bot = withBuffer $ \b -> return $ b `Buffer.moveto` (0, Buffer.height b - 1)

--
-- | Load a new buffer with contents of file
--
e_load  :: FilePath -> IO ()
e_load f = do
        h <- openFile f ReadWriteMode
        s <- hGetContents h            -- close it?
        Editor.newBuffer f $ lines s   -- lazy for large files?

