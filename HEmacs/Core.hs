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

   ) where

import HEmacs.Editor
import qualified HEmacs.Editor as Editor
import qualified HEmacs.UI     as UI

import Data.Char        ( ord, chr ) 
import Data.Maybe       ( isJust, fromJust )
import Data.List        ( lines )
import Control.Monad    ( when )
import qualified Control.Exception ( catch, Exception )

import System.IO        ( openFile, hGetContents, IOMode(..), )

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
--
start :: Editor.Config -> Maybe [FilePath] -> IO ()
start confs fs = do
    UI.start
    size@(h,w) <- UI.screenSize
    Editor.setScreenSize size
    Editor.setUserSettings confs
    case fs of
        Nothing   -> Editor.newBuffer "undefined" []
        Just (fs) -> mapM_ e_load fs >> e_refresh >> return ()

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
    UI.withInvisibleCursor $ eventLoop' km

-- 
-- | read a keystroke, and interpret it using the current key mappings.
-- The actions keys may be bound to are given in $events.
-- 
eventLoop' :: Editor.KeyMap -> IO ()
eventLoop' keyhandler = do
    k <- UI.getKey UI.refresh
    s <- Control.Exception.catch (keyhandler k) (do_except)
    case s of
        EQuit -> return ()
        EOk   -> UI.refresh >> eventLoop' keyhandler

    where
        do_except :: Control.Exception.Exception -> IO EventStatus
        do_except e = return EQuit

-- ---------------------------------------------------------------------
-- | Editor actions. This is the instruction set of the editor core.
--

--
-- | Quit
--
e_quit :: IO EventStatus
e_quit = return EQuit

--
-- | Refresh the screen
--
e_refresh :: IO EventStatus
e_refresh = UI.refresh >> return EOk

--
-- | Do nothing
--
e_noop :: IO EventStatus
e_noop = return EOk

--
-- | Load a new buffer with contents of file
--
e_load  :: FilePath -> IO EventStatus
e_load f = do
        h <- openFile f ReadWriteMode
        s <- hGetContents h            -- close it?
        Editor.newBuffer f $ lines s   -- lazy for large files?
        return EOk

