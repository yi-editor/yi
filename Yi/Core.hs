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
-- | The core edsl and machine of yi. This module is the link
-- between the editor machine defined in 'Yi.Editor', and the real
-- world defined in 'Yi.UI'. The instructions defined here
-- manipulate the editor state, and control the screen through the UI.
-- Key bindings, and libraries should manipulate Yi through the
-- interface defined here.
--
module Yi.Core (

        -- * Main loop
        start,
        end,
        eventLoop,

        -- * Editor actions
        e_quit,
        e_refresh,
        e_noop,

        -- * File related
        e_load,

        -- * Cursor movement
        e_left,
        e_right,
        e_top,
        e_bot,
        e_down,
        e_up,
        e_sol,
        e_eol,

        -- * Buffer editing
        e_replace,
        e_insert,
        e_delete,
        e_kill

   ) where

import Yi.Editor
import Yi.MkTemp
import qualified Yi.Editor as Editor
import qualified Yi.UI     as UI
import qualified Yi.Buffer as Buffer

import System.IO
import System.Exit

import qualified Control.Exception ( catch, Exception, throwIO )

import GHC.Base

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
--
start :: Editor.Config -> Maybe [FilePath] -> IO ()
start confs mfs = do
    UI.start
    Editor.setUserSettings confs
    case mfs of
        Just fs -> mapM_ e_load fs

        -- vi-like behaviour, just for now.
        Nothing -> do mf <- mkstemp "/tmp/yi.XXXXXXXXXX" 
                      case mf of
                        Just (f,h) -> do hClose h
                                         Editor.fillNewBuffer f
                        Nothing -> error "Core.start: mkstemp failed"
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

------------------------------------------------------------------------

--
-- | Move cursor left 1
--
e_left :: IO ()
e_left = modifyCurrentBuffer Buffer.left

--
-- | Move cursor right 1
--
e_right :: IO ()
e_right = modifyCurrentBuffer Buffer.right

--
-- | Move cursor to origin
--
e_top :: IO ()
e_top = modifyCurrentBuffer (Buffer.moveTo 0)

--
-- | Move cursor to end of buffer
--
e_bot :: IO ()
e_bot = modifyCurrentBuffer $ \b -> do
            i <- Buffer.size b
            Buffer.moveTo (i-1) b

--
-- | Move cursor down 1 line
--
e_down :: IO ()
e_down = modifyCurrentBuffer $ \b -> do
    x <- Buffer.prevNLOffset_ b
    Buffer.nextNL b
    Buffer.nextXorNL b (x+1)

--
-- | Move cursor up to the same @x@ offset, 1 line previous
--
e_up :: IO ()
e_up = modifyCurrentBuffer $ \b -> do
    x <- Buffer.prevNLOffset b -- prev \n
    Buffer.left b   -- blargh
    Buffer.prevNL b
    p <- Buffer.point b -- if we're at '0', don't shift
    Buffer.nextXorNL b (x + min p 1)

--
-- | Move cursor to start of line (blargh)
--
e_sol :: IO ()
e_sol = modifyCurrentBuffer $ \b -> Buffer.left b >>= Buffer.prevNL >>= Buffer.right

--
-- | Move cursor to end of line
--
e_eol :: IO ()
e_eol = modifyCurrentBuffer Buffer.nextNL

--
-- | Load a new buffer with contents of file
-- TODO: change type
--
e_load  :: FilePath -> IO ()
e_load f = Editor.fillNewBuffer f          -- lazy for large files?

------------------------------------------------------------------------

--
-- | Replace buffer at point with next char
--
e_replace :: IO ()
e_replace = modifyCurrentBuffer $ \b -> do
    k <- UI.getKey UI.refresh   -- read next key
    case k of
        Key c -> Buffer.replace c b
        _     -> e_noop >> return b -- TODO

-- | Insert new character
e_insert :: IO ()
e_insert = modifyCurrentBuffer $ \b -> do
    k <- UI.getKey UI.refresh
    case k of
        Key c -> Buffer.insert c b
        _     -> e_noop >> return b -- TODO

-- | Delete character under cursor
e_delete :: IO ()
e_delete = modifyCurrentBuffer $ Buffer.delete

-- | Kill to end of line
e_kill :: IO ()
e_kill = modifyCurrentBuffer $ Buffer.killToEOL
