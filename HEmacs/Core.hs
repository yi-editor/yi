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
        e_fallback

   ) where

import HEmacs.Editor
import qualified HEmacs.Editor as Editor
import qualified HEmacs.UI     as UI

-- deprecated
import qualified HEmacs.Curses as Curses

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
        Nothing   -> Editor.newBuffer "empty buffer" []
        Just (fs) -> mapM_ e_load_ fs >> e_refresh >> return ()

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
    Curses.withCursor Curses.CursorInvisible $ eventLoop' km

-- 
-- | read a keystroke, and interpret it using the current key mappings.
-- The actions keys may be bound to are given in $events.
-- 
eventLoop' :: Editor.KeyMap -> IO ()
eventLoop' keyhandler = do
    k <- UI.getKey UI.refresh
    c <- Control.Exception.catch (keyhandler k) (do_except)
    case c of
        EQuit -> return ()
        EOk   -> Curses.refresh >> eventLoop' keyhandler

--
-- | deal with exceptions
--
do_except :: Control.Exception.Exception -> IO EditStatus
do_except e = Curses.beep >> cont
--do_except e = Curses.beep >> {-UI.do_message undefined {-attr_error-} (show e) >>-} cont_clr

------------------------------------------------------------------------
--
-- | continue, don't redraw
--
cont :: IO EditStatus
cont = return EOk

{-
--
-- | redraw and continue
--
cont_clr :: IO ELCont
cont_clr = return $ EOk
-}

--
-- | refresh screen now and continue
--
cont_refresh :: IO EditStatus
cont_refresh = UI.refresh >> cont

--
-- | quit
--
nocont :: IO EditStatus
nocont = return $ EQuit

-- ---------------------------------------------------------------------
-- | What actions may user's call, or bind keys to? This is essentially
-- the editor core language. TODO: this should look much like a real,
-- targettable core language for manipulating the editor machine.
--

--
-- | quit, but warn if unsaved. this is too high-level, in fact.
--
e_quit :: IO EditStatus
e_quit = nocont
    -- unsaved <- getUnsavedStatus
    --if unsaved then UI.warn "No write since last change" >> cont else nocont

--
-- | refresh the screen and continue
--
e_refresh :: IO EditStatus
e_refresh = UI.refresh >> cont

--
-- | noop
--
e_none :: IO EditStatus
e_none = cont

-- ---------------------------------------------------------------------
-- | load the current buffer with contents of file 'f'
--
e_load :: FilePath -> IO EditStatus
e_load f = do
        h <- openFile f ReadWriteMode
        s <- hGetContents h            -- close it?
        Editor.newBuffer f $ lines s   -- lazy for large files?
        UI.refresh                     -- probably should redraw now
        return EOk          

--
-- | load the current buffer, no refresh
--
e_load_  :: FilePath -> IO EditStatus
e_load_ f = do
        h <- openFile f ReadWriteMode
        s <- hGetContents h            -- close it?
        Editor.newBuffer f $ lines s   -- lazy for large files?
        return EOk          

------------------------------------------------------------------------
--
-- | a key sequence with a submapping -- read the next key and work out
-- what to do. this is a lookahead.
--
e_submap :: String -> (Key -> Action) -> IO EditStatus
e_submap info submap = do
    -- UI.draw_submap
    Curses.refresh
    k <- UI.getKey UI.refresh {-refresh_fn-}
    --draw_botinfo
    submap k

--
-- | Check if control is set and call handle_key again with the plain key.
--
e_fallback handle_key (Key k)
    | ord '\^A' <= ord k && ord k <= ord '\^Z' 
    = handle_key (Editor.Key $ chr $ ord k - ord '\^A' + ord 'a')

e_fallback _ _ = e_none

------------------------------------------------------------------------
--

-- e_fallback = undefined
e_actentry = undefined
e_clear_tags = undefined
e_collapse = undefined
e_delentry = undefined
e_delete_tagged = undefined
e_editentry = undefined
e_firstentry = undefined
e_hscroll = undefined
e_lastentry = undefined
e_move_tagged_after = undefined
e_move_tagged_before = undefined
e_move_tagged_under = undefined
e_newentry = undefined
e_newunder = undefined
e_nextentry = undefined
e_pgdnentry = undefined
e_pgdn = undefined
e_pgup = undefined
e_pgupentry = undefined
e_preventry = undefined
e_redo = undefined
e_save = undefined
e_tag = undefined
e_undo = undefined

{-
goto_next_or_redraw :: IO ELCont
goto_next_or_redraw s =
    if (selected_entry s) + 1 < (n_entries_viewed s) then
        e_nextentry s
    else draw_entries s >> cont s

-- ---------------------------------------------------------------------
-- Text view manipulation

update_scroll snew = do
    draw_textarea snew
    draw_botinfo snew
    cont snew

e_vscroll :: Int -> IO ELCont
e_vscroll amount s = update_scroll (do_vscroll amount s)

e_hscroll :: Int -> IO ELCont
e_hscroll amount s = update_scroll (do_hscroll amount s)

e_pgdn :: Entry a => Status a -> IO (ELCont a)
e_pgdn s = e_vscroll ((textarea_height s) `div` 2) s

e_pgup :: Entry a => Status a -> IO (ELCont a)
e_pgup s = e_vscroll (-(textarea_height s) `div` 2) s

e_eob :: Entry a => Status a -> IO (ELCont a)
e_eob s = update_scroll s{textarea_vscroll = max_vscroll s}

e_bob :: Entry a => Status a -> IO (ELCont a)
e_bob s = update_scroll s{textarea_vscroll = 0}

-}
