--
-- riot/UI.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

module HEmacs.UI (

        HEmacs.UI.init,
        deinit,
        event_loop,
        set_topinfo,
        set_entries,
        set_callbacks,
        change_entry_type,
        refresh,
        -- Event handlers
        e_vscroll, e_hscroll, e_pgdn, e_pgup, e_eob, e_bob,
        e_selectentry, e_nextentry, e_preventry, 
        e_pgdnentry, e_pgupentry, e_firstentry, e_lastentry,
        e_actentry, e_delentry, e_editentry, e_collapse,
        e_quit, e_refresh, e_none, e_submap, e_fallback,
        e_newentry, e_newunder, e_undo, e_redo,
        e_tag, e_clear_tags, e_delete_tagged,
        e_move_tagged_after, e_move_tagged_before, e_move_tagged_under, 
        e_save,
        -- Classes & data structures
        Status,
        ELCont

   ) where

import HEmacs.Entry
import HEmacs.Style     (UIAttr(..), init_uiattr, default_uiattr, StyleSpec)
import HEmacs.CWString  (withLCString, peekLCString)
import qualified HEmacs.Editor as Editor
import qualified HEmacs.Curses as Curses

import Data.Maybe
import Data.Char
import Data.List        (find,init)
import Time             (getClockTime, toCalendarTime, CalendarTime)
import System.IO        ( stdin )

import Control.Monad    (liftM)
import qualified Control.Exception

import Foreign.C.String (withCString, peekCString)

#ifndef NO_POSIX
import System.Posix.Signals
#endif

-- Helper functions {{{

text_n_lines :: String -> Int
text_n_lines text =
    length (lines text)

text_n_columns :: String -> Int
text_n_columns text =
    foldl max 0 (map length (lines text))

safetail [] = []
safetail (x:xs) = xs

maybehead [] = Nothing
maybehead (x:xs) = Just x

left_align w t =
    take w (t ++ repeat ' ')

lnth n l = lookup n $ zip [0..] l

mpass :: (a -> b) -> Maybe a -> Maybe b
mpass f Nothing = Nothing
mpass f (Just x) = Just $ f x

-- }}}


-- Status {{{

data Entry a => UndoInfo a = UndoInfo {
    undo_selected_entry :: Int,
    undo_active_entry :: Maybe Int,
    undo_unsaved_changes :: Bool,
    undo_entries :: [EntryTree a]
}

undo_count = 100

--
-- editor state?
--

data Entry a => Status a = Status {
    attr :: UIAttr,
    topinfo_text :: String,
    textarea_hscroll,
    textarea_vscroll,
    entryarea_vscroll :: Int,
    screen_width, 
    screen_height :: Int,
    entries :: [EntryTree a],
    entries_fname :: Maybe String,
    selected_entry :: Int,
    active_entry :: Maybe Int,
    undo_buffer,
    redo_buffer :: [UndoInfo a],
    save_callback :: String -> [EntryTree a] -> IO (),
    new_callback :: String -> CalendarTime -> IO (a),
    unsaved_changes :: Bool
}

new_status :: Entry a => Status a
new_status = Status {
    attr = default_uiattr,
    topinfo_text = "--",
    textarea_hscroll = 0,
    textarea_vscroll = 0,
    entryarea_vscroll = 0,
    screen_width = 80,
    screen_height = 24,
    entries = [],
    entries_fname = Nothing,
    selected_entry = 0,
    active_entry = Nothing,
    undo_buffer = [],
    redo_buffer = [],
    save_callback = save_disabled,
    new_callback = new_disabled,
    unsaved_changes = False
}

new_disabled _ _ = error "No handler to create new entries set."
save_disabled _ _ = error "No save handler set."

set_topinfo :: Entry a => Status a -> String -> Status a
set_topinfo s v = s{topinfo_text = v}

set_entries :: Entry a => Status a -> [EntryTree a] -> (Maybe String) -> Status a
set_entries s e fname =
    s{
        entries = e,
        entries_fname = fname,
        selected_entry = 0,
        active_entry = Nothing,
        entryarea_vscroll = 0,
        textarea_vscroll = 0,
        textarea_hscroll = 0,
        unsaved_changes = False,
        undo_buffer = [],
        redo_buffer = []
    }

change_entry_type :: (Entry a, Entry b) => Status a -> Status b
change_entry_type s =
    new_status{
        attr = attr s,
        topinfo_text = topinfo_text s,
        screen_width = screen_width s,
        screen_height = screen_height s
    } 

set_callbacks :: Entry a =>
    Status a
    -> (String -> [EntryTree a] -> IO ())
    -> (String -> CalendarTime -> IO (a))
    -> Status a
set_callbacks s save_cb new_cb =
    s{save_callback = save_cb, new_callback = new_cb}

-- }}}


-- Sizes {{{

entryarea_height s = max 1 $ (screen_height s - 3) `div` 3
topinfo_line s = 0
midinfo_line s = (entryarea_height s) + 1
botinfo_line s = max (screen_height s - 1) (midinfo_line s + 2)
entryarea_startline s = 1
entryarea_endline s = midinfo_line s - 1
textarea_startline s = midinfo_line s + 1
textarea_endline s = botinfo_line s - 1
textarea_height status = (textarea_endline status) - (textarea_startline status) + 1

-- }}}


-- Viewlist indexing {{{

valid_entry s e = 0<=e && e<n_entries_viewed s

entrytree_viewlist :: Entry a => [EntryTree a] -> [(Loc, EntryTree a)]
entrytree_viewlist et =
    entrytree_map f et
    where
       f e chview loc =
           if entrytree_expanded e then
               (loc, e):chview
           else
               [(loc, e)]

entries_viewlist s = entrytree_viewlist (entries s)
entries_viewlist_plain s = snd $ unzip $ entries_viewlist s
entries_viewlist_loc s = fst $ unzip $ entries_viewlist s

viewlist_entry s e = lnth e (entries_viewlist_plain s)
viewlist_entry_loc s e = fromJust $ lnth e (entries_viewlist_loc s)
selected_entry_loc s = viewlist_entry_loc s (selected_entry s)
active_entry_loc s = mpass (viewlist_entry_loc s) (active_entry s)

n_entries_viewed s = length $ entries_viewlist s

find_viewlist_pos et loc = 
    viewlist_pos 0 et loc
    where
        viewlist_pos cpos (e:_) (Loc (0:[])) = cpos
        viewlist_pos cpos (e:_) (Loc (0:ll)) =
            case entrytree_expanded e of
                False -> cpos -- Go to parent
                True -> viewlist_pos (cpos+1) (entrytree_children e) (Loc ll)
        viewlist_pos cpos (e:et) (Loc (l:ll)) =
            viewlist_pos (cpos+1+nchv) et $ Loc ((l-1):ll)
            where
                nchv = case entrytree_expanded e of
                           False -> 0
                           True -> length $ entrytree_viewlist $ entrytree_children e
        viewlist_pos _ _ _ = error "Invalid location"

-- }}}


-- Initialisation {{{

get_size :: Entry a => Status a -> IO (Status a)
get_size s = do
    (h, w) <- Curses.scrSize
    return s{screen_width = w, screen_height = h}

init :: Entry a => [StyleSpec] -> IO (Status a)
init styles = do
    Curses.initCurses
    Curses.keypad Curses.stdScr True
    a <- init_uiattr styles
    get_size new_status{attr = a}

deinit :: IO ()
deinit = do
    Curses.endWin
    return ()
 
-- }}}


-- Drawing {{{

waddstr w s = Control.Exception.try (Curses.wAddStr w s) >> return ()

cset_attr (a, p) = do
    Curses.wAttrSet Curses.stdScr (a, p)

creset_attr = do
    cset_attr (Curses.attr0, Curses.Pair 0)

fill_to_eol = do
    (h, w) <- Curses.scrSize
    (y, x) <- Curses.getYX Curses.stdScr
    waddstr Curses.stdScr (replicate (max 0 (w-x)) ' ')

draw_lines s d l_first l_last skip f = do
    Curses.wMove Curses.stdScr l_first 0
    do_draw_lines s (drop skip d) l_first l_last skip f
    where
        do_draw_lines s d l l_last nr f
            | l>l_last = return ()
            | otherwise = do
                f s (maybehead d) nr
                do_draw_lines s (safetail d) (l+1) l_last (nr+1) f


-- Text area

textarea_text :: Entry a => Status a -> String
textarea_text s =
    case active_entry s of
        Nothing -> []
        Just e -> maybe "" entry_text $ viewlist_entry s e

next_tab_stop pos = ((pos `div` 8) + 1) * 8

do_tab_stops pos [] = []
do_tab_stops pos ('\t':ss) = 
    replicate (nxt - pos) ' ' ++ (do_tab_stops nxt ss)
    where
        nxt = next_tab_stop pos
do_tab_stops pos (s:ss) = s:(do_tab_stops (pos + 1) ss)

do_draw_text s text hs vs sl el = do
    cset_attr (attr_text $ attr s)
    draw_lines s (map (do_tab_stops 0) $ lines text) sl el vs drawl
    creset_attr
    where
        w = screen_width s
        drawl s Nothing _ = 
            fill_to_eol
        drawl s (Just l) _ = 
            waddstr Curses.stdScr (take w $ drop hs $ l ++ repeat ' ')

do_draw_textarea s text = do
    do_draw_text s text hs vs sl el
    where
        hs = textarea_hscroll s
        vs = textarea_vscroll s
        sl = textarea_startline s
        el = textarea_endline s

draw_textarea :: Entry a => Status a -> IO ()
draw_textarea status =
    do_draw_textarea status (textarea_text status)

-- Info lines

botinfo_text :: Entry a => Status a -> String
botinfo_text s =
    case active_entry s of
        Nothing -> "--"
        Just e -> maybe "--" entry_title $ viewlist_entry s e


midinfo_text :: Entry a => Status a -> String
midinfo_text s = 
    fromMaybe "(no file)" (entries_fname s)
    ++ if unsaved_changes s then " [modified]" else ""


do_draw_infoline_align status line left right = do
    Curses.wMove Curses.stdScr line 0
    cset_attr (attr_infoline $ attr status)
    waddstr Curses.stdScr ((take n_l left_)++(take n_r right_))
    creset_attr
    where
        left_ = left ++ (repeat ' ')
        right_ = ' ':right
        w = screen_width status
        n_r = min w (length right_)
        n_l = max 0 (w-n_r)

do_draw_infoline status line text = 
    do_draw_infoline_align status line text ""

mk_n_of_m n m = 
    "("++(show n)++"/"++(show m)++") "

draw_topinfo :: Entry a => Status a -> IO ()
draw_topinfo s =
    do_draw_infoline s (topinfo_line s) (topinfo_text s) 

draw_midinfo :: Entry a => Status a -> IO ()
draw_midinfo s =
    do_draw_infoline_align s l t (mk_n_of_m n m)
    where
        l = midinfo_line s
        t = midinfo_text s
        m = length (entries_viewlist_plain s)
        n = 1 + selected_entry s
       
draw_botinfo :: Entry a => Status a -> IO ()
draw_botinfo s =
    do_draw_infoline_align s l t (mk_n_of_m n m)
    where
        l = botinfo_line s
        t = botinfo_text s
        m = text_n_lines (textarea_text s)
        n = min m $ (textarea_vscroll s)+(textarea_height s)

-- Entries

entry_attr s nr =
    case (nr == selected_entry s, Just nr == active_entry s) of
        (False, False) -> attr_entry $ attr s
        (True, False)  -> attr_entry_sel $ attr s
        (False, True)  -> attr_entry_act $ attr s
        (True, True)   -> attr_entry_act_sel $ attr s

do_draw_entry s Nothing _ = do
    cset_attr (attr_entry $ attr s)
    fill_to_eol
    creset_attr

do_draw_entry s (Just (Loc loc, e)) nr = do
    cset_attr (entry_attr s nr)
    waddstr Curses.stdScr (left_align w l)
    creset_attr
    where
        w = screen_width s
        bullet = case (entrytree_children e) of
                     [] -> " - " --[' ', Curses.bullet, ' ']
                     otherwise -> " + "
        indent = replicate (3 * (length loc - 1)) ' '
        tg = case entrytree_tagged e of
                 True -> "*"
                 False -> " "
        flags = left_align 4 (entry_flags e)
        l = concat [" ", flags, tg, " ", indent, bullet, entry_title e]

draw_entries :: Entry a => Status a -> IO ()
draw_entries s = 
    draw_lines s (entries_viewlist s) sl el vs do_draw_entry
    where
        vs = entryarea_vscroll s
        sl = entryarea_startline s
        el = entryarea_endline s

-- Refresh

redraw :: Entry a => Status a -> IO ()
redraw status = do
    --Curses.wclear Curses.stdScr
    draw_topinfo status
    draw_entries status
    draw_midinfo status
    draw_textarea status
    draw_botinfo status

refresh s = redraw s >> Curses.refresh

-- }}}


-- Text area scrolling {{{

max_scroll_ :: Int -> Int -> Int
max_scroll_ item_s view_s =
    max 0 (item_s - view_s)

text_max_hscroll :: String -> (Int, Int) -> Int
text_max_hscroll text (_, view_w) =
    max_scroll_ (text_n_columns text) view_w

text_max_vscroll :: String -> (Int, Int) -> Int
text_max_vscroll text (view_h, _) =
    max_scroll_ (text_n_lines text) view_h

textarea_size :: Entry a => Status a -> (Int, Int)
textarea_size s =
    (textarea_height s, screen_width s)

calc_scroll s asc csc_fn msc_fn = 
    max 0 (min msc (csc + asc))
    where
        msc = msc_fn (textarea_text s) (textarea_size s)
        csc = csc_fn s

do_hscroll :: Entry a => Int -> Status a -> Status a
do_hscroll amount s = 
    s{textarea_hscroll = sc}
    where
        sc = calc_scroll s amount textarea_hscroll text_max_hscroll

do_vscroll :: Entry a => Int -> Status a -> Status a
do_vscroll amount s = 
    s{textarea_vscroll = sc}
    where
        sc = calc_scroll s amount textarea_vscroll text_max_vscroll

max_vscroll :: Entry a => Status a -> Int
max_vscroll s =
    text_max_vscroll (textarea_text s) (textarea_height s, screen_width s)

max_hscroll :: Entry a => Status a -> Int
max_hscroll s =
    text_max_hscroll (textarea_text s) (textarea_height s, screen_width s)

check_vscroll :: Entry a => Status a -> Status a
check_vscroll s =
    case textarea_vscroll s > max_vscroll s of
        True -> s{textarea_vscroll = max_vscroll s}
        False -> s

check_hscroll :: Entry a => Status a -> Status a
check_hscroll s =
    case textarea_hscroll s > max_hscroll s of
        True -> s{textarea_hscroll = max_hscroll s}
        False -> s

check_textarea :: Entry a => Status a -> Status a
check_textarea = check_vscroll . check_hscroll

-- }}}


-- Entry selection {{{

check_active :: Entry a => Status a -> Status a
check_active s =
    case active_entry s of
        Just e | e >= n_entries_viewed s -> s{active_entry = Nothing}
        otherwise -> s
        
check_selected :: Entry a => Status a -> Status a
check_selected s =
    case selected_entry s >= n_entries_viewed s of
        True -> s{selected_entry = max 0 $ (n_entries_viewed s) - 1}
        false -> s

check_e_vscroll :: Entry a => Status a -> Status a
check_e_vscroll s =
    do_selectentry (selected_entry s) s

check_entryarea :: Entry a => Status a -> Status a
check_entryarea s = check_e_vscroll $ check_selected $ check_active s

check_everything :: Entry a => Status a -> Status a
check_everything = check_textarea . check_entryarea

do_selectentry n s | valid_entry s n =
    if n > l_e then
        s{selected_entry = n, entryarea_vscroll = n - h + 1}
    else if n < f_e then
        s{selected_entry = n, entryarea_vscroll = n}
    else
        s{selected_entry = n}
    where
        n_entries = length (entries_viewlist s)
        h = entryarea_height s
        f_e = entryarea_vscroll s
        l_e = f_e + h - 1
do_selectentry 0 s | n_entries_viewed s == 0 =
    s{selected_entry = 0, entryarea_vscroll = 0}
do_selectentry _ _ = error "Invalid entry"

do_actentry s e | valid_entry s e =
    s{active_entry = Just e, textarea_vscroll = 0, textarea_hscroll = 0}

-- }}}


-- Expand/collapse {{{

update_entries s et = 
    s{entries = et, active_entry = ae, selected_entry = se}
    where
        ae = mpass (find_viewlist_pos et) (active_entry_loc s)
        se = find_viewlist_pos et (selected_entry_loc s)

do_colexp_loc :: Entry a =>
    ([EntryTree a] -> Loc -> Maybe [EntryTree a]) -> Status a -> Loc
    -> Status a
do_colexp_loc fn s loc =
    case fn (entries s) loc of
        Nothing -> s
        Just et -> check_everything $ update_entries s et

do_colexp :: Entry a =>
    ([EntryTree a] -> Loc -> Maybe [EntryTree a]) -> Status a -> Int
    -> Status a
do_colexp fn s e = do_colexp_loc fn s $ viewlist_entry_loc s e

do_expand :: Entry a => Status a -> Int -> Status a
do_expand s e | valid_entry s e =
    do_colexp entrytree_expand s e


do_collapse :: Entry a => Status a -> Int -> Status a
do_collapse s e | valid_entry s e =
    do_colexp entrytree_collapse s e


do_collapse_p :: Entry a => Status a -> Int -> Status a
do_collapse_p s e | valid_entry s e =
    do_colexp entrytree_collapse_p s e

-- }}}


-- Undo/redo/save {{{

mk_snapshot :: Entry a => Status a -> UndoInfo a
mk_snapshot s =
    UndoInfo (selected_entry s) (active_entry s)
             (unsaved_changes s) (entries s)
    
snapshot :: Entry a => Status a -> Status a
snapshot s =
    s{undo_buffer = nub, redo_buffer=[], unsaved_changes=True}
    where
        (rb, ub) = (redo_buffer s, undo_buffer s)
        nub = take undo_count $ (mk_snapshot s):(rb ++ reverse rb ++ ub)

use_snapshot :: Entry a => 
    Status a -> UndoInfo a -> [UndoInfo a] -> [UndoInfo a] -> Status a
use_snapshot s (UndoInfo sel act unsaved et) ub rb =
    check_entryarea $ s{
        undo_buffer = ub,
        redo_buffer = rb,
        entries = et,
        selected_entry = sel,
        active_entry = act,
        textarea_vscroll = 0,
        textarea_hscroll = 0,
        unsaved_changes = unsaved
    }

mark_saved :: Entry a => Status a -> Status a
mark_saved s =
    s{
        unsaved_changes = False,
        undo_buffer = map set_unsaved (undo_buffer s),
        redo_buffer = map set_unsaved (redo_buffer s)
    }
    where
        set_unsaved u = u{undo_unsaved_changes = True}

undo :: Entry a => Status a -> Status a
undo s =
    case undo_buffer s of 
        [] -> s
        (su:ss) -> use_snapshot s su ss ((mk_snapshot s):(redo_buffer s))


redo :: Entry a => Status a -> Status a
redo s =
    case redo_buffer s of 
        [] -> s
        (sr:ss) -> use_snapshot s sr ((mk_snapshot s):(undo_buffer s)) ss

do_save :: Entry a => Status a -> String -> IO (Status a)
do_save s fname = do 
    save_callback s fname (entries s)
    return (mark_saved s)

save :: Entry a => Status a -> IO (Status a)
save s = do
    case entries_fname s of
        Nothing -> error "No file name set."
        Just fname -> do_save s fname
        
-- }}}


-- Entry and entry tree manipulation {{{

rm_new_loc_vl [] rmlocv = Nothing
rm_new_loc_vl ((loc, _):vv) rmlocv =
    case loc_rm_effect loc rmlocv of
        Nothing -> rm_new_loc_vl vv rmlocv
        just_loc -> just_loc
    
rm_new_loc :: Entry a => Status a -> [Loc] -> Int -> Maybe Loc
rm_new_loc s rmlocv e =
    case loc_rm_effect loc rmlocv of
        Nothing -> 
            case rm_new_loc_vl vl_after rmlocv of
                Nothing -> rm_new_loc_vl vl_before rmlocv
                just_loc -> just_loc
        just_loc -> just_loc
    where
        loc@(Loc loc_) = viewlist_entry_loc s e
        vl_after = drop (e+1) $ entries_viewlist s
        vl_before = reverse $ take e $ entries_viewlist s

rm_get_new_entry :: Entry a =>
    Status a -> [Loc] -> [EntryTree a] -> Int -> Maybe Int
rm_get_new_entry s rmlocv etn e =
    mpass (find_viewlist_pos etn) (rm_new_loc s rmlocv e)

do_delete :: Entry a => Status a -> [Loc] -> Status a
do_delete s rmlocv =
    check_everything (snapshot s){entries = etn, 
                                  selected_entry = nsel,
                                  active_entry = nact}
    where
        et = entries s
        etn = entrytree_remove et rmlocv
        gn = rm_get_new_entry s rmlocv etn
        nsel = fromMaybe 0 $ gn (selected_entry s)
        nact = maybe Nothing gn (active_entry s)


mv_get_new_entry :: Entry a =>
    Status a -> InsertWhere -> [Loc] -> [EntryTree a] -> Int -> Maybe Int
mv_get_new_entry s insw mvlocv etn e =
    mpass (find_viewlist_pos etn) 
          $ mpass (\l -> loc_ins_effect l insw (length mvlocv))
                  (rm_new_loc s mvlocv e)

do_move :: Entry a => 
    Status a -> InsertWhere -> [Loc] -> Status a
do_move s insw mvlocv =
    check_everything (snapshot s){entries = etn, 
                                  selected_entry = nsel,
                                  active_entry = nact}
    where
        et = entries s
        etn = fst $ entrytree_move et insw mvlocv
        gn = mv_get_new_entry s insw mvlocv etn
        nsel = fromMaybe 0 $ gn (selected_entry s)
        nact = maybe Nothing gn (active_entry s)

do_delentry s e =
    do_delete s [viewlist_entry_loc s e]

do_move_tagged :: Entry a => Status a -> InsertWhere -> Status a
do_move_tagged s insw =
    do_clear_tags $ do_move s insw tagged
    where
        tagged = entrytree_get_tagged (entries s)

do_tag s act e =
    maybe s (\nent_ -> s{entries = nent_}) etn
    where
        etn = entrytree_tag (entries s) (viewlist_entry_loc s e) act

do_clear_tags s =
    maybe s (\nent_ -> s{entries = nent_}) etn
    where
        etn = entrytree_clear_tags (entries s)

unmodified_or_empty :: String -> String -> Bool
unmodified_or_empty n o =
    n == o || (dropWhile isSpace n == [])


#ifdef CF_CHARSET_SUPPORT

to_locale str = withLCString str (\s -> peekCString s)
from_locale str = withCString str (\s -> peekLCString s)

#else

to_locale str = return str
from_locale str = return str

#endif


do_edit :: String -> IO (String, CalendarTime)
do_edit text = do
    Curses.withCursor Curses.CursorVisible (do
        Curses.endWin
        text_ <- to_locale text
        newtext_ <- Editor.edittext text_
        newtext <- from_locale newtext_
        t_ <- getClockTime
        t <- toCalendarTime t_
	Curses.clearOk True
        Curses.refresh
        if unmodified_or_empty newtext_ text_ then
            error "Aborted unmodified or empty entry."
          else
            return (newtext, t))

do_modify :: EditableEntry a => 
    Status a -> EntryTree a -> Loc -> (String, CalendarTime) -> Status a
do_modify s ent loc (newtext, tm) =
    check_everything $ ss{entries = et, 
                          active_entry = Just pos,
                          selected_entry = pos}
    where
        ss = snapshot s
        newent = entry_set_text ent newtext tm
        et = entrytree_replace (entries s) loc newent
        pos = find_viewlist_pos et loc

do_insert :: Entry a => Status a -> InsertWhere -> EntryTree a -> Status a
do_insert s insw ent =
    check_everything $ ss{entries = et, 
                          active_entry = Just pos,
                          selected_entry=pos}
    where
        ss = snapshot s
        (et_, loc) = entrytree_insert (entries s) insw [ent]
        et = case loc of
                 Loc (_:[]) -> et_
                 Loc ll -> fromMaybe et_ $ entrytree_expand et_ $ Loc (Data.List.init ll)
        pos = find_viewlist_pos et loc

do_editentry :: EditableEntry a => Status a -> Int -> IO(Status a)
do_editentry s e = 
    liftM (do_modify s ent loc) $ do_edit text
    where
        (loc, ent) = fromJust $ lnth e (entries_viewlist s)
        text = entry_text ent

do_newentry :: Entry a => Status a -> InsertWhere -> IO (Status a)
do_newentry s insw = do
    (text, tm) <- do_edit ""
    liftM (do_insert s insw . new_entrytree) $ new_callback s text tm


-- }}}


-- Message & yes/no query {{{

do_message :: Entry a =>
    Status a -> (UIAttr -> (Curses.Attr, Curses.Pair)) -> String -> IO ()
do_message s attr_fn msg = do
    Curses.wMove Curses.stdScr (botinfo_line s) 0
    cset_attr (attr_fn $ attr s)
    waddstr Curses.stdScr $ take (screen_width s) $ msg ++ repeat ' '
    creset_attr

message :: Entry a => Status a -> String -> IO ()
message s msg = do_message s attr_message msg


get_key s refresh_fn = do
    Control.Exception.catch (Curses.cBreak True) (\_ -> return ())
    k <- Curses.getCh
    case k of
        Nothing -> get_key s refresh_fn
	Just Curses.KeyResize -> do
	    snew <- get_size s
	    refresh_fn snew
	    get_key snew refresh_fn
	Just k -> return (s, k)


get_yn s yes no cancel refresh_fn = do
    (snew, k) <- get_key s refresh_fn
    case k of
        (Curses.KeyChar 'y') -> return (snew, yes)
        (Curses.KeyChar 'Y') -> return (snew, yes)
        (Curses.KeyChar 'n') -> return (snew, no)
        (Curses.KeyChar 'N') -> return (snew, no)
        (Curses.KeyChar '\^G') -> return (snew, cancel)
        otherwise -> Curses.beep >> get_yn s yes no cancel refresh_fn

yesno :: Entry a => 
    Status a
    -> String
    -> (Status a -> IO (ELCont a))
    -> (Status a -> IO (ELCont a))
    -> IO (ELCont a)
yesno s msg yes no = do
    draw_yesno msg s
    Curses.refresh
    (snew, action) <- get_yn s yes no cont refresh_fn
    draw_botinfo snew
    action snew
    where
        draw_yesno msg s = do
            Curses.wMove Curses.stdScr (botinfo_line s) 0
            cset_attr (attr_message $ attr s)
            waddstr Curses.stdScr (msg++" [y/n/^G]")
            fill_to_eol
            creset_attr
        refresh_fn s = redraw s >> draw_yesno msg s >> Curses.refresh

-- }}}


-- Event handling {{{

data Entry a => ELCont a =
    ELCont (Status a) Bool |
    ELQuit (Status a)

-- Generic stuff

cont :: Entry a => Status a -> IO (ELCont a)
cont s = return $ ELCont s False

cont_clr :: Entry a => Status a -> IO (ELCont a)
cont_clr s = return $ ELCont s True

cont_refresh :: Entry a => Status a -> IO (ELCont a)
cont_refresh s = refresh s >> cont s

nocont :: Entry a => Status a -> IO (ELCont a)
nocont s = return $ ELQuit s

e_quit :: Entry a => Status a -> IO (ELCont a)
e_quit s = 
    if unsaved_changes s then
        yesno s "Save changes before quitting?"
              (\s_ -> save s_ >>= nocont) nocont
    else
        nocont s

e_refresh :: Entry a => Status a -> IO (ELCont a)
e_refresh = \s -> refresh s >> cont s

e_none :: Entry a => Status a -> IO (ELCont a)
e_none = cont

e_submap :: Entry a => String -> (Curses.Key -> Status a -> IO (ELCont a)) -> Status a -> IO (ELCont a)
e_submap info submap s = do
    draw_submap s
    Curses.refresh
    (snew, k) <- get_key s refresh_fn
    draw_botinfo snew
    submap k snew
    where
        draw_submap s = do
            Curses.wMove Curses.stdScr (botinfo_line s) 0
            cset_attr (attr_message $ attr s)
            waddstr Curses.stdScr ("["++info++"-]")
            creset_attr
	refresh_fn s = redraw s >> draw_submap s >> Curses.refresh


-- Check if control is set and call handle_key again with the plain key.
e_fallback handle_key (Curses.KeyChar k)
    | ord '\^A' <= ord k && ord k <= ord '\^Z' =
        handle_key (Curses.KeyChar $ chr $ ord k - ord '\^A' + ord 'a')
e_fallback _ _ = e_none

goto_next_or_redraw :: Entry a => Status a -> IO (ELCont a)
goto_next_or_redraw s =
    if (selected_entry s) + 1 < (n_entries_viewed s) then
        e_nextentry s
    else
        draw_entries s >> cont s

-- Text view manipulation

update_scroll snew = do
    draw_textarea snew
    draw_botinfo snew
    cont snew

e_vscroll :: Entry a => Int -> Status a -> IO (ELCont a)
e_vscroll amount s =
    update_scroll (do_vscroll amount s)

e_hscroll :: Entry a => Int -> Status a -> IO (ELCont a)
e_hscroll amount s =
    update_scroll (do_hscroll amount s)

e_pgdn :: Entry a => Status a -> IO (ELCont a)
e_pgdn s = e_vscroll ((textarea_height s) `div` 2) s

e_pgup :: Entry a => Status a -> IO (ELCont a)
e_pgup s = e_vscroll (-(textarea_height s) `div` 2) s

e_eob :: Entry a => Status a -> IO (ELCont a)
e_eob s = update_scroll s{textarea_vscroll = max_vscroll s}

e_bob :: Entry a => Status a -> IO (ELCont a)
e_bob s = update_scroll s{textarea_vscroll = 0}

-- Entry tree manipulation

e_selectentry :: Entry a => Int -> Status a -> IO (ELCont a)
e_selectentry n s = do
    snew <- return (do_selectentry n s)
    draw_entries snew
    draw_midinfo snew
    cont snew

e_nextentry :: Entry a => Status a -> IO (ELCont a)
e_nextentry s = e_selectentry (selected_entry s + 1) s

e_preventry :: Entry a => Status a -> IO (ELCont a)
e_preventry s = e_selectentry (selected_entry s - 1) s

e_pgdnentry :: Entry a => Status a -> IO (ELCont a)
e_pgdnentry s = 
    e_selectentry e s
    where
        e = min (n_entries_viewed s - 1) 
                $ selected_entry s + (entryarea_height s `div` 2)

e_pgupentry :: Entry a => Status a -> IO (ELCont a)
e_pgupentry s =
    e_selectentry e s
    where
        e = max 0 $ selected_entry s - (entryarea_height s `div` 2)

e_lastentry :: Entry a => Status a -> IO (ELCont a)
e_lastentry s = e_selectentry (n_entries_viewed s - 1) s

e_firstentry :: Entry a => Status a -> IO (ELCont a)
e_firstentry s = e_selectentry 0 s

e_actentry :: Entry a => Status a -> IO (ELCont a)
e_actentry s = do
    refresh snew
    cont snew
    where
        sel = selected_entry s
        snew = do_expand (do_actentry s sel) sel 

e_delentry :: Entry a => Status a -> IO (ELCont a)
e_delentry s = cont_refresh (do_delentry s $ selected_entry s)

e_collapse :: Entry a => Status a -> IO (ELCont a)
e_collapse s = cont_refresh (do_collapse_p s $ selected_entry s)

e_editentry :: EditableEntry a => Status a -> IO (ELCont a)
e_editentry s = 
    (do_editentry s $ selected_entry s)
    >>= cont_refresh

e_newentry :: Entry a => Status a -> IO (ELCont a)
e_newentry s =
    do_newentry s loc >>= cont_refresh
    where
        loc = case null (entries s) of
                  True -> First
                  False -> (After $ selected_entry_loc s)
            
e_newunder :: Entry a => Status a -> IO (ELCont a)
e_newunder s =
    do_newentry s loc >>= cont_refresh
    where
        loc = case null (entries s) of
                  True -> First
                  False -> (FirstUnder $ selected_entry_loc s)

e_undo :: Entry a => Status a -> IO (ELCont a)
e_undo s = cont_refresh (undo s)
 
e_redo :: Entry a => Status a -> IO (ELCont a)
e_redo s = cont_refresh (redo s)
    

-- Tags

e_tag :: Entry a => TagAction -> Status a -> IO (ELCont a)
e_tag act s = goto_next_or_redraw $ do_tag s act (selected_entry s)

e_clear_tags :: Entry a => Status a -> IO (ELCont a)
e_clear_tags s = do
    snew <- return (do_clear_tags s)
    draw_entries snew
    cont snew

e_move_tagged_after :: Entry a => Status a -> IO (ELCont a)
e_move_tagged_after s = 
    cont_refresh $ do_move_tagged s (After $ selected_entry_loc s)

e_move_tagged_before :: Entry a => Status a -> IO (ELCont a)
e_move_tagged_before s = 
    cont_refresh $ do_move_tagged s (Before $ selected_entry_loc s)

e_move_tagged_under :: Entry a => Status a -> IO (ELCont a)
e_move_tagged_under s =
    cont_refresh $ do_expand snew (selected_entry snew)
    where
        snew = do_move_tagged s (FirstUnder $ selected_entry_loc s)

e_delete_tagged :: Entry a => Status a -> IO (ELCont a)
e_delete_tagged s =
    cont_refresh $ do_delete s $ entrytree_get_tagged $ entries s

-- Misc.

e_save :: Entry a => Status a -> IO (ELCont a)
e_save s = do
    snew <- save s
    draw_midinfo snew
    message snew $ "Saved " ++ (fromMaybe "???" $  entries_fname s)
    cont_clr snew


-- }}}


-- Main event loop {{{

do_except :: Entry a => Status a -> Control.Exception.Exception -> IO (ELCont a)
do_except s e = do
    Curses.beep
    do_message s attr_error (show e)
    cont_clr s

event_loop_ :: Entry a =>
    Status a
    -> (Curses.Key -> Status a -> IO (ELCont a))
    -> Bool
    -> IO ()
event_loop_ s keyhandler clr = do
    (snew, k) <- get_key s refresh
    if clr then draw_botinfo snew else return ()
    c <- Control.Exception.catch (keyhandler k snew) (do_except snew)
    case c of
        ELQuit _ ->  return ()
        ELCont snew clr -> Curses.refresh >> event_loop_ snew keyhandler clr

event_loop :: Entry a => Status a -> (Curses.Key -> Status a -> IO (ELCont a)) -> IO ()
event_loop s k = 
    Curses.withCursor Curses.CursorInvisible $ event_loop_ s k False

-- }}}

