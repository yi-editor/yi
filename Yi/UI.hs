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

--
-- | User interface abstractions. Should always be general enough to
-- permit multiple user interfaces.
--
module Yi.UI (

        -- * UI initialisation 
        start, end, 
        screenSize,

        fillLine,       -- IO ()

        refresh,
        reset,
        warn,
        
        getKey,
        withInvisibleCursor,

  ) where

import Yi.Style
import Yi.Buffer
import qualified Yi.Curses as Curses
import qualified Yi.Editor as Editor

import Data.Maybe                       ( isJust, fromJust ) 
import Control.Monad                    ( when )
import qualified Control.Exception      ( try, catch )

------------------------------------------------------------------------
-- Initialisation

--
-- | how to initialise the ui
--
start :: IO ()
start = do
    Curses.initCurses                   -- initialise the screen
    Curses.keypad Curses.stdScr True    -- grab the keyboard

--
-- | Clean up and go home
--
end :: IO ()
end = Curses.endWin

--
-- | Find the current screen height and widith. This uses the ffi. You
-- should probably use Editor.getScreenSize once everything is
-- initialised
--
screenSize :: IO (Int, Int)
screenSize = Curses.scrSize

--
-- | set the cursor invisible
--
withInvisibleCursor = Curses.withCursor Curses.CursorInvisible

-- ---------------------------------------------------------------------
-- | Read a key.
--
getKey refresh_fn = do
    Control.Exception.catch (Curses.cBreak True) (\_ -> return ())
    k <- Curses.getCh
    case k of
        Nothing               -> getKey refresh_fn
        Just Editor.KeyResize -> do
            -- snew <- get_size s
            refresh_fn --snew
            getKey refresh_fn
        Just k                -> return k
 
-- ---------------------------------------------------------------------
-- Drawing stuff
--

--
-- | Draw as much of the buffer as will fit in the screen
--
drawBuffer :: Buffer a => a -> IO ()
drawBuffer buf = do
    let (w,h) = size buf
    mapM_ (drawLine w) $ take h $ (contents buf) ++ repeat []
    cset_attr (Curses.setReverse Curses.attr0 True , Curses.Pair (1))
    drawModeLine w (name buf)
    reset

drawMainBuffer :: Buffer a => a -> IO ()
drawMainBuffer buf = do
    let (w,h) = size buf
    mapM_ (drawLine w) $ take h $ (contents buf) ++ repeat []
    cset_attr (Curses.setReverse Curses.attr0 True , Curses.Pair 0)
    drawModeLine w (name buf)
    reset

drawModeLine :: Int -> String -> IO ()
drawModeLine w title = drawLine w ("\"" ++ title ++ "\"" ++ repeat ' ')

--
-- | needs to be fast -- or use better rendering systems!
--
drawLine :: Int -> String -> IO ()
drawLine w s  = Curses.wAddStr Curses.stdScr $ take w (s ++ repeat ' ')

-- 
-- | redraw the entire screen from the editor state
-- totally bogus.
--
redraw :: IO ()
redraw = do
    (mb,bs) <- Editor.getBuffers
    Curses.wMove Curses.stdScr 0 0  -- mv cursor to origin
    when (isJust mb) $ drawMainBuffer (fromJust mb)
    mapM_ drawBuffer bs

    when (isJust mb) $ Curses.withCursor Curses.CursorVisible $ do
        cset_attr (Curses.setReverse Curses.attr0 True , Curses.Pair 1)
        let (x,y) = point (fromJust mb)
        Curses.wMove Curses.stdScr y x   -- goto point
        reset

--
-- | Fill to end of line spaces
--
fillLine :: IO ()
fillLine = do
    (_, w) <- Curses.scrSize
    (_, x) <- Curses.getYX Curses.stdScr
    Curses.wAddStr Curses.stdScr $ replicate (max 0 (w-x)) ' '

-- ---------------------------------------------------------------------
--

--
-- | manipulate the current attributes of the standard screen
--
cset_attr :: (Curses.Attr, Curses.Pair) -> IO ()
cset_attr (a, p) = Curses.wAttrSet Curses.stdScr (a, p)

--
-- | Reset the screen to normal values
--
reset :: IO ()
reset = cset_attr (Curses.attr0, Curses.Pair 0)

-- ---------------------------------------------------------------------
-- Refreshing
--
    
--
-- | redraw and refresh the screen
--
refresh :: IO ()
refresh = redraw >> Curses.refresh

------------------------------------------------------------------------
-- misc

warn :: String -> IO ()
warn msg = do   -- do_message s attr_message msg
    Curses.wMove Curses.stdScr 0 0
    Curses.wAddStr Curses.stdScr $ take 80 $ msg ++ repeat ' '

------------------------------------------------------------------------
-- dead

{-
--
-- | draw all the lines to the screen?
--
draw_lines :: [String]
              -> Int
              -> Int
              -> Int
              -> (Maybe String -> Int -> IO ())
              -> IO ()

draw_lines d l_first l_last skip f = do
        Curses.wMove Curses.stdScr l_first 0
        do_draw_lines (drop skip d) l_first l_last skip f
    where
        do_draw_lines d l l_last nr f
            | l > l_last = return ()
            | otherwise  = do f (maybehead d) nr
                              do_draw_lines (safetail d) (l+1) l_last (nr+1) f

-- ---------------------------------------------------------------------
-- Text area

next_tab_stop pos = ((pos `div` 8) + 1) * 8

do_tab_stops pos [] = []
do_tab_stops pos ('\t':ss) = 
    replicate (nxt - pos) ' ' ++ (do_tab_stops nxt ss)
    where
        nxt = next_tab_stop pos

do_tab_stops pos (s:ss) = s:(do_tab_stops (pos + 1) ss)

--
-- | draw some text
--
do_draw_text s text hs vs sl el = do
    cset_attr (attr_text $ attr s)
    draw_lines s (map (do_tab_stops 0) $ lines text) sl el vs drawl
    creset_attr
  where w = screen_width s
        drawl s Nothing _  = fill_to_eol
        drawl s (Just l) _ = waddstr Curses.stdScr $
                                take w $ drop hs $ l ++ repeat ' '

do_draw_textarea s text = do_draw_text s text hs vs sl el
    where
        hs = textarea_hscroll s
        vs = textarea_vscroll s
        sl = textarea_startline s
        el = textarea_endline s

draw_textarea :: Entry a => Status a -> IO ()
draw_textarea status = do_draw_textarea status (textarea_text status)

do_draw_infoline_align status line left right = do
    Curses.wMove Curses.stdScr line 0           -- move the cursor to start of line
    cset_attr (attr_infoline $ attr status)
    waddstr Curses.stdScr ((take n_l left_)++(take n_r right_))
    creset_attr

  where left_ = left ++ (repeat ' ')
        right_ = ' ':right
        w = screen_width status
        n_r = min w (length right_)
        n_l = max 0 (w-n_r)

do_draw_infoline status line text = do_draw_infoline_align status line text ""

mk_n_of_m n m = "("++(show n)++"/"++(show m)++") "

draw_topinfo :: IO ()
draw_topinfo = do_draw_infoline (topinfo_line) (topinfo_text) 

draw_midinfo :: Entry a => Status a -> IO ()
draw_midinfo s = do_draw_infoline_align s l t (mk_n_of_m n m)
    where
        l = midinfo_line s
        t = midinfo_text s
        m = length (entries_viewlist_plain s)
        n = 1 + selected_entry s
       
--
-- | draw text into the status (bottom) line on the screen
--
draw_botinfo :: Entry a => Status a -> IO ()
draw_botinfo s = do_draw_infoline_align s l t "(yi) " {-(mk_n_of_m n m)-}
    where
        l = botinfo_line s
        t = botinfo_text s
        m = text_n_lines (textarea_text s)
        n = min m $ (textarea_vscroll s)+(textarea_height s)

--
-- | draw the little message when a multi-key command sequence is
-- detected
--
draw_submap :: IO ()
draw_submap = do
    Curses.wMove Curses.stdScr 0 {-botinfo_line-} 0
--  cset_attr (attr_message $ attr)
    waddstr Curses.stdScr ("["++"null"++"-]")
    creset_attr

-- ---------------------------------------------------------------------
-- Text area scrolling

max_scroll_ :: Int -> Int -> Int
max_scroll_ item_s view_s = max 0 (item_s - view_s)

text_max_hscroll :: String -> (Int, Int) -> Int
text_max_hscroll text (_, view_w) =
    max_scroll_ (text_n_columns text) view_w

text_max_vscroll :: String -> (Int, Int) -> Int
text_max_vscroll text (view_h, _) =
    max_scroll_ (text_n_lines text) view_h

textarea_size :: Entry a => Status a -> (Int, Int)
textarea_size s = (textarea_height s, screen_width s)

calc_scroll s asc csc_fn msc_fn = max 0 (min msc (csc + asc))
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

-- ---------------------------------------------------------------------
-- Entry selection

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

-- ---------------------------------------------------------------------
-- Expand/collapse

update_entries s et = 
    s{entries = et, active_entry = ae, selected_entry = se}
    where
        ae = mpass (find_viewlist_pos et) (active_entry_loc s)
        se = find_viewlist_pos et (selected_entry_loc s)

do_colexp_loc :: Entry a 
              => ([EntryTree a] -> Loc -> Maybe [EntryTree a]) 
              -> Status a -> Loc -> Status a

do_colexp_loc fn s loc = case fn (entries s) loc of
        Nothing -> s
        Just et -> check_everything $ update_entries s et

do_colexp :: Entry a 
          => ([EntryTree a] -> Loc -> Maybe [EntryTree a]) 
          -> Status a -> Int -> Status a

do_colexp fn s e = do_colexp_loc fn s $ viewlist_entry_loc s e

do_expand :: Entry a => Status a -> Int -> Status a
do_expand s e | valid_entry s e = do_colexp entrytree_expand s e

do_collapse :: Entry a => Status a -> Int -> Status a
do_collapse s e | valid_entry s e = do_colexp entrytree_collapse s e

do_collapse_p :: Entry a => Status a -> Int -> Status a
do_collapse_p s e | valid_entry s e = do_colexp entrytree_collapse_p s e

-- ---------------------------------------------------------------------
-- Undo/redo/save

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
        
-- ---------------------------------------------------------------------
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

-}

-- ---------------------------------------------------------------------
-- Message & yes/no query

{-
do_message :: (UIAttr -> (Curses.Attr, Curses.Pair)) -> String -> IO ()
do_message attr_fn msg = do
    Curses.wMove Curses.stdScr (botinfo_line s) 0
    cset_attr (attr_fn $ attr s)
    waddstr Curses.stdScr $ take (screen_width s) $ msg ++ repeat ' '
    creset_attr
-}

{-
get_yn s yes no cancel refresh_fn = do
    (snew, k) <- getKey s refresh_fn
    case k of
        (Editor.Key 'y') -> return (snew, yes)
        (Editor.Key 'Y') -> return (snew, yes)
        (Editor.Key 'n') -> return (snew, no)
        (Editor.Key 'N') -> return (snew, no)
        (Editor.Key '\^G') -> return (snew, cancel)
        otherwise -> Curses.beep >> get_yn s yes no cancel refresh_fn

yesno :: Entry a
      => Status a
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

-}

{-
#ifdef CF_CHARSET_SUPPORT

to_locale str = withLCString str (\s -> peekCString s)
from_locale str = withCString str (\s -> peekLCString s)

#else

to_locale str = return str
from_locale str = return str

#endif
-}
