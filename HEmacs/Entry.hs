--
-- riot/Entry.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

-- Module information {{{

module HEmacs.Entry (

    Entry(..),
    EditableEntry(..),
    EntryTree(..),
    InsertWhere(..),
    Loc(..),
    TagAction(..),
    new_entrytree,
    list_to_entrytree,
    entrytree_set_entry,
    entrytree_get,
    entrytree_map,
    entrytree_map_maybe,
    entrytree_map_path,
    entrytree_flatten,
    entrytree_replace_f,
    entrytree_replace,
    entrytree_insert,
    entrytree_remove,
    entrytree_move,
    entrytree_expand,
    entrytree_collapse,
    entrytree_collapse_p,
    entrytree_tag,
    entrytree_clear_tags,
    entrytree_get_tagged,
    loc_rm_effect,
    loc_rm_effect_insw,
    loc_ins_effect
 
  ) where

import Data.Maybe
import Data.List                (sort)
import Time                     (CalendarTime)

-- Classes & misc {{{

class Entry a where
    entry_title :: a -> String
    entry_title e = ""
    
    entry_text :: a -> String
    entry_text e = ""
    
    entry_flags :: a -> String
    entry_flags e = ""


class Entry a => EditableEntry a where
    entry_set_text :: a -> String -> CalendarTime -> a


data Entry a => EntryTree a = EntryTree {
    entrytree_expanded :: Bool, 
    entrytree_tagged :: Bool,
    entrytree_thisentry :: a,
    entrytree_children :: [EntryTree a]
}


instance Entry a => Entry (EntryTree a) where
    entry_title = entry_title . entrytree_thisentry
    entry_text = entry_text . entrytree_thisentry
    entry_flags = entry_flags . entrytree_thisentry


instance EditableEntry a => EditableEntry (EntryTree a) where
    entry_set_text e txt tm =
        e{entrytree_thisentry = nent}
        where
            nent = entry_set_text (entrytree_thisentry e) txt tm


new_entrytree e =
    EntryTree False False e []

entrytree_set_entry et e =
    et{entrytree_thisentry = e}
    

-- }}}


-- Location stuff {{{

newtype Loc = Loc [Int] deriving Eq

instance Ord Loc where
    (Loc []) <= (Loc _) = True
    (Loc _) <= (Loc []) = False
    (Loc (l1:ll1)) <= (Loc (l2:ll2)) =
        if l1 < l2 then
            True
        else if l1==l2 then
            (Loc ll1) <= (Loc ll2)
        else
            False

from_loc (Loc ll) = ll

loc_before (Loc ll) = Loc $ loc_before_ ll
    where
        loc_before_ (l:[]) | l>0 = [l-1]
        loc_before_ (l:ll) = l:(loc_before_ ll)

loc_after (Loc ll) = Loc $ loc_after_ ll
    where
        loc_after_ (l:[]) = [l+1]
        loc_after_ (l:ll) = l:(loc_after_ ll)

loc_firstunder (Loc ll) = Loc (ll++[0])

loc_lastunder (Loc ll) nch = Loc (ll++[nch])

-- }}}


-- Get {{{

entrytree_get :: Entry a => [EntryTree a] -> Loc -> EntryTree a
entrytree_get (e:et) (Loc (0:[])) = e
entrytree_get (e:et) (Loc (0:loc)) = entrytree_get (entrytree_children e) (Loc loc)
entrytree_get (e:et) (Loc (n:loc)) | n>0 = entrytree_get et (Loc ((n-1):loc))
entrytree_get _ _ = error "Invalid entry tree location"

-- }}}


-- entrytree_map, entrytree_map_path {{{

entrytree_map_ f [] _ = []
entrytree_map_ f (e2:et) rrr@(r:rr) =
    e2map++etmap
    where
        nch = entrytree_map_ f (entrytree_children e2) (0:rrr)
        e2map = f e2 nch (Loc $ reverse rrr)
        etmap = entrytree_map_ f et ((r+1):rr)

entrytree_map :: Entry a => (EntryTree a -> [b] -> Loc -> [b]) -> [EntryTree a] -> [b]
entrytree_map f et = entrytree_map_ f et [0]

-- Apply function to all entries in the tree, starting from leaves
-- and feeding the function also a Maybe indicating whether the children
-- of the entry in question were changed.
entrytree_map_maybe_ f [] _ = Nothing
entrytree_map_maybe_ f (e2:et) rrr@(r:rr) =
    case (e2n, etn) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just etn) -> Just (e2:etn)
        (Just e2n, Nothing) -> Just (e2n++et)
        (Just e2n, Just etn) -> Just (e2n++etn)
    where
        nch = entrytree_map_maybe_ f (entrytree_children e2) (0:rrr)
        e2n = f e2 nch (Loc $ reverse rrr)
        etn = entrytree_map_maybe_ f et ((r+1):rr)

entrytree_map_maybe :: Entry a => 
    (EntryTree a -> Maybe [EntryTree a] -> Loc -> Maybe [EntryTree a])
    -> [EntryTree a] -> Maybe [EntryTree a]
entrytree_map_maybe f et = entrytree_map_maybe_ f et [0]

-- Similar to entrytree_map, but restrict to working on a path given by
-- a Loc.
entrytree_map_path_ f [] _ _ = Nothing
entrytree_map_path_ f _ [] _ = Nothing
entrytree_map_path_ f (e:et) (l:ll) rrr@(r:rr) | l==r =
    maybe Nothing (\e_ -> Just (e_++et)) ne
    where
        nch = entrytree_map_path_ f (entrytree_children e) ll (0:rrr)
        ne = f e nch (Loc $ reverse rrr)
entrytree_map_path_ f (e:et) lll@(l:ll) rrr@(r:rr) | r<l =
   case entrytree_map_path_ f et lll ((r+1):rr) of
       Nothing -> Nothing
       Just et_ -> Just (e:et_)

entrytree_map_path :: Entry a => 
    (EntryTree a -> Maybe [EntryTree a] -> Loc -> Maybe [EntryTree a])
    -> [EntryTree a] -> Loc -> Maybe [EntryTree a]
entrytree_map_path f et (Loc ll) = entrytree_map_path_ f et ll [0]

-- }}}


-- Conversions: flatten, list_to_entrytree {{{

entrytree_flatten :: Entry a => [EntryTree a] -> [(Bool, Int, a)]
entrytree_flatten et =
    entrytree_map f et
    where
       f e chflat (Loc loc) =
           (entrytree_expanded e, length loc, entrytree_thisentry e):chflat
        

list_to_entrytree_ :: Entry a => Int -> [(Bool, Int, a)] -> ([EntryTree a], [(Bool, Int, a)])
list_to_entrytree_ _ [] = ([], [])
list_to_entrytree_ d eee@(e@(e_x, e_d, e_e):ee)
    | e_d<d     = ([], eee)
    | e_d==d    = (et:more, ee__)
    | otherwise = error "Invalid depths in list"
    where
        et = EntryTree e_x False e_e e_ch
        (e_ch, ee_) = list_to_entrytree_ (d+1) ee
        (more, ee__) = list_to_entrytree_ d ee_


list_to_entrytree :: Entry a => [(Bool, Int, a)] -> [EntryTree a]
list_to_entrytree et = fst $ list_to_entrytree_ 0 et

-- }}}


-- Remove/insert effect calculation on locations {{{

rm_effect_ :: [Int] -> [Int] -> [Int]
rm_effect_ [] [] = error "Invalid insertion point"
rm_effect_ _ [] = []
rm_effect_ (r:[]) iii@(i:ii) =
    if r<i then (i-1):ii
    else if r==i then error "Invalid insertion point"
    else iii
rm_effect_ (r:rr) iii@(i:ii) =
    if r==i then i:(rm_effect_ rr ii)
    else iii

has_init :: Loc -> Loc -> Bool
has_init (Loc l2) (Loc l1) = take (length l1) l2 == l1 

-- The list must be sorted
rm_inits (l1:ll@(l2:ll2)) = 
     if has_init l2 l1 then
         rm_inits (l1:ll2)
     else
         l1:(rm_inits ll)
rm_inits ll = ll

loc_rm_effect loc locv = 
    if null notafter then
        Just loc
    else if has_init loc (last notafter) then
        Nothing -- loc will be removed!
    else
        Just $ foldl sub_effect loc (reverse $ notafter)
    where
        notafter = filter (\l -> l<=loc) $ rm_inits $ sort locv
        sub_effect (Loc ll) (Loc su) = Loc $ rm_effect_ su ll

ins_effect_before [] [] ne = error "Invalid insertion"
ins_effect_before [] _ ne = []
ins_effect_before _ [] ne = []
ins_effect_before lll@(l:ll) (i:ii) ne
    | l >= i && null ii = (l+ne):ll
    | l == i = l:(ins_effect_before ll ii ne)
    | otherwise = lll

ins_effect_after [] [] ne = []
ins_effect_after [] _ ne = []
ins_effect_after _ [] ne = []
ins_effect_after lll@(l:ll) (i:ii) ne
    | l > i && null ii = (l+ne):ll
    | l == i = l:(ins_effect_after ll ii ne)
    | otherwise = lll

ins_effect_firstunder (l:ll) [] ne = (l+ne):ll
ins_effect_firstunder [] _ ne = []
ins_effect_firstunder lll@(l:ll) (i:ii) ne
    | l == i = l:(ins_effect_firstunder ll ii ne)
    | otherwise = lll

ins_effect_lastunder lll iii ne = lll

loc_ins_effect :: Loc -> InsertWhere -> Int -> Loc
loc_ins_effect loc Last _ =
    loc
loc_ins_effect (Loc (l:ll)) First ne =
    Loc $ (l+ne):ll
loc_ins_effect (Loc loc) (Before (Loc iloc)) ne =
    Loc $ ins_effect_before loc iloc ne
loc_ins_effect (Loc loc) (After (Loc iloc)) ne =
    Loc $ ins_effect_after loc iloc ne
loc_ins_effect (Loc loc) (FirstUnder (Loc iloc)) ne =
    Loc $ ins_effect_firstunder loc iloc ne
loc_ins_effect (Loc loc) (LastUnder (Loc iloc)) ne =
    Loc $ ins_effect_lastunder loc iloc ne

mpass f = maybe Nothing (Just . f) 

loc_rm_effect_insw :: InsertWhere -> [Loc] -> Maybe InsertWhere
loc_rm_effect_insw First _ = Just First
loc_rm_effect_insw Last _ = Just Last
loc_rm_effect_insw (Before loc) locv = mpass Before $ loc_rm_effect loc locv
loc_rm_effect_insw (After loc) locv = mpass After $ loc_rm_effect loc locv
loc_rm_effect_insw (FirstUnder loc) locv = mpass FirstUnder $ loc_rm_effect loc locv
loc_rm_effect_insw (LastUnder loc) locv = mpass LastUnder $ loc_rm_effect loc locv

-- }}}


-- Remove, insert, replace, move etc. {{{

-- Replace

entrytree_replace_f :: Entry a => [EntryTree a] -> Loc -> (EntryTree a -> ([EntryTree a], b)) -> ([EntryTree a], b)
entrytree_replace_f (e:et) (Loc (0:[])) f = (\(fe, x) -> (fe ++ et, x)) $ f e
entrytree_replace_f (e:et) (Loc (0:loc)) f =
    (e{entrytree_children = ch}:et, x)
    where
        (ch, x) = entrytree_replace_f (entrytree_children e) (Loc loc) f
entrytree_replace_f (e:et) (Loc (n:loc)) f
    | n>0 = (e:et2, x)
    where
        (et2, x) = entrytree_replace_f et (Loc ((n-1):loc)) f
entrytree_replace_f _ _ _ = error "Invalid entry tree location"


entrytree_replace :: Entry a => [EntryTree a] -> Loc -> EntryTree a -> [EntryTree a]
entrytree_replace et loc enew =
    fst $ entrytree_replace_f et loc $ \_ -> ([enew], ())

-- Insert

data InsertWhere = 
    First | Last |
    Before Loc | After Loc | 
    FirstUnder Loc | LastUnder Loc

entrytree_insert :: Entry a => [EntryTree a] -> InsertWhere -> [EntryTree a] -> ([EntryTree a], Loc)

entrytree_insert et First eins =
    (eins ++ et, Loc [0])

entrytree_insert et Last eins =
    (et ++ eins, Loc [length et])

entrytree_insert et (Before loc) eins =
    (fst $ entrytree_replace_f et loc $ \e -> (eins ++ [e], ()), loc_before loc)

entrytree_insert et (After loc) eins =
    (fst $ entrytree_replace_f et loc $ \e -> ([e] ++ eins, ()), loc_after loc)

entrytree_insert et (FirstUnder loc) eins =
    (fst $ entrytree_replace_f et loc f, loc_firstunder loc)
    where
        f e = ([e{entrytree_children = eins ++ entrytree_children e}], ())

entrytree_insert et (LastUnder loc) eins =
    g $ entrytree_replace_f et loc f
    where
        f e = ([e{entrytree_children = ch ++ eins}], length ch)
              where
                  ch = entrytree_children e
        g (et, nch) = (et, loc_lastunder loc nch)


-- Remove

-- location list must be reverse-sorted for entrytree_remove_
entrytree_remove_ :: Entry a => [EntryTree a] -> [Loc] -> ([EntryTree a], [EntryTree a])
entrytree_remove_ et [] = (et, [])
entrytree_remove_ et (l:ll) =
    (\(et_, el) -> (et_, e:el)) $ entrytree_remove_ et2 ll
    where
        (et2, e) = entrytree_replace_f et l (\e_ -> ([], e_))


entrytree_remove :: Entry a => [EntryTree a] -> [Loc] -> [EntryTree a]
entrytree_remove et locv = fst $ entrytree_remove_ et (reverse $ sort locv)


-- Move

entrytree_move :: Entry a => [EntryTree a] -> InsertWhere -> [Loc] -> ([EntryTree a], Loc)
entrytree_move et insw locv =
    case insw_ of
        Nothing -> error "List of entries to be moved contains (a parent of) target."
        Just insw__ -> entrytree_insert et_ insw__ (reverse entries)
    where
        insw_ = loc_rm_effect_insw insw locv
        (et_, entries) = entrytree_remove_ et (reverse $ sort locv)

-- }}}


-- Expand & collapse {{{

entrytree_expand :: Entry a => [EntryTree a] -> Loc -> Maybe [EntryTree a]
entrytree_expand = entrytree_map_path f
    where
        f e Nothing _
            | entrytree_expanded e || length (entrytree_children e) == 0 = Nothing
            | otherwise = Just [e{entrytree_expanded = True}]
        f e (Just nch) _ =
            Just [e{entrytree_expanded = True, entrytree_children=nch}]


entrytree_collapse :: Entry a => [EntryTree a] -> Loc -> Maybe [EntryTree a]
entrytree_collapse et loc = entrytree_map_path f et loc
    where
        f e _ eloc | loc==eloc =
            case entrytree_expanded e of
                True -> Just [e{entrytree_expanded = False}]
                False -> Nothing
        f e Nothing _ = Nothing
        f e (Just nch) _ =
            Just [e{entrytree_children=nch}]


entrytree_collapse_p :: Entry a => [EntryTree a] -> Loc -> Maybe [EntryTree a]
entrytree_collapse_p et loc@(Loc cl) = entrytree_map_path f et loc
    where
        f e Nothing (Loc ll) | (length cl - length ll) <= 1 =
            case entrytree_expanded e of
                True -> Just [e{entrytree_expanded = False}]
                False -> Nothing
        f e Nothing _ = Nothing
        f e (Just nch) _ =
            Just [e{entrytree_children=nch}]

-- }}}


-- {{{ Tagging


data TagAction = TagSet | TagUnset | TagToggle

entrytree_tag :: Entry a => [EntryTree a] -> Loc -> TagAction -> Maybe [EntryTree a]
entrytree_tag et loc what =
    entrytree_map_path f et loc
    where
        f e Nothing eloc | loc==eloc =
            case (entrytree_tagged e, what) of
                (False, TagSet) -> Just [e{entrytree_tagged=True}]
                (False, TagToggle) -> Just [e{entrytree_tagged=True}]
                (True, TagUnset) -> Just [e{entrytree_tagged=False}]
                (True, TagToggle) -> Just [e{entrytree_tagged=False}]
                otherwise -> Nothing
        f e (Just nch) _ =
            Just [e{entrytree_children = nch}]
        f _ _ _ = Nothing


entrytree_clear_tags :: Entry a => [EntryTree a] -> Maybe [EntryTree a]
entrytree_clear_tags et =
    entrytree_map_maybe f et
    where
        f e Nothing _ =
            case entrytree_tagged e of
                False -> Nothing
                True -> Just [e{entrytree_tagged=False}]
        f e (Just nch) _ =
            Just [e{entrytree_tagged=False, entrytree_children=nch}]


entrytree_get_tagged :: Entry a => [EntryTree a] -> [Loc]
entrytree_get_tagged et =
   entrytree_map f et
   where
       f e chloc loc =
           case entrytree_tagged e of
               True -> loc:chloc
               False -> chloc
   
-- }}}
