--
-- riot/MBox.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

module HEmacs.MBox (

    MBoxEntry,
    read_mbox,
    read_mbox_entrytree,
    show_mbox,
    show_mbox_entrytree,
    new_mboxentry,
    mboxentry_msgid,
    mboxentry_inreplyto,
    thread_mbox

  ) where

import HEmacs.Version(version)
import HEmacs.Entry

#ifdef CF_CHARSET_SUPPORT
import HEmacs.IConv
#endif

import Data.Char
import Data.List        ( sortBy )
import Data.Maybe
import Data.Unique
import Data.FiniteMap   ( FiniteMap, listToFM, lookupFM )
import Time
import System.Locale    ( defaultTimeLocale )

-- {{{ MBoxEntry

data MBoxEntry = MBoxEntry {
    headers, body :: String
}

instance Entry MBoxEntry where
    entry_title m =
        case get_header "Subject:" (headers m) of
            Nothing -> first_nonempty_line $ entry_text m
            Just s -> s
    entry_text m = decode (charset m) (body m)
    entry_flags m = ""

instance EditableEntry MBoxEntry where
    entry_set_text m txt tm =
        m{headers = h, body = b}
	where
	    (b, ct) = encode (Just "utf-8") txt
	    h = set_header "Content-Type:" (headers m) ct


instance Show MBoxEntry where
    show m = (headers m) ++ "\n\n" ++ (fix_body $ body m)

instance Read MBoxEntry where
    readsPrec _ s = case do_read_mboxentry ('\n':s) of
                        Nothing -> []
                        Just p -> [p]


mk_msgid :: CalendarTime -> Unique -> String
mk_msgid tm u =
    "<riot."++stm++"."++(show $ hashUnique u)++">"
    where
        stm = formatCalendarTime defaultTimeLocale "%Y%m%d%H%M%S" tm

new_mboxentry :: String -> CalendarTime -> IO MBoxEntry
new_mboxentry txt tm = do
    u <- newUnique
    return MBoxEntry{
        headers="From background-static " ++ stm ++
                "\nMessage-Id: " ++ (mk_msgid tm u) ++
                "\nDate: " ++ stm ++
                "\nStatus: RO" ++
                "\nX-Riot-Version: " ++ version,
        body=txt
    }
    where
        stm = calendarTimeToString tm

-- }}}


-- Misc. {{{

first_nonempty_line s =
    reverse $ dropWhile isSpace $ reverse
            $ takeWhile (\c -> c /= '\n') $ dropWhile isSpace s

fix_body [] = []
fix_body s@('\n':'\n':[]) = s
fix_body s@('\n':[]) = '\n':s
fix_body (c:[]) = c:"\n\n"
fix_body ('\n':'F':'r':'o':'m':' ':s) = "\n>From"++(fix_body s)
fix_body (s:ss)= s:(fix_body ss)

-- }}}


-- Read {{{

-- Find 'From'
do_read_mboxentry (' ':s) = do_read_mboxentry s
do_read_mboxentry ('\t':s) = do_read_mboxentry s
do_read_mboxentry ('\n':s@('F':'r':'o':'m':' ':_)) =
    case do_read_headers_r "" s of
        Nothing -> Nothing
        Just (h_r, s_) -> Just (MBoxEntry { 
                                    headers = reverse h_r, 
                                    body = reverse b_r 
                                }, s__)
                          where
                              (b_r, s__) = do_read_body_r "" s_

do_read_mboxentry _ = Nothing

do_read_headers_r got ('\n':'\n':left) = Just (got, left)
do_read_headers_r got (s:ss) = do_read_headers_r (s:got) ss
do_read_headers_r _ _ = Nothing

do_read_body_r got ('\n':ss@('F':'r':'o':'m':_)) = ('\n':got, ss)
do_read_body_r got ('\n':'>':'F':'r':'o':'m':' ':ss) = 
    do_read_body_r (" morF\n"++got) ss
do_read_body_r got [] = (got, [])
do_read_body_r got (s:ss) = do_read_body_r (s:got) ss

read_mbox :: String -> [MBoxEntry]
read_mbox s =
    case do_read_mboxentry ('\n':s) of
        Nothing -> case null s of
                       True -> []
                       False -> error $ "MBox parse error."
        Just (e, s_) -> e:(read_mbox s_)
        

read_mbox_entrytree :: String -> [EntryTree MBoxEntry]
read_mbox_entrytree s =
    thread_mbox $ read_mbox s

is_header f s = (map toLower $ take (length f) s) == (map toLower f)

get_header f [] = Nothing
get_header f ('\n':s) | is_header f s =
    Just $ dropWhile isSpace $ drop (length f)
         $ reverse $ fst $ get_header_content [] s
get_header f (s:ss) = get_header f ss    

get_header_content acc ('\n':'\t':ss) = get_header_content ('\t':'\n':acc) ss
get_header_content acc sss@('\n':ss) = (acc, sss)
get_header_content acc (s:ss) = get_header_content (s:acc) ss
get_header_content acc [] = (acc, [])

set_header f [] fnew = '\n':f++" "++fnew
set_header f ('\n':s) fnew | is_header f s =
    '\n':f++" "++fnew++(snd $ get_header_content [] s)
set_header f (s:ss) fnew = s:(set_header f ss fnew)


-- }}}


-- Show {{{

set_inreplyto m id =
    m{headers=set_header "In-Reply-To:" (headers m) id}

show_mbox :: [MBoxEntry] -> String
show_mbox mm = concat $ map show mm

show_mbox_entrytree :: [EntryTree MBoxEntry] -> String
show_mbox_entrytree et =
    do_show_mbox_entrytree et Nothing

do_show_mbox_entrytree :: [EntryTree MBoxEntry] -> Maybe String -> String
do_show_mbox_entrytree [] _ = []
do_show_mbox_entrytree (e:et) parid =
    (show m) ++ (do_show_mbox_entrytree ch $ mboxentry_msgid m)
        ++ (do_show_mbox_entrytree et parid)
    where
        ch = entrytree_children e
        m_ = entrytree_thisentry e
        m = case (parid, mboxentry_inreplyto m_ == parid) of
		(Just p, False) -> set_inreplyto m_ p
		otherwise -> m_

-- }}}


-- Threading {{

parse_msgid ('<':ss) =
    case rest of
        ('>':_) -> Just ('<':id++">")
        otherwise -> Nothing
    where
        (id, rest) = span (\c -> c/='>') ss
parse_msgid _ = Nothing

mboxentry_msgid :: MBoxEntry -> Maybe String
mboxentry_msgid m = 
    maybe Nothing parse_msgid 
          $ get_header "Message-Id:" (headers m)

mboxentry_inreplyto :: MBoxEntry -> Maybe String
mboxentry_inreplyto m = 
    maybe Nothing parse_msgid 
          $ get_header "In-Reply-To:" (headers m)


get_inreplyto :: FiniteMap String (Int, MBoxEntry) -> MBoxEntry -> Maybe (Int, MBoxEntry)
get_inreplyto fm m =
    case mboxentry_inreplyto m of
        Nothing -> Nothing
        Just id -> lookupFM fm id


make_fm :: [(Int, MBoxEntry)] -> FiniteMap String (Int, MBoxEntry)
make_fm l = 
    listToFM $ f l
    where
        f [] = []
        f ((i, m):mm) = case mboxentry_msgid m of
                            Nothing -> f mm
                            Just id -> (id, (i, m)):(f mm)

idpath fm (i, m) acc =
   case get_inreplyto fm m of
       Nothing -> (i:acc)
       Just re -> idpath fm re (i:acc)

compare_idp fm s1@(i1, m1) s2@(i2, m2) =
    compare idp1 idp2
    where
        idp1 = idpath fm s1 []
        idp2 = idpath fm s2 []


thread_mbox :: [MBoxEntry] -> [EntryTree MBoxEntry]
thread_mbox mm =
    list_to_entrytree $ map cdepth mss
    where
        ms = zip [1..] mm
        fm = make_fm ms
        mss = sortBy (compare_idp fm) ms
        cdepth im@(i, m) =
            (False, length idp - 1, m)
            where
                idp = idpath fm im []
    

-- }}}


-- Character set conversions {{{

get_charset [] = Nothing
get_charset ('c':'h':'a':'r':'s':'e':'t':'=':ss) = 
    Just $ takeWhile (not . isSpace) ss
get_charset (s:ss) = get_charset ss

charset m = 
    case get_header "Content-Type:" (headers m) of
        Nothing -> Nothing
	Just h -> get_charset h

#ifdef CF_CHARSET_SUPPORT

fail_notice a e = "[Failed to "++a++": "++(show e)++"]\n\n"

encode Nothing txt = (txt, "text/plain")
encode (Just charset) txt =
    case from_unicode charset txt of
        Right t -> (t, "text/plain; charset=" ++ charset)
	Left e -> ((fail_notice "encode" e) ++ txt, "text/plain")

decode Nothing txt = txt
decode (Just charset) txt =
    case to_unicode charset txt of
        Right t -> t
	Left e -> (fail_notice "decode" e) ++ txt

#else

encode _ txt = (txt, "text/plain")
decode _ txt = txt

#endif


-- }}}
