--
-- riot/Style.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

--
-- | Colors and friends
--

module HEmacs.Style (

        UIAttr(..), StyleSpec(..),
        init_uiattr, default_uiattr, 

        -- Colours
        c_default, c_black, c_red, c_green, c_yellow, 
        c_blue, c_magenta, c_cyan, c_white,

        -- Attributes
        sa_bold, sa_underline, sa_dim, sa_reverse,
        a_none, a_bold, a_underline, a_dim, a_reverse,

   ) where

import qualified HEmacs.Curses as Curses

import Data.Maybe        ( fromJust )

c_default   = fromJust $ Curses.color "default" -- doesn't work?
c_black     = fromJust $ Curses.color "black"
c_red       = fromJust $ Curses.color "red"
c_green     = fromJust $ Curses.color "green"
c_yellow    = fromJust $ Curses.color "yellow"
c_blue      = fromJust $ Curses.color "blue"
c_magenta   = fromJust $ Curses.color "magenta"
c_cyan      = fromJust $ Curses.color "cyan"
c_white     = fromJust $ Curses.color "white"

sa_bold      = \a -> Curses.setBold a True
sa_underline = \a -> Curses.setUnderline a True
sa_dim       = \a -> Curses.setDim a True
sa_reverse   = \a -> Curses.setReverse a True

a_none       = Curses.attr0
a_bold       = sa_bold Curses.attr0
a_underline  = sa_underline Curses.attr0
a_dim        = sa_dim Curses.attr0
a_reverse    = sa_reverse Curses.attr0

newtype StyleSpec = StyleSpec (String, (Curses.Attr, Curses.Color, Curses.Color))

data UIAttr = UIAttr {
    attr_infoline,
    attr_text,
    attr_entry,
    attr_entry_sel,
    attr_entry_act,
    attr_entry_act_sel,
    attr_error,
    attr_message :: (Curses.Attr, Curses.Pair)
}

default_uiattr = UIAttr {
    attr_infoline = ar,
    attr_text = a0,
    attr_entry = a0,
    attr_entry_sel = ar,
    attr_entry_act = ab,
    attr_entry_act_sel = ab,
    attr_error = ab,
    attr_message = ab
} where
    a0 = (a_none, Curses.Pair 0)
    ar = (a_reverse, Curses.Pair 0)
    ab = (a_bold, Curses.Pair 0)

uiattr_set :: UIAttr -> String -> (Curses.Attr, Curses.Pair) -> UIAttr
uiattr_set a "attr_infoline" v = a{attr_infoline = v}
uiattr_set a "attr_text" v = a{attr_text = v}
uiattr_set a "attr_entry" v = a{attr_entry = v}
uiattr_set a "attr_entry_sel" v = a{attr_entry_sel = v}
uiattr_set a "attr_entry_act" v = a{attr_entry_act = v}
uiattr_set a "attr_entry_act_sel" v = a{attr_entry_act_sel = v}
uiattr_set a "attr_error" v = a{attr_error = v}
uiattr_set a "attr_message" v = a{attr_message = v}

init_style :: (Curses.Attr, Curses.Color, Curses.Color) 
           -> Curses.Pair 
           -> Bool 
           -> IO(Curses.Attr, Curses.Pair)

init_style (a, fg, bg) p bw =
    case bw of
        True -> return (a, Curses.Pair 0)
        False -> Curses.initPair p fg bg >> return (a, p)

do_init_styles :: UIAttr -> [StyleSpec] -> Bool -> Int -> IO (UIAttr)
do_init_styles a styles bw pair =
    case styles of
        [] -> return a
        (StyleSpec (name, dflt):more) -> do
            attr <- init_style dflt (Curses.Pair pair) bw
            a2   <- return (uiattr_set a name attr)
            do_init_styles a2 more bw (pair+1)

--
-- | Set up the ui attributes
--
init_uiattr :: [StyleSpec] -> IO (UIAttr)
init_uiattr styles = do
    p <- Curses.colorPairs              -- how many colors
    let bw = p < length styles
    do_init_styles default_uiattr styles bw 1

