{-# OPTIONS -#include YiUtils.h #-}
-- 
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
-- Based on: riot/Style.hs
-- 
--     Copyright (c) Tuomo Valkonen 2004.
--
-- Released under the same license.
--

--
-- | Colors and friends. At the moment this is tied to curses. This
-- needs to be fixed. The attibutes and colours should be a type class
--

module Yi.Style (

        UI(..), Style, ui,

        -- Colours
        defaultColor, black, red, green, yellow, 
        blue, magenta, cyan, white,

        Foreground(..), Background(..), bgAttr, fgAttr,
        BrightColor(..), DarkColor(..),

        -- Attributes
        setBoldA, setUnderlineA, setDimA, setReverseA,
        nullA, boldA, underlineA, dimA, reverseA,

   ) where

import qualified Yi.Curses as Curses
import Data.Maybe        ( fromJust )

------------------------------------------------------------------------
--
-- Basic (ncurses) colours.
--
defaultColor :: Curses.Color
defaultColor = fromJust $ Curses.color "default" -- doesn't work?

black, red, green, yellow, blue, magenta, cyan, white :: Curses.Color
black     = fromJust $ Curses.color "black"
red       = fromJust $ Curses.color "red"
green     = fromJust $ Curses.color "green"
yellow    = fromJust $ Curses.color "yellow"
blue      = fromJust $ Curses.color "blue"
magenta   = fromJust $ Curses.color "magenta"
cyan      = fromJust $ Curses.color "cyan"
white     = fromJust $ Curses.color "white"

------------------------------------------------------------------------

--
-- Combine attribute with another attribute
--
setBoldA, setUnderlineA, setDimA, setReverseA :: Curses.Attr -> Curses.Attr
setBoldA      = flip Curses.setBold      True
setUnderlineA = flip Curses.setUnderline True
setDimA       = flip Curses.setDim       True
setReverseA   = flip Curses.setReverse   True

--
-- | Some attribute constants
--
boldA, nullA, underlineA, dimA, reverseA :: Curses.Attr
nullA       = Curses.attr0
boldA       = setBoldA      nullA
underlineA  = setUnderlineA nullA
dimA        = setDimA       nullA
reverseA    = setReverseA   nullA

------------------------------------------------------------------------
--
-- The UI type
--
data UI = UI { 
        window      :: Style    -- ^ window fg and bg (ignore for now)
       ,modeln      :: Style    -- ^ out of focus modeline colours
       ,modeln_hl   :: Style    -- ^ in focus modeline
       ,commandln   :: Style    -- ^ command line colours
       ,eof         :: Style    -- empty file marker colours
     }

-- foreground and background color
type Style = (Foreground, Background)

--
-- | Default settings. Need to map (Color,Color) -> Pair
--
ui :: UI
ui = UI { 
         window    = (FgDefault,      BgDefault)
        ,modeln    = (Fg  Black,      Bg DarkBlue)
        ,modeln_hl = (FgBright White, Bg DarkBlue)
        ,commandln = (FgDefault,      BgDefault)
        ,eof       = (Fg  DarkBlue,   BgDefault)
     } 


------------------------------------------------------------------------
--
-- Nicer, user-visible colour defs.
--
-- We separate colours into dark and bright colours, to prevent users
-- from erroneously constructing bright colours for dark backgrounds,
-- which doesn't work.
--

data DarkColor 
    = Black
    | DarkRed
    | DarkGreen
    | Brown
    | DarkBlue
    | Purple 
    | DarkCyan
    | DarkWhite

-- Bright colours, can only be used for foregrounds
data BrightColor 
    = Grey
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

-- foreground colours can be anything
data Foreground = FgBright BrightColor 
                | Fg       DarkColor
                | FgDefault

-- Background colors can't be bright.
data Background = Bg DarkColor
                | BgDefault

------------------------------------------------------------------------

--
-- Dark colors to curses colours
--
dark2curses :: DarkColor -> Curses.Color
dark2curses c = case c of
    Black       -> black
    DarkRed     -> red
    DarkGreen   -> green
    Brown       -> yellow
    DarkBlue    -> blue
    Purple      -> magenta
    DarkCyan    -> cyan
    DarkWhite   -> white

bright2curses :: BrightColor -> Curses.Color
bright2curses c = case c of
    Grey        -> black
    Red         -> red
    Green       -> green
    Yellow      -> yellow
    Blue        -> blue
    Magenta     -> magenta
    Cyan        -> cyan
    White       -> white

-- dark colours
bgAttr :: Background -> (Curses.Attr, Curses.Color)
bgAttr BgDefault  = (nullA, defaultColor)
bgAttr (Bg c)     = (nullA, dark2curses c)

-- light colours
fgAttr :: Foreground -> (Curses.Attr, Curses.Color)
fgAttr FgDefault    = (nullA, defaultColor)
fgAttr (Fg  c)      = (nullA, dark2curses c)
fgAttr (FgBright c) = (boldA, bright2curses c)

