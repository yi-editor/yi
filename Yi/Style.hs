--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
-- | Colors and friends.
--

module Yi.Style where

import Data.Word                (Word8)

--
-- | The UI type
--
data UIStyle =
    UIStyle {
         window           :: !Style    -- ^ window fg and bg (ignore for now)
       , modeline         :: !Style    -- ^ out of focus modeline colours
       , modeline_focused :: !Style    -- ^ in focus modeline
       , selected         :: !Style    -- ^ the selected portion
       , eof              :: !Style    -- ^ empty file marker colours
     } 

data Color
    = RGB {-# UNPACK #-} !Word8 !Word8 !Word8
    | Default
    | Reverse
    deriving (Eq,Ord,Show)


--
-- | Default settings
--
{-
  Notice that the selected field is initially set to the default, which
  essentially means that selected text will not be highlighted. The reason
  for this is that if the mode does not remember to UnSet the mark after,
  for example, cutting, then there will always be a highlighted region, that
  is essentially the default for now which has worked up until now because the
  selected text wasn't highlighted, now that it is, we need the modes to unset
  the mark when nothing should be hightlighted.
-}
uiStyle :: UIStyle
uiStyle = UIStyle {
         window             = Style defaultfg    defaultbg
        ,modeline           = Style black        darkcyan
        ,modeline_focused   = Style brightwhite  darkcyan
        ,selected           = Style defaultfg    defaultbg
        ,eof                = Style blue         defaultbg
     }

------------------------------------------------------------------------

-- | Foreground and background color pairs
data Style = Style {-# UNPACK #-} !Color !Color deriving (Eq,Ord,Show)

------------------------------------------------------------------------
--
-- | Some simple colours (derivied from proxima/src/common/CommonTypes.hs)
--
-- But we don't have a light blue?
--
black, grey, darkred, red, darkgreen, green, brown, yellow          :: Color
darkblue, blue, purple, magenta, darkcyan, cyan, white, brightwhite :: Color
black       = RGB 0 0 0
grey        = RGB 128 128 128
darkred     = RGB 139 0 0
red         = RGB 255 0 0
darkgreen   = RGB 0 100 0
green       = RGB 0 128 0
brown       = RGB 165 42 42
yellow      = RGB 255 255 0
darkblue    = RGB 0 0 139
blue        = RGB 0 0 255
purple      = RGB 128 0 128
magenta     = RGB 255 0 255
darkcyan    = RGB 0 139 139 
cyan        = RGB 0 255 255
white       = RGB 165 165 165
brightwhite = RGB 255 255 255

defaultfg, defaultbg, reversefg, reversebg :: Color
defaultfg   = Default
defaultbg   = Default
reversefg   = Reverse
reversebg   = Reverse

------------------------------------------------------------------------

--
-- Combine attribute with another attribute
--
boldA, reverseA, nullA ::  Vty.Attr -> Vty.Attr
boldA a    = a { Vty.bold = True }
reverseA a = a { Vty.rv = True }
nullA       = id

------------------------------------------------------------------------

newtype CColor = CColor {fromCColor :: (Vty.Attr -> Vty.Attr, Vty.Color)}
-- 
-- | Map Style rgb rgb colours to ncurses pairs
-- TODO a generic way to turn an rgb into the nearest curses color
--
style2curses :: Style -> (CColor, CColor)
style2curses (Style fg bg) = (fgCursCol fg, bgCursCol bg)
{-# INLINE style2curses #-}

fgCursCol :: Color -> CColor
fgCursCol c = case c of
    RGB 0 0 0         -> CColor (nullA,    Vty.black)
    RGB 128 128 128   -> CColor (boldA,    Vty.black)
    RGB 139 0 0       -> CColor (nullA,    Vty.red)
    RGB 255 0 0       -> CColor (boldA,    Vty.red)
    RGB 0 100 0       -> CColor (nullA,    Vty.green)
    RGB 0 128 0       -> CColor (boldA,    Vty.green)
    RGB 165 42 42     -> CColor (nullA,    Vty.yellow)
    RGB 255 255 0     -> CColor (boldA,    Vty.yellow)
    RGB 0 0 139       -> CColor (nullA,    Vty.blue)
    RGB 0 0 255       -> CColor (boldA,    Vty.blue)
    RGB 128 0 128     -> CColor (nullA,    Vty.magenta)
    RGB 255 0 255     -> CColor (boldA,    Vty.magenta)
    RGB 0 139 139     -> CColor (nullA,    Vty.cyan)
    RGB 0 255 255     -> CColor (boldA,    Vty.cyan)
    RGB 165 165 165   -> CColor (nullA,    Vty.white)
    RGB 255 255 255   -> CColor (boldA,    Vty.white)
    Default           -> CColor (nullA,    Vty.def)
    Reverse           -> CColor (reverseA, Vty.def)
    _                 -> CColor (nullA,    Vty.black) -- NB

bgCursCol :: Color -> CColor
bgCursCol c = case c of
    RGB 0 0 0         -> CColor (nullA,    Vty.black)
    RGB 128 128 128   -> CColor (nullA,    Vty.black)
    RGB 139 0 0       -> CColor (nullA,    Vty.red)
    RGB 255 0 0       -> CColor (nullA,    Vty.red)
    RGB 0 100 0       -> CColor (nullA,    Vty.green)
    RGB 0 128 0       -> CColor (nullA,    Vty.green)
    RGB 165 42 42     -> CColor (nullA,    Vty.yellow)
    RGB 255 255 0     -> CColor (nullA,    Vty.yellow)
    RGB 0 0 139       -> CColor (nullA,    Vty.blue)
    RGB 0 0 255       -> CColor (nullA,    Vty.blue)
    RGB 128 0 128     -> CColor (nullA,    Vty.magenta)
    RGB 255 0 255     -> CColor (nullA,    Vty.magenta)
    RGB 0 139 139     -> CColor (nullA,    Vty.cyan)
    RGB 0 255 255     -> CColor (nullA,    Vty.cyan)
    RGB 165 165 165   -> CColor (nullA,    Vty.white)
    RGB 255 255 255   -> CColor (nullA,    Vty.white)
    Default           -> CColor (nullA,    Vty.def)
    Reverse           -> CColor (reverseA, Vty.def)
    _                 -> CColor (nullA,    Vty.white)    -- NB

defaultSty :: Style
defaultSty = Style Default Default

styleToAttr :: Style -> Vty.Attr
styleToAttr = ccolorToAttr . style2curses
    where ccolorToAttr ((CColor (fmod, fcolor)), (CColor (bmod, bcol))) = 
              fmod . bmod $ Vty.attr {Vty.fg = fcolor, Vty.bg = bcol}
