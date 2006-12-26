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

#include "ghcconfig.h"

import qualified Yi.Vty as Vty

import qualified Data.ByteString as P (ByteString)

import Data.Word                (Word8)
import Data.Maybe               (fromJust)
import Data.IORef               (newIORef, IORef)
import qualified Data.Map as M  (empty, lookup, Map)

import System.IO.Unsafe         (unsafePerformIO)

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
data Style = Style {-# UNPACK #-} !Color !Color deriving (Eq,Ord)

-- | A List of characters with styles attached
data CharA = C {-# UNPACK #-} !Char
           | A {-# UNPACK #-} !Char !Style

-- | A list of such values (the representation is optimised)
data StringA = Fast   {-# UNPACK #-} !P.ByteString !Style
             | FancyS {-# UNPACK #-} ![(P.ByteString, Style)]  -- one line made up of segments

data Color
    = RGB {-# UNPACK #-} !Word8 !Word8 !Word8
    | Default
    | Reverse
    deriving (Eq,Ord)

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
#if defined(HAVE_USE_DEFAULT_COLORS)
defaultfg   = Default
defaultbg   = Default
#else
defaultfg   = white
defaultbg   = black
#endif
reversefg   = Reverse
reversebg   = Reverse

------------------------------------------------------------------------

-- | Given a curses color pair, find the Vty.Pair (i.e. the pair
-- curses thinks these colors map to) from the state
lookupPair :: PairMap -> Style -> (Vty.Attr, Vty.Pair)
lookupPair m s = case M.lookup s m of
                    Nothing   -> (Vty.attr0, Vty.Pair 0) -- default settings
                    Just v    -> v
{-# INLINE lookupPair #-}

-- | Keep a map of nice style defs to underlying curses pairs, created at init time
type PairMap = M.Map Style (Vty.Attr, Vty.Pair)

-- | map of Vty.Color pairs to ncurses terminal Pair settings
pairMap :: IORef PairMap
pairMap = unsafePerformIO $ newIORef M.empty
{-# NOINLINE pairMap #-}

------------------------------------------------------------------------
--
-- Basic (ncurses) colours.
--
defaultColor :: Vty.Color
defaultColor = fromJust $ Vty.color "default"

cblack, cred, cgreen, cyellow, cblue, cmagenta, ccyan, cwhite :: Vty.Color
cblack     = fromJust $ Vty.color "black"
cred       = fromJust $ Vty.color "red"
cgreen     = fromJust $ Vty.color "green"
cyellow    = fromJust $ Vty.color "yellow"
cblue      = fromJust $ Vty.color "blue"
cmagenta   = fromJust $ Vty.color "magenta"
ccyan      = fromJust $ Vty.color "cyan"
cwhite     = fromJust $ Vty.color "white"

--
-- Combine attribute with another attribute
--
setBoldA, setReverseA ::  Vty.Attr -> Vty.Attr
setBoldA     = flip Vty.setBold    True
setReverseA  = flip Vty.setReverse True

--
-- | Some attribute constants
--
boldA, nullA, reverseA :: Vty.Attr
nullA       = Vty.attr0
boldA       = setBoldA      nullA
reverseA    = setReverseA   nullA

------------------------------------------------------------------------

newtype CColor = CColor {fromCColor :: (Vty.Attr, Vty.Color)}
-- 
-- | Map Style rgb rgb colours to ncurses pairs
-- TODO a generic way to turn an rgb into the nearest curses color
--
style2curses :: Style -> (CColor, CColor)
style2curses (Style fg bg) = (fgCursCol fg, bgCursCol bg)
{-# INLINE style2curses #-}

fgCursCol :: Color -> CColor
fgCursCol c = case c of
    RGB 0 0 0         -> CColor (nullA, cblack)
    RGB 128 128 128   -> CColor (boldA, cblack)
    RGB 139 0 0       -> CColor (nullA, cred)
    RGB 255 0 0       -> CColor (boldA, cred)
    RGB 0 100 0       -> CColor (nullA, cgreen)
    RGB 0 128 0       -> CColor (boldA, cgreen)
    RGB 165 42 42     -> CColor (nullA, cyellow)
    RGB 255 255 0     -> CColor (boldA, cyellow)
    RGB 0 0 139       -> CColor (nullA, cblue)
    RGB 0 0 255       -> CColor (boldA, cblue)
    RGB 128 0 128     -> CColor (nullA, cmagenta)
    RGB 255 0 255     -> CColor (boldA, cmagenta)
    RGB 0 139 139     -> CColor (nullA, ccyan)
    RGB 0 255 255     -> CColor (boldA, ccyan)
    RGB 165 165 165   -> CColor (nullA, cwhite)
    RGB 255 255 255   -> CColor (boldA, cwhite)
    Default           -> CColor (nullA, defaultColor)
    Reverse           -> CColor (reverseA, defaultColor)
    _                 -> CColor (nullA, cblack) -- NB

bgCursCol :: Color -> CColor
bgCursCol c = case c of
    RGB 0 0 0         -> CColor (nullA, cblack)
    RGB 128 128 128   -> CColor (nullA, cblack)
    RGB 139 0 0       -> CColor (nullA, cred)
    RGB 255 0 0       -> CColor (nullA, cred)
    RGB 0 100 0       -> CColor (nullA, cgreen)
    RGB 0 128 0       -> CColor (nullA, cgreen)
    RGB 165 42 42     -> CColor (nullA, cyellow)
    RGB 255 255 0     -> CColor (nullA, cyellow)
    RGB 0 0 139       -> CColor (nullA, cblue)
    RGB 0 0 255       -> CColor (nullA, cblue)
    RGB 128 0 128     -> CColor (nullA, cmagenta)
    RGB 255 0 255     -> CColor (nullA, cmagenta)
    RGB 0 139 139     -> CColor (nullA, ccyan)
    RGB 0 255 255     -> CColor (nullA, ccyan)
    RGB 165 165 165   -> CColor (nullA, cwhite)
    RGB 255 255 255   -> CColor (nullA, cwhite)
    Default           -> CColor (nullA, defaultColor)
    Reverse           -> CColor (reverseA, defaultColor)
    _                 -> CColor (nullA, cwhite)    -- NB

defaultSty :: Style
defaultSty = Style Default Default


ccolorToAttr :: CColor -> Vty.Attr
ccolorToAttr (CColor (modifier, color)) = Vty.attrPlus modifier (Vty.colorToAttr color)  

styleToAttr :: Style -> Vty.Attr
styleToAttr =  ccolorToAttr . fst . style2curses
