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

--
-- | Colors and friends.
--

module Yi.Style (

        Color(..),              -- a color defn
        Style(..),          -- a pair of foreground and background
        UIStyle(..), ui,    -- a set of ui component colours, and a default

        black, grey, darkRed, red, darkGreen, green, brown, yellow,
        darkBlue, blue, purple, magenta, darkCyan, cyan, white, brightWhite,

   ) where

--
-- | The UI type
--
data UIStyle = 
    UIStyle { 
        window           :: Style    -- ^ window fg and bg (ignore for now)
       ,modeline         :: Style    -- ^ out of focus modeline colours
       ,modeline_focused :: Style    -- ^ in focus modeline
       ,eof              :: Style    -- empty file marker colours
--     ,error_messages   :: Style    -- ^ error messages in the modeline
     }

--
-- | Default settings. Need to map (Color,Color) -> Pair
--
ui :: UIStyle
ui = UIStyle {
         window             = Style Default      Default
        ,modeline           = Style black        darkCyan 
        ,modeline_focused   = Style brightWhite  darkCyan 
        ,eof                = Style darkBlue     Default
--      ,error_messages     = Style BrightWhiteF DarkRed
     } 

--
-- | Foreground and background color pairs
--
data Style = Style Color Color

--
-- our representation of colours
--
data Color
    = RGB (Int,Int,Int)
    | Default
    -- .. anything else

--
-- Some colours (derivied from proxima/src/common/CommonTypes.hs)
--
black, grey, darkRed, red, darkGreen, green, brown, yellow          :: Color
darkBlue, blue, purple, magenta, darkCyan, cyan, white, brightWhite :: Color
black       = RGB (0,0,0)
grey        = RGB (128,128,128)
darkRed     = RGB (139,0,0)
red         = RGB (255,0,0)
darkGreen   = RGB (0,100,0)
green       = RGB (0,128,0)
brown       = RGB (165,42,42)
yellow      = RGB (255,255,0)
darkBlue    = RGB (0,0,139)
blue        = RGB (0,0,255)
purple      = RGB (128,0,128)
magenta     = RGB (255,0,255)
darkCyan    = RGB (0,139,139) 
cyan        = RGB (0,255,255)
white       = RGB (165,165,165)
brightWhite = RGB (255,255,255)

