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

--
-- | Colors and friends.
--

module Yi.Style (

        UIStyle(..), Style(..), ui,
        ForegroundColor(..), BackgroundColor(..),

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
-- | Foreground and background color pairs
--
data Style = Style ForegroundColor BackgroundColor

--
-- | Default settings. Need to map (Color,Color) -> Pair
--
ui :: UIStyle
ui = UIStyle {
         window             = Style DefaultF     DefaultB
        ,modeline           = Style BlackF       DarkCyanB
        ,modeline_focused   = Style BrightWhiteF DarkCyanB
        ,eof                = Style DarkBlueF    DefaultB
--      ,error_messages     = Style BrightWhiteF DarkRedB
     } 

--
-- Nicer, user-visible colour defs.
--
-- We separate colours into dark and bright colours, to prevent users
-- from erroneously constructing bright colours for dark backgrounds,
-- which doesn't work.

--
-- Foreground colours
--
data ForegroundColor
    = BlackF
    | GreyF
    | DarkRedF
    | RedF
    | DarkGreenF
    | GreenF
    | BrownF
    | YellowF
    | DarkBlueF
    | BlueF
    | PurpleF
    | MagentaF
    | DarkCyanF
    | CyanF
    | WhiteF
    | BrightWhiteF
    | DefaultF

--
-- Background colors can't be bright.
--
data BackgroundColor
    = BlackB
    | DarkRedB
    | DarkGreenB
    | BrownB
    | DarkBlueB
    | PurpleB
    | DarkCyanB
    | WhiteB
    | DefaultB

