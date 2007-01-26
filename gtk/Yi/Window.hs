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
-- | A 'Window' is a rectangular view onto a buffer. Editor actions
-- manipulate buffers via particular windows onto that buffer. Some
-- editor actions, such as scrolling, only manipulate windows, without
-- touching the underlying buffer.
--

module Yi.Window where

import Yi.FastBuffer
import Yi.Buffer

import Data.Unique              ( Unique, newUnique )

import Graphics.UI.Gtk hiding (Window)

--
-- | A window onto a buffer.
--
-- Windows need some small information about the terminal. For example,
-- they need to know the height and width.  That's about it.
--
-- This doesn't handle line wrapping, yet.
--
data Window =
    Window {
        key         :: !Unique         -- ^ each window has a unique
       ,bufkey      :: !Unique         -- ^ the buffer this window opens to
       ,textview    :: TextView
--     ,mode        :: !Bool           -- ^ this window has modeline?
    }

-- Until we know better, lets pretend height and cursor position are constants to the rest of the code.
height :: Window -> Int
height = const 10

cursor :: Window -> (Int,Int)
cursor = const (5,5)

instance Eq Window where
    Window { key = u } == Window { key = v }  = u == v

instance Ord Window where
    Window { key = u } `compare` Window { key = v }  = u `compare` v
    Window { key = u } <     Window { key = v }      = u <     v

-- ---------------------------------------------------------------------
-- Construction

--
-- A new window
--
-- The origin, height and width should be calculated with reference to
-- all existing windows.
--
emptyWindow :: FBuffer -> (Int,Int) -> IO Window
emptyWindow b (h,w) = do
    wu <- newUnique
    v <- textViewNewWithBuffer (textbuf $ rawbuf b)
    let win = Window {
                    key       = wu
                   ,bufkey    = (keyB b)
                   ,textview  = v
              }
    return win

------------------------------------------------------------------------
--
-- | Given a buffer, and some information update the modeline
-- There's some useful code in textinfo.window.c. Worth a read.
--
-- N.B. the contents of modelines should be specified by keymaps, and
-- not hardcoded.
--
updateModeLine :: Window -> FBuffer -> IO (Maybe String)
updateModeLine w' b = return Nothing -- FIXME

--
-- | Give a point, and the file size, gives us a percent string
--
getPercent :: Int -> Int -> String
getPercent a b = show p ++ "%"
    where p = ceiling ((fromIntegral a) / (fromIntegral b) * 100 :: Double) :: Int




