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

-- | A 'Window' is a view onto a buffer.

module Yi.Window where

import Yi.FastBuffer
import Yi.Buffer

import Data.Unique              ( Unique, newUnique, hashUnique )

import Graphics.UI.Gtk hiding (Window)
import Graphics.UI.Gtk.SourceView

data Window =
    Window {
        key         :: !Unique         -- ^ each window has a unique
       ,bufkey      :: !Unique         -- ^ the buffer this window opens to
       ,textview    :: SourceView
       ,modeline    :: Label
       ,widget      :: Box            -- ^ Top-level widget for this window.
       ,isMini      :: Bool
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

instance Show Window where
    show Window { key = u } = "Window #" ++ show (hashUnique u)

-- ---------------------------------------------------------------------
-- Construction

--
-- A new window
--
-- The origin, height and width should be calculated with reference to
-- all existing windows.
--
emptyWindow :: Bool -> FBuffer -> IO Window
emptyWindow mini b = do
    wu <- newUnique

    f <- fontDescriptionNew
    fontDescriptionSetFamily f "Monospace"

    ml <- labelNew Nothing
    widgetModifyFont ml (Just f)
    set ml [ miscXalign := 0.01 ] -- so the text is left-justified.

    v <- sourceViewNewWithBuffer (textbuf $ rawbuf b)
    widgetModifyFont v (Just f)


    box <- if mini 
     then do
      widgetSetSizeRequest v (-1) 1

      prompt <- labelNew (Just $ nameB b)
      widgetModifyFont prompt (Just f)

      hb <- hBoxNew False 1
      set hb [ containerChild := prompt, 
               containerChild := v, 
               boxChildPacking prompt := PackNatural,
               boxChildPacking v := PackNatural] 

      return (castToBox hb)
     else do
      scroll <- scrolledWindowNew Nothing Nothing
      set scroll [containerChild := v]

      vb <- vBoxNew False 1
      set vb [ containerChild := scroll, 
               containerChild := ml, 
               boxChildPacking ml := PackNatural] 
      return (castToBox vb)

    let win = Window {
                    key       = wu
                   ,bufkey    = (keyB b)
                   ,textview  = v
                   ,modeline  = ml
                   ,widget    = box
                   ,isMini    = mini
              }
    return win






