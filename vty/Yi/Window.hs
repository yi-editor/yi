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

-- | A 'Window' is a rectangular view onto a buffer. 

module Yi.Window where

import Yi.Buffer
import Yi.Vty

import Data.Unique              ( Unique, newUnique, hashUnique )

--
-- | A window onto a buffer.
--
-- Windows need some small information about the terminal. For example,
-- they need to know the height and width.  That's about it.
--

data Window =
    Window {
        key         :: !Unique         -- ^ each window has a unique
       ,bufkey      :: !Unique         -- ^ the buffer this window opens to
       ,mode        :: !Bool           -- ^ this window has modeline?
       ,height      :: !Int            -- ^ height of this window
       ,width       :: !Int            -- ^ width of this window

       ,cursor      :: !(Int,Int)      -- ^ cursor point on screen (y,x)

       ,pnt         :: !Int            -- ^ current point

       ,tospnt      :: !Int            -- ^ the buffer point of the top of screen
       ,toslineno   :: !Int            -- ^ line number of top of screen
                       
       ,bospnt      :: !Int            -- ^ the buffer point of the bottom of screen
       ,picture     :: !Image          -- ^ the picture currently displayed.
    }


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
emptyWindow :: FBuffer -> (Int,Int) -> IO Window
emptyWindow b (h,w) = do
    wu <- newUnique
    let win = Window {
                    key       = wu
                   ,mode      = False
                   ,bufkey    = (keyB b)
                   ,height    = h      -- - 1 for the cmdline?
                   ,width     = w
                   ,cursor    = (0,0)  -- (y,x) (screen columns, needs to know about tabs)
                   ,pnt       = 0      -- cache point when out of focus
                   ,tospnt    = 0
                   ,bospnt    = 0
                   ,toslineno = 1      -- start on line 1
                   ,picture = empty
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
updateModeLine w' b = do
    if not (mode w') then return Nothing else do
    ln <- curLn b
    let f    = nameB b
        lns  = show ln
        top  = toslineno w'
        cols = show $ 1 + snd (cursor w')

    p <- indexOfEol b
    s <- sizeB b
    let pct = if top == 1 then "Top" else getPercent p s

    case flip replicate ' ' (16 - length cols - length pct) of { spc' ->
    case flip replicate ' ' (width w' - (3 + sum
                (map length [f,lns,cols,pct,spc']))) of { spaces ->
    return $ Just $ "\"" ++ f ++ "\"" ++ spaces ++
                    lns ++ "," ++ cols ++ spc' ++ pct
    }}

--
-- | Give a point, and the file size, gives us a percent string
--
getPercent :: Int -> Int -> String
getPercent a b = show p ++ "%"
    where p = ceiling ((fromIntegral a) / (fromIntegral b) * 100 :: Double) :: Int


-- ---------------------------------------------------------------------
-- Editing operations

--
-- | return index of Sol on line @n@ above current line
--
indexOfSolAbove :: FBuffer -> Int -> IO Int
indexOfSolAbove b n = do
    p <- pointB b
    moveToSol b
    loop n b
    q <- pointB b
    moveTo b p
    return q

    where loop 0 _  = return ()
          loop i b' = lineUp b' >> loop (i-1) b'


--
-- | Adjust the window's size-related fields, assuming a new window
-- height, and width. Now, if we know the height of the screen, and the
-- number of lines, center the line in the screen please.
resize :: Int -> Int -> Window -> Window
resize y x w = w { height = y, width = x }



