--
-- Copyright (c) 2007 Jean-Philippe Bernardy
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

module Yi.WindowSet where
-- FIXME: export abstractly


import Yi.Debug
import Control.Monad.Trans
import Data.List as List (nub, delete)
import Data.Foldable
import Data.Traversable

newtype WindowSet a = WindowSet { contents :: [a] }
    deriving (Show, Functor, Foldable, Traversable)


new :: a -> WindowSet a
new w = WindowSet [w]

add :: Eq a => a -> WindowSet a -> WindowSet a
add w (WindowSet ws) = WindowSet $ List.nub (w:ws) 

delete :: Eq a => a -> WindowSet a -> WindowSet a
delete _ (WindowSet [w']) = WindowSet [w'] -- never delete the last window
delete w (WindowSet ws) = WindowSet $ List.delete w ws

current :: WindowSet a -> a
current (WindowSet ws) = head ws


shift :: Int -> WindowSet a -> WindowSet a
shift n (WindowSet ws) = WindowSet ((drop m) ws ++ (take m) ws)
                          where m = n `mod` length ws

setFocus :: Eq a => a -> WindowSet a -> WindowSet a
setFocus w (WindowSet ws) = WindowSet (nub (w:ws)) --FIXME: this sucks!

update :: Eq a => a -> WindowSet a -> WindowSet a
update w (WindowSet ws) = WindowSet (map (\w' -> if w == w' then w else w') ws)

size :: WindowSet a -> Int
size (WindowSet ws) = length ws

debug msg (WindowSet ws) = logPutStrLn $ msg ++ ": " ++ show ws 
