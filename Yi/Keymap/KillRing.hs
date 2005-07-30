{-# OPTIONS -fglasgow-exts #-}
-- 
-- Copyright (c) 2005 Jean-Philippe Bernardy
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

module Yi.Keymap.KillRing where

import Yi.Editor
import Yi.Core
import Data.Dynamic

import Control.Monad ( when )

-- * Killring structure

data Killring = Killring {
                         krKilled :: Bool
                         , krAccumulate :: Bool
                         , krContents :: [String]
                         }
    deriving (Typeable, Show)

instance Initializable Killring where
    initial = return $ Killring False False [[]]

-- * Killring "ADT"

killringMaxDepth :: Int
killringMaxDepth = 10

-- | Finish an atomic command, for the purpose of killring accumulation.
killringEndCmd :: Action
killringEndCmd = do Killring killed _ r <- getDynamic undefined
                    setDynamic $ Killring False killed r

-- | Put some text in the killring.
-- It's accumulated if the last command was a kill too
killringPut :: String -> Action
killringPut s = do Killring _ acc r@(x:xs) <- getDynamic undefined
                   if acc 
                      then setDynamic $ Killring True acc (s:take killringMaxDepth r)
                      else setDynamic $ Killring True acc ((x++s):xs)

-- | Return the killring contents as a list. Head is most recent.
killringGet :: IO [String]
killringGet = do Killring _ _ r <- getDynamic undefined
                 return r

-- | Construct a region from its bounds
mkRegion :: Int -> Int -> (Int, Int)
mkRegion x y = if x < y then (x,y) else (y,x)

-- * Killring actions

killRegionE :: Action
killRegionE = do m <- getMarkE
                 p <- getPointE
                 let r = mkRegion m p
                 text <- readRegionE r
                 killringPut text
                 deleteRegionE r

killLineE :: Action
killLineE = do eol <- atEolE
               l <- readRestOfLnE
               killringPut l
               killE
               when eol $
                    do c <- readE
                       killringPut [c]
                       deleteE
 
yankE :: Action
yankE = do (text:_) <- killringGet 
           --kr@(Killring _ _ _) <- getDynamic undefined
           --let text = show kr
           getPointE >>= setMarkE
           insertNE text
           
