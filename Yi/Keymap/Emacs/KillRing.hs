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

module Yi.Keymap.Emacs.KillRing where

import Yi.Core
import Yi.Keymap.Emacs.UnivArgument
import Yi.Region
import Yi.Keymap
import Yi.Buffer
import Data.Dynamic

import Control.Monad ( when, replicateM_ )

-- * Killring structure

data Killring = Killring { krKilled :: Bool
                         , krAccumulate :: Bool
                         , krContents :: [String]
                         , krLastYank :: Bool
                         , krYanked :: Bool
                         }
    deriving (Typeable, Show)

instance Initializable Killring where
    initial = Killring { krKilled = False
                       , krAccumulate = False
                       , krContents = [[]]
                       , krLastYank = False
                       , krYanked = False
                       }

-- * Killring "ADT"

killringMaxDepth :: Int
killringMaxDepth = 10

-- | Finish an atomic command, for the purpose of killring accumulation.
killringEndCmd :: Action
killringEndCmd = do kr@Killring {krKilled = killed} <- getDynamic
                    setDynamic $ kr {krKilled = False, krAccumulate = killed }

-- | Put some text in the killring.
-- It's accumulated if the last command was a kill too
killringPut :: String -> Action
killringPut s = do kr@Killring {krContents = r@(x:xs), krAccumulate=acc} <- getDynamic
                   setDynamic $ kr {krKilled = True,
                                    krContents =
                                        if acc then (x++s):xs
                                               else s:take killringMaxDepth r }

-- | Return the killring contents as a list. Head is most recent.
killringGet :: YiM [String]
killringGet = do Killring {krContents = r} <- getDynamic
                 return r

killringModify :: (Killring -> Killring) -> YiM ()
killringModify f = do
                   kr <- getDynamic
                   setDynamic $ f kr

-- * Killring actions

-- | Get the current region boundaries
getRegionE :: YiM Region
getRegionE = do m <- withBuffer getSelectionMarkPointB
                p <- withBuffer pointB
                return $ mkRegion m p

-- | C-w
killRegionE :: Action
killRegionE = do r <- getRegionE
                 text <- readRegionE r
                 killringPut text
                 deleteRegionE r

-- | C-k
killLineE :: Action
killLineE = withUnivArg $ \a -> case a of
               Nothing -> killRestOfLineE
               Just n -> replicateM_ (2*n) killRestOfLineE

-- | Kill the rest of line
killRestOfLineE :: Action
killRestOfLineE =
    do eol <- atEolE
       l <- readRestOfLnE
       killringPut l
       killE
       when eol $
            do c <- readE
               killringPut [c]
               deleteE

-- | C-y
yankE :: Action
yankE = do (text:_) <- killringGet
           --kr@(Killring _ _ _) <- getDynamic undefined
           --let text = show kr
           withBuffer (pointB >>= setSelectionMarkPointB)
           insertNE text

-- | M-w
killRingSaveE :: Action
killRingSaveE = do text <- readRegionE =<< getRegionE
                   killringPut text
-- | M-y

-- TODO: Handle argument, verify last command was a yank
yankPopE :: Action
yankPopE = do r <- getRegionE
              deleteRegionE r
              kr@Killring {krContents = ring} <- getDynamic
              setDynamic $ kr {krContents = tail ring ++ [head ring]}
              yankE

-- | C-M-w
appendNextKillE :: Action
appendNextKillE = killringModify (\kr -> kr {krKilled=True})
