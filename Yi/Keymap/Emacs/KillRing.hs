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
import Yi.Buffer.HighLevel
import Data.Dynamic
import Yi.CharMove

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
killringEndCmd :: YiM ()
killringEndCmd = do kr@Killring {krKilled = killed} <- getDynamic
                    setDynamic $ kr {krKilled = False, krAccumulate = killed }

-- | Put some text in the killring.
-- It's accumulated if the last command was a kill too
killringPut :: String -> YiM ()
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
getRegionB :: BufferM Region
getRegionB = do m <- getSelectionMarkPointB
                p <- pointB
                return $ mkRegion m p

-- | C-w
killRegionE :: YiM ()
killRegionE = do r <- withBuffer getRegionB
                 text <- withBuffer $ readRegionB r
                 killringPut text
                 withBuffer unsetMarkB
                 withBuffer $ deleteRegionB r

-- | C-k
killLineE :: YiM ()
killLineE = withUnivArg $ \a -> case a of
               Nothing -> killRestOfLineE
               Just n -> replicateM_ (2*n) killRestOfLineE

-- | Kill the rest of line
killRestOfLineE :: YiM ()
killRestOfLineE =
    do eol <- withBuffer atEol
       l <- withBuffer readRestOfLnB
       killringPut l
       withBuffer deleteToEol
       when eol $
            do c <- withBuffer readB
               killringPut [c]
               withBuffer deleteB

-- | C-y
yankE :: YiM ()
yankE = do (text:_) <- killringGet
           --kr@(Killring _ _ _) <- getDynamic undefined
           --let text = show kr
           withBuffer $ do pointB >>= setSelectionMarkPointB
                           insertN text
                           unsetMarkB

-- | M-w
killRingSaveE :: YiM ()
killRingSaveE = do text <- withBuffer (readRegionB =<< getRegionB)
                   killringPut text
                   withBuffer unsetMarkB
-- | M-y

-- TODO: Handle argument, verify last command was a yank
yankPopE :: YiM ()
yankPopE = do r <- withBuffer getRegionB
              withBuffer $ deleteRegionB r
              kr@Killring {krContents = ring} <- getDynamic
              setDynamic $ kr {krContents = tail ring ++ [head ring]}
              yankE

-- | C-M-w
appendNextKillE :: YiM ()
appendNextKillE = killringModify (\kr -> kr {krKilled=True})
