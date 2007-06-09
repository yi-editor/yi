--
-- Copyright (c) 2005,2007 Jean-Philippe Bernardy
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

-- "command history" implementation

module Yi.History where

import Yi.Yi

import Yi.Buffer
import Data.Char
import Data.List
import Data.Dynamic

import Control.Monad.Trans

import Yi.Editor


data History = History {_historyCurrent :: Int, 
                        _historyContents :: [String]} 

    deriving (Show, Typeable)
instance Initializable History where
    initial = (History (-1) [])
    

historyUp :: YiM ()
historyUp = historyMove 1

historyDown :: YiM () 
historyDown = historyMove (-1)

historyStart :: YiM ()
historyStart = do
  (History _cur cont) <- getDynamic
  setDynamic (History 0 (nub ("":cont)))
  debugHist

historyFinish :: YiM ()
historyFinish = do
  (History _cur cont) <- getDynamic
  curValue <- readAllE
  setDynamic $ History (-1) (nub $ dropWhile null $ (curValue:cont))

debugHist :: YiM ()
debugHist = do
  h :: History <- getDynamic
  lift $ logPutStrLn (show h)

historyMove :: Int -> YiM ()
historyMove delta = do
  (History cur cont) <- getDynamic
  curValue <- readAllE
  let len = length cont
      next = cur + delta
      nextValue = cont !! next
  case (next < 0, next >= len) of
    (True, _) -> msgE "end of history, no next item."
    (_, True) -> msgE "beginning of history, no previous item."
    (_,_) -> do 
         setDynamic (History next (take cur cont ++ [curValue] ++ drop (cur+1) cont))
         debugHist
         withBuffer $ do
              sz <- sizeB
              moveTo 0
              deleteN sz
              insertN nextValue

