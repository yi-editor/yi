{-# LANGUAGE PatternSignatures, DeriveDataTypeable #-}

-- Copyright (c) 2005,2007,2008 Jean-Philippe Bernardy

-- "command history" implementation

module Yi.History where

import Yi.Yi

import Yi.Buffer
import Data.Char
import Data.List
import Data.Dynamic

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
  curValue <- withBuffer elemsB
  setDynamic $ History (-1) (nub $ dropWhile null $ (curValue:cont))

debugHist :: YiM ()
debugHist = do
  h :: History <- getDynamic
  logPutStrLn (show h)

historyMove :: Int -> YiM ()
historyMove delta = do
  (History cur cont) <- getDynamic
  curValue <- withBuffer elemsB
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

