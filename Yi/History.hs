{-# LANGUAGE PatternSignatures, DeriveDataTypeable #-}

-- Copyright (c) 2005,2007,2008 Jean-Philippe Bernardy

-- "command history" implementation

module Yi.History where

import Yi.Buffer
import Data.Char
import Data.List
import Data.Dynamic
import Yi.Dynamic
import Yi.Editor
import Yi.Accessor

data History = History {_historyCurrent :: Int,
                        _historyContents :: [String]}

    deriving (Show, Typeable)
instance Initializable History where
    initial = (History (-1) [])


historyUp :: EditorM ()
historyUp = historyMove 1

historyDown :: EditorM ()
historyDown = historyMove (-1)

historyStart :: EditorM ()
historyStart = do
  (History _cur cont) <- getA dynA
  setA dynA (History 0 (nub ("":cont)))
  debugHist

historyFinish :: EditorM ()
historyFinish = do
  (History _cur cont) <- getA dynA
  curValue <- withBuffer0 elemsB
  setA dynA $ History (-1) (nub $ dropWhile null $ (curValue:cont))

-- TODO: scrap
debugHist :: EditorM ()
debugHist = return ()

historyMove :: Int -> EditorM ()
historyMove delta = do
  (History cur cont) <- getA dynA
  curValue <- withBuffer0 elemsB
  let len = length cont
      next = cur + delta
      nextValue = cont !! next
  case (next < 0, next >= len) of
    (True, _) -> printMsg "end of history, no next item."
    (_, True) -> printMsg "beginning of history, no previous item."
    (_,_) -> do
         setA dynA (History next (take cur cont ++ [curValue] ++ drop (cur+1) cont))
         debugHist
         withBuffer0 $ do
              sz <- sizeB
              moveTo 0
              deleteN sz
              insertN nextValue

