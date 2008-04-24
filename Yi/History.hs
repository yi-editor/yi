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
import qualified Data.Map as M

type Histories = M.Map String History

instance (Typeable k, Typeable v) => Initializable (M.Map k v) where
    initial = M.empty

data History = History {_historyCurrent :: Int,
                        _historyContents :: [String]}

    deriving (Show, Typeable)
instance Initializable History where
    initial = (History (-1) [])

dynKeyA :: (Initializable v, Ord k) => k -> Accessor (M.Map k v) v
dynKeyA k = Accessor (maybe initial id . M.lookup k) (\f -> M.alter (upd f) k)
    where upd f Nothing = Just (f initial)
          upd f (Just x) = Just (f x)


miniBuffer = "minibuffer"

historyUp :: EditorM ()
historyUp = historyMove 1

historyDown :: EditorM ()
historyDown = historyMove (-1)

historyStart :: EditorM ()
historyStart = historyStartGen miniBuffer

historyStartGen ident = do
  (History _cur cont) <- getA (dynKeyA ident .> dynA)
  setA (dynKeyA ident .> dynA) (History 0 (nub ("":cont)))
  debugHist

historyFinish :: EditorM ()
historyFinish = historyFinishGen miniBuffer (withBuffer0 elemsB)

historyFinishGen ident getCurValue = do
  (History _cur cont) <- getA (dynKeyA ident .> dynA)
  curValue <- getCurValue
  setA (dynKeyA ident .> dynA) $ History (-1) (nub $ dropWhile null $ (curValue:cont))

historyGetGen ident = do
  (History cur cont) <- getA (dynKeyA ident .> dynA)
  return $ case cont of
    [] -> ""
    (x:_) -> x
  

-- TODO: scrap
debugHist :: EditorM ()
debugHist = return ()
  

historyMove :: Int -> EditorM ()
historyMove delta = replaceBufferContent =<< historyMoveGen miniBuffer delta (withBuffer0 elemsB) 

replaceBufferContent :: String -> EditorM ()
replaceBufferContent newvalue = withBuffer0 $ do
              sz <- sizeB
              moveTo 0
              deleteN sz
              insertN newvalue


historyMoveGen :: String -> Int -> EditorM String -> EditorM String
historyMoveGen ident delta getCurValue = do
  (History cur cont) <- getA (dynKeyA ident .> dynA) 
  curValue <- getCurValue
  let len = length cont
      next = cur + delta
      nextValue = cont !! next
  case (next < 0, next >= len) of
    (True, _) -> do printMsg $ "end of " ++ ident ++ " history, no next item." 
                    return curValue
    (_, True) -> do printMsg $ "beginning of " ++ ident ++ " history, no previous item."
                    return curValue
    (_,_) -> do
         setA (dynKeyA ident .> dynA) (History next (take cur cont ++ [curValue] ++ drop (cur+1) cont))
         debugHist
         return nextValue
