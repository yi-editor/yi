{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell, FlexibleInstances #-}

-- Copyright (c) 2005,2007,2008 Jean-Philippe Bernardy

-- "command history" implementation

module Yi.History where

import Yi.Buffer
import Data.Binary
import Data.DeriveTH
import Data.List
import Data.Accessor.Container
import Yi.Dynamic
import Yi.Editor
import qualified Data.Map as M
import Yi.Prelude 
import Prelude ()
type Histories = M.Map String History

instance (Typeable k, Typeable v) => Initializable (M.Map k v) where
    initial = M.empty

data History = History {_historyCurrent :: Int,
                        _historyContents :: [String],
                        _historyPrefix :: String}

    deriving (Show, Typeable)
instance Initializable History where
    initial = (History (-1) [] "")
$(derive makeBinary ''History)

instance YiVariable (M.Map String History)

dynKeyA :: (Initializable v, Ord k) => k -> Accessor (M.Map k v) v
dynKeyA = mapDefault initial

miniBuffer :: String
miniBuffer = "minibuffer"

historyUp :: EditorM ()
historyUp = historyMove miniBuffer 1

historyDown :: EditorM ()
historyDown = historyMove miniBuffer (-1)

historyStart :: EditorM ()
historyStart = historyStartGen miniBuffer

-- | Start an input session with History
historyStartGen :: String -> EditorM ()
historyStartGen ident = do
  (History _cur cont pref) <- getA (dynKeyA ident . dynA)
  putA (dynKeyA ident . dynA) (History 0 (nub ("":cont)) pref)

historyFinish :: EditorM ()
historyFinish = historyFinishGen miniBuffer (withBuffer0 elemsB)

-- | Finish the current input session with history.
historyFinishGen :: String -> EditorM String -> EditorM ()
historyFinishGen ident getCurValue = do
  (History _cur cont pref) <- getA (dynKeyA ident . dynA)
  curValue <- getCurValue
  let cont' = dropWhile (curValue==) . dropWhile null $ cont
  length curValue `seq` -- force the new value, otherwise we'll hold on to the buffer from which it's computed
    cont'         `seq` -- force checking the top of the history, otherwise we'll build up thunks
    putA (dynKeyA ident . dynA) $ History (-1) (curValue:cont') pref

historyFind :: [String] -> Int -> Int -> Int -> String -> Int
historyFind cont len cur delta pref =
  case (next < 0, next >= len) of
    (True,_) -> next
    (_,True) -> next
    (_,_) -> if isPrefixOf pref (cont !! next)
      then next
      else historyFind cont len cur deltaLarger pref
  where
    next = cur + delta
    deltaLarger = delta + signum delta

historyMove :: String -> Int -> EditorM ()
historyMove ident delta = (withBuffer0 . replaceBufferContent) =<< historyMoveGen ident delta (withBuffer0 elemsB) 

historyMoveGen :: String -> Int -> EditorM String -> EditorM String
historyMoveGen ident delta getCurValue = do
  (History cur cont pref) <- getA (dynKeyA ident . dynA) 
  
  curValue <- getCurValue
  let len = length cont
      next = historyFind cont len cur delta pref
      nextValue = cont !! next
  case (next < 0, next >= len) of
    (True, _) -> do printMsg $ "end of " ++ ident ++ " history, no next item." 
                    return curValue
    (_, True) -> do printMsg $ "beginning of " ++ ident ++ " history, no previous item."
                    return curValue
    (_,_) -> do
         putA (dynKeyA ident . dynA) (History next (take cur cont ++ [curValue] ++ drop (cur+1) cont) pref)
         return nextValue

historyPrefixSet :: String -> EditorM ()
historyPrefixSet pref = historyPrefixSet' miniBuffer pref

historyPrefixSet' :: String -> String -> EditorM ()
historyPrefixSet' ident pref = do
  (History cur cont _pref) <- getA (dynKeyA ident . dynA)
  putA (dynKeyA ident . dynA) (History cur cont pref)
  return ()
