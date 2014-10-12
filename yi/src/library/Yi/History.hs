{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.History
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- ‘Command history’ implementation.

module Yi.History where

import           Control.Applicative
import           Control.Lens
import           Data.Binary
import           Data.Default
import           Data.List
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Data.Typeable
import           Yi.Buffer
import           Yi.Dynamic
import           Yi.Editor
import qualified Yi.Rope as R

type Histories = M.Map String History


data History = History { _historyCurrent :: Int
                       , _historyContents :: [T.Text]
                       , _historyPrefix :: T.Text
                       }

    deriving (Show, Typeable)
instance Default History where
    def = History (-1) [] mempty

instance Binary History where
  put (History cu co pr) =
    put cu >> put (map E.encodeUtf8 co) >> put (E.encodeUtf8 pr)
  get = liftA3 History get (fmap E.decodeUtf8 <$> get) (E.decodeUtf8 <$> get)

instance YiVariable (M.Map String History)

dynKeyA :: (Default v, Ord k) => k -> Lens' (M.Map k v) v
dynKeyA key = lens (M.findWithDefault def key) (flip (M.insert key))

miniBuffer :: T.Text
miniBuffer = "minibuffer"

historyUp :: EditorM ()
historyUp = historyMove miniBuffer 1

historyDown :: EditorM ()
historyDown = historyMove miniBuffer (-1)

historyStart :: EditorM ()
historyStart = historyStartGen miniBuffer

-- | Start an input session with History
historyStartGen :: T.Text -> EditorM ()
historyStartGen identT = do
  let ident = T.unpack identT
  (History _cur cont pref) <- use (dynA . dynKeyA ident)
  assign (dynA . dynKeyA ident) (History 0 (nub ("":cont)) pref)

historyFinish :: EditorM ()
historyFinish = historyFinishGen miniBuffer (R.toText <$> withCurrentBuffer elemsB)

-- | Finish the current input session with history.
historyFinishGen :: T.Text -> EditorM T.Text -> EditorM ()
historyFinishGen identTODO getCurValue = do
  let ident = T.unpack identTODO
  (History _cur cont pref) <- use (dynA . dynKeyA ident)
  curValue <- getCurValue
  let cont' = dropWhile (curValue ==) . dropWhile T.null $ cont
  curValue `seq`        -- force the new value, otherwise we'll hold
                        -- on to the buffer from which it's computed
    cont'         `seq` -- force checking the top of the history,
                        -- otherwise we'll build up thunks
    assign (dynA . dynKeyA ident) $ History (-1) (curValue:cont') pref

historyFind :: [T.Text] -> Int -> Int -> Int -> T.Text -> Int
historyFind cont len cur delta pref =
  case (next < 0, next >= len) of
    (True,_) -> next
    (_,True) -> next
    (_,_) -> if pref `T.isPrefixOf` (cont !! next)
      then next
      else historyFind cont len cur deltaLarger pref
  where
    next = cur + delta
    deltaLarger = delta + signum delta

historyMove :: T.Text -> Int -> EditorM ()
historyMove ident delta = do
  s <- historyMoveGen ident delta (R.toText <$> withCurrentBuffer elemsB)
  withCurrentBuffer . replaceBufferContent . R.fromText $ s

historyMoveGen :: T.Text -> Int -> EditorM T.Text -> EditorM T.Text
historyMoveGen identTODO delta getCurValue = do
  let ident = T.unpack identTODO
  (History cur cont pref) <- use (dynA . dynKeyA ident)

  curValue <- getCurValue
  let len = length cont
      next = historyFind cont len cur delta pref
      nextValue = cont !! next
  case (next < 0, next >= len) of
    (True, _) -> do
      printMsg $ "end of " <> identTODO <> " history, no next item."
      return curValue
    (_, True) -> do
      printMsg $ "beginning of " <> identTODO <> " history, no previous item."
      return curValue
    (_,_) -> do
      assign (dynA . dynKeyA ident) (History next (take cur cont ++ [curValue] ++ drop (cur+1) cont) pref)
      return nextValue

historyPrefixSet :: T.Text -> EditorM ()
historyPrefixSet = historyPrefixSet' miniBuffer

historyPrefixSet' :: T.Text -> T.Text -> EditorM ()
historyPrefixSet' identTODO pref = do
  let ident = T.unpack identTODO
  (History cur cont _pref) <- use (dynA . dynKeyA ident)
  assign (dynA . dynKeyA ident) (History cur cont pref)
  return ()
