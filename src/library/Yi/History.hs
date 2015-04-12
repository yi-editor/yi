{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Control.Applicative (liftA3, (<$>))
import           Control.Lens        (Lens', lens, set, (^.))
import           Data.Binary         (Binary, get, put)
import           Data.Default        (Default, def)
import           Data.List           (nub)
import qualified Data.Map            as M (Map, findWithDefault, insert, mapKeys)
import           Data.Monoid         (mempty, (<>))
import qualified Data.Text           as T (Text, isPrefixOf, null, pack, unpack)
import qualified Data.Text.Encoding  as E (decodeUtf8, encodeUtf8)
import           Data.Typeable       (Typeable)
import           Yi.Buffer           (elemsB, replaceBufferContent)
import           Yi.Editor
import qualified Yi.Rope             as R (fromText, toText)
import           Yi.Types            (YiVariable)

newtype Histories = Histories (M.Map T.Text History)
                  deriving (Show, Eq, Typeable)

instance Binary Histories where
  put (Histories m) = put $ M.mapKeys T.unpack m
  get = Histories . M.mapKeys T.pack <$> get

instance Default Histories where
  def = Histories def

data History = History { _historyCurrent  :: Int
                       , _historyContents :: [T.Text]
                       , _historyPrefix   :: T.Text
                       } deriving (Show, Eq, Typeable)

instance Default History where
    def = History (-1) [] mempty

instance Binary History where
  put (History cu co pr) =
    put cu >> put (map E.encodeUtf8 co) >> put (E.encodeUtf8 pr)
  get = liftA3 History get (fmap E.decodeUtf8 <$> get) (E.decodeUtf8 <$> get)

instance YiVariable Histories

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
historyStartGen ident = do
  Histories histories <- getEditorDyn
  let (History _cur cont pref) = histories ^. dynKeyA ident
  setHistory ident (History 0 (nub ("":cont)) pref) histories

historyFinish :: EditorM ()
historyFinish = historyFinishGen miniBuffer (R.toText <$> withCurrentBuffer elemsB)

-- | Finish the current input session with history.
historyFinishGen :: T.Text -> EditorM T.Text -> EditorM ()
historyFinishGen ident getCurValue = do
  Histories histories <- getEditorDyn
  let History _cur cont pref = histories ^. dynKeyA ident
  curValue <- getCurValue
  let cont' = dropWhile (curValue ==) . dropWhile T.null $ cont
  curValue `seq`        -- force the new value, otherwise we'll hold
                        -- on to the buffer from which it's computed
    cont'         `seq` -- force checking the top of the history,
                        -- otherwise we'll build up thunks
    setHistory ident (History (-1) (curValue:cont') pref) histories

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
historyMoveGen ident delta getCurValue = do
  Histories histories <- getEditorDyn
  let History cur cont pref = histories ^. dynKeyA ident

  curValue <- getCurValue
  let len = length cont
      next = historyFind cont len cur delta pref
      nextValue = cont !! next
  case (next < 0, next >= len) of
    (True, _) -> do
      printMsg $ "end of " <> ident <> " history, no next item."
      return curValue
    (_, True) -> do
      printMsg $ "beginning of " <> ident <> " history, no previous item."
      return curValue
    (_,_) -> do
      let contents = take cur cont ++ [curValue] ++ drop (cur + 1) cont
      setHistory ident (History next contents pref) histories
      return nextValue

historyPrefixSet :: T.Text -> EditorM ()
historyPrefixSet = historyPrefixSet' miniBuffer

historyPrefixSet' :: T.Text -> T.Text -> EditorM ()
historyPrefixSet' ident pref = do
  Histories histories <- getEditorDyn
  let History cur cont _pref = histories ^. dynKeyA ident
  setHistory ident (History cur cont pref) histories

-- | Helper that sets the given history at ident and 'putEditorDyn's
-- the result.
setHistory :: (MonadEditor m, Functor m) => T.Text -- ^ identifier
           -> History -- ^ History to set
           -> M.Map T.Text History -- ^ Map of existing histories
           -> m ()
setHistory i h = putEditorDyn . Histories . set (dynKeyA i) h
