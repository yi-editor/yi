{-# LANGUAGE ScopedTypeVariables #-}
module Data.List.PointedList.Extras
  ( filterr
  , catMaybesr
  ) where

import           Control.Monad                (guard)
import           Data.List.PointedList        (PointedList(..))
import qualified Data.Maybe            as M   (catMaybes)

-- | filter a pointed list, preferring the right list as the new focus if the focus is lost.
filterr :: (a -> Bool) -> PointedList a -> Maybe (PointedList a)
filterr filt pl = catMaybesr $ fmap (\a -> guard (filt a) >> Just a) pl

-- | catMaybes on a pointed list, preferring the right list as the new focus if the focus is lost.
catMaybesr :: forall a . PointedList (Maybe a) -> Maybe (PointedList a)
catMaybesr (PointedList mls mf mrs) = case mf of
  Nothing -> shiftFocus rs ss
  Just f  -> pure $ PointedList rs f ss
 where
  rs, ss :: [a]
  rs = M.catMaybes mls
  ss = M.catMaybes mrs

  shiftFocus :: [a] -> [a] -> Maybe (PointedList a)
  shiftFocus []     (f:rs) = pure $ PointedList [] f rs
  shiftFocus (f:ls) []     = pure $ PointedList ls f []
  shiftFocus _      _      = Nothing


