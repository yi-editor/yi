{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module Yi.Fuzzy.Zipper where

import GHC.Exts (IsList(..))
import Data.Monoid
import Prelude as P
import Data.Binary (Binary(..))
import Control.Monad (replicateM, mapM_)


data Zipper a = Zipper ![a] ![a]
  deriving (Eq,Show)

instance Monoid (Zipper a) where
  mempty = Zipper [] []
  mappend z0 z1 = fromList $ toList z0 <> toList z1 -- EXPENSIVE!

instance IsList (Zipper a) where
  type Item (Zipper a) = a
  fromList = Zipper []
  toList = right . toStart

instance Functor Zipper where
  fmap f (Zipper ls rs) = Zipper (fmap f ls) (fmap f rs)

instance Binary b => Binary (Zipper b) where
    put (Zipper ls rs) = do
      put (length ls)
      mapM_ put ls
      put (length rs)
      mapM_ put rs

    get = do
      llen <- get
      ls <- replicateM llen get
      rlen <- get
      rs <- replicateM rlen get
      pure $ Zipper ls rs


current :: Zipper a -> Maybe a
current (Zipper _ rs) =
  case rs of
    []  -> Nothing
    r:_ -> Just r

mapWithCurrent :: (a -> b) -> (a -> b)  -> Zipper a -> Zipper b
mapWithCurrent f fcur (Zipper ls rs) =
  case rs of
    []     -> Zipper (f <$> ls) []
    c:rs' -> Zipper (f <$> ls) (fcur c:(f <$> rs'))

right :: Zipper a -> [a]
right (Zipper _ rs) = rs

left :: Zipper a -> [a]
left (Zipper ls _) = ls

filter :: (a -> Bool) -> Zipper a -> Zipper a
filter p (Zipper ls rs) = Zipper (P.filter p ls) (P.filter p rs)

goRight :: Zipper a -> Zipper a
goRight z@(Zipper ls rs) =
  case rs of
    []    -> z
    r:rs' -> Zipper (r:ls) rs'

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper ls rs) =
  case ls of
    []    -> z
    l:ls' -> Zipper ls' (l:rs)

toStart :: Zipper a -> Zipper a
toStart z@(Zipper ls rs) =
  case ls of
    [] -> z
    l:ls' -> Zipper ls' (l:rs)

toEnd :: Zipper a -> Zipper a
toEnd z@(Zipper ls rs) =
  case rs of
    [] -> z
    r:rs' -> Zipper (r:ls) rs'


