{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances #-}
module Yi.WindowSet
  ( module Data.List.PointedList
  , WindowSet
  , current
  , currentA
  , add
  , forward
  , backward
  , move
  , delete
  , deleteOthers
  , setFocus
  , withFocus
  , debug
  ) where

import Control.Monad
import Control.Monad.Trans
import Data.Accessor
import Data.List.PointedList hiding (delete)

import Yi.Debug (logPutStrLn)

#ifdef TESTING
import Test.QuickCheck

instance Arbitrary a => Arbitrary (WindowSet a) where
    arbitrary = return PointedList `ap` arbitrary `ap` arbitrary `ap` arbitrary
#endif

type WindowSet = PointedList

current :: WindowSet a -> a
current = focus

currentA :: Accessor (WindowSet a) a
currentA = accessor current (\b (PointedList a _ c) -> PointedList a b c)

add :: a -> WindowSet a -> WindowSet a
add = insertRight

forward :: WindowSet a -> WindowSet a
forward (PointedList a b []) = let (x:xs) = reverse a in
                                 PointedList [] x (xs ++ [b])
forward pl = tryNext pl

backward :: WindowSet a -> WindowSet a
backward (PointedList [] b c) = let (x:xs) = reverse c in
                                  PointedList (b:xs) x []
backward pl = tryPrevious pl

-- | Move the focused window or tab to the specified index
move :: Int ->  WindowSet a -> WindowSet a
move n ws@(PointedList a b c)
    | Prelude.length a <  n = PointedList (reverse (take (n - (Prelude.length a)) c) ++ a) b (drop n c)
    | Prelude.length a == n = ws
    | {-length a >  n-}
      otherwise = PointedList (take n (reverse a)) b (drop n (reverse a) ++ c)

delete :: WindowSet a -> WindowSet a
delete ws = case deleteLeft ws of
              Nothing  -> ws
              Just ws' -> ws'

deleteOthers :: WindowSet a -> WindowSet a
deleteOthers (PointedList _ b _) = PointedList [] b []

-- | Move to the provided window in the list of windows
setFocus :: (Show a, Eq a) => a -> WindowSet a -> WindowSet a
setFocus w ws@(PointedList a b c)
  | b == w     = ws
  | w `elem` a = setFocus w (backward ws)
  | w `elem` c = setFocus w (forward ws)
  | otherwise  = error $ "Lost window: " ++ show w ++ " among " ++ show ws

withFocus :: WindowSet a -> WindowSet (a, Bool)
withFocus (PointedList a b c) = PointedList (zip a (repeat False)) (b, True) (zip c (repeat False))

debug :: (Show a, MonadIO m) => String -> WindowSet a -> m ()
debug msg ws = logPutStrLn $ msg ++ ": " ++ show ws
