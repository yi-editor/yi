{-# LANGUAGE CPP #-}
--
-- Copyright (c) 2007 Jean-Philippe Bernardy
--
--

module Yi.WindowSet where
-- FIXME: export abstractly
-- TODO: rename to RoundRobin or somesuch.

import Prelude ()
import Yi.Prelude

import Control.Monad.Trans
import Data.Accessor (accessor)
import Data.Binary
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.List hiding (elem)
import Control.Applicative
import Control.Monad
#ifdef TESTING
import Test.QuickCheck


instance Arbitrary a => Arbitrary (WindowSet a) where
    arbitrary = return WindowSet `ap` arbitrary `ap` arbitrary `ap` arbitrary

#endif

data WindowSet a = WindowSet { before::[a], current::a, after :: [a] }
    deriving (Show, Eq {-! Binary !-})

instance Foldable WindowSet where
    foldMap f (WindowSet b c a) = getDual (foldMap (Dual . f) b) `mappend` (f c) `mappend` foldMap f a

instance Functor WindowSet where
    fmap f (WindowSet b c a) = WindowSet (fmap f b) (f c) (fmap f a)

instance Traversable WindowSet where
    traverse f (WindowSet b c a) = WindowSet <$> (reverse <$> traverse f (reverse b)) <*> f c <*> traverse f a

currentA :: Accessor (WindowSet a) a
currentA = accessor current (\b (WindowSet a _ c) -> WindowSet a b c)

fromList :: [a] -> Maybe (WindowSet a)
fromList [] = Nothing
fromList (x:xs) = Just $ WindowSet [] x xs

new :: a -> WindowSet a
new w = WindowSet [] w []

-- | Add a window, focus it.
add :: a -> WindowSet a -> WindowSet a
add w (WindowSet b c a) = WindowSet (c:b) w a

-- | Add a window to the end, and focus it.
addLast :: a -> WindowSet a -> WindowSet a
addLast w (WindowSet b c a) = WindowSet (reverse a ++ c:b) w ([]::[a]) 

next :: WindowSet a -> a
next = current . forward

-- | Delete a window
delete :: WindowSet a -> WindowSet a
delete (WindowSet [] c []) = WindowSet [] c [] -- never delete the last window
delete (WindowSet (b:bs) _ a) = WindowSet bs b a
delete (WindowSet [] _ (a:as)) = WindowSet [] a as

-- property: delete (add w) ws == ws

size :: WindowSet a -> Int
size (WindowSet as _ bs) = length as + 1 + length bs

deleteOthers :: WindowSet a -> WindowSet a
deleteOthers (WindowSet _ c _) = WindowSet [] c []

forward :: WindowSet a -> WindowSet a
forward (WindowSet [] c []) = WindowSet [] c []
forward (WindowSet b c (a:as)) = WindowSet (c:b) a as
forward (WindowSet b c []) = WindowSet [] (last b) ((reverse (init b)) ++ [c])

backward :: WindowSet a -> WindowSet a
backward (WindowSet [] c []) = WindowSet [] c []
backward (WindowSet (b:bs) c a) = WindowSet bs b (c:a)
backward (WindowSet [] c a) = WindowSet (c:reverse (init a)) (last a) []

-- | Move the focused window or tab to the specified index
move :: Int ->  WindowSet a -> WindowSet a
move n ws@(WindowSet a b c)
    | length a <  n = WindowSet (reverse (take (n - (length a)) c) ++ a) b (drop n c)
    | length a == n = ws
    | {-length a >  n-}
      otherwise = WindowSet (take n (reverse a)) b (drop n (reverse a) ++ c)

setFocus :: (Show a, Eq a) => a -> WindowSet a -> WindowSet a
setFocus w ws@(WindowSet b c a) 
    | c == w = ws
    | w `elem` a = setFocus w (forward ws)
    | w `elem` b = setFocus w (backward ws)
    | otherwise = error $ "Lost window: " ++ show w ++ " among " ++ show ws

withFocus :: WindowSet a -> WindowSet (a, Bool)
withFocus (WindowSet b c a) = WindowSet (zip b (repeat False)) (c, True) (zip a (repeat False))

focusIndex :: Int -> WindowSet a -> WindowSet a
focusIndex n ws = WindowSet (reverse b) c a
    where (b,c:a) = splitAt n $ toList ws

debug :: (Show a, MonadIO m) => String -> WindowSet a -> m ()
debug msg (WindowSet b c a) = logPutStrLn $ msg ++ ": " ++ show b ++ show c ++ show a



--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 1932088040

instance Binary t1 => Binary (WindowSet t1)
    where put (WindowSet x1
                         x2
                         x3) = return () >> (put x1 >> (put x2 >> put x3))
          get = case 0 of
                    0 -> ap (ap (ap (return WindowSet) get) get) get
