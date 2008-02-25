--
-- Copyright (c) 2007 Jean-Philippe Bernardy
--
--

module Yi.WindowSet where
-- FIXME: export abstractly

import Prelude hiding (elem, error)

import Yi.Debug
import Control.Monad.Trans
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative
import Control.Monad
import Yi.Accessor
#ifdef TESTING
import Test.QuickCheck


instance Arbitrary a => Arbitrary (WindowSet a) where
    arbitrary = return WindowSet `ap` arbitrary `ap` arbitrary `ap` arbitrary

#endif

data WindowSet a = WindowSet { before::[a], current::a, after :: [a] }
    deriving (Show, Eq)

instance Foldable WindowSet where
    foldMap f (WindowSet b c a) = getDual (foldMap (Dual . f) b) `mappend` (f c) `mappend` foldMap f a

instance Functor WindowSet where
    fmap f (WindowSet b c a) = WindowSet (fmap f b) (f c) (fmap f a)

instance Traversable WindowSet where
    traverse f (WindowSet b c a) = WindowSet <$> (reverse <$> traverse f (reverse b)) <*> f c <*> traverse f a

currentA :: Accessor (WindowSet a) a
currentA = Accessor current modifyCurrent
    where modifyCurrent :: (a -> a) -> WindowSet a -> WindowSet a
          modifyCurrent f (WindowSet b c a) = WindowSet b (f c) a


new :: a -> WindowSet a
new w = WindowSet [] w []

-- | Add a window, focus it.
add :: a -> WindowSet a -> WindowSet a
add w (WindowSet b c a) = WindowSet (c:b) w a

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


