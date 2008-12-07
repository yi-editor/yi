-- Copyright (C) 2008 JP Bernardy

-- | A module for "rich" accesssors.

module Yi.Accessor (Accessor, mkAccessor, putA, getA, every, getsA, 
                    modA, getVal, (^:), (^=), (^.)) where
import Prelude hiding ((.), id)
import Data.Accessor
import Control.Monad.State
import Control.Category
import Data.Accessor.Basic (T, self, fromLens)
import Data.Traversable
import Data.Foldable

instance Category T where
    id = self
    (.) = (<.)

mkAccessor :: (whole -> part) -> ((part -> part) -> (whole -> whole)) -> Accessor whole part 
mkAccessor getter modifier = fromLens $ \whole -> (getter whole, \part -> modifier (const part) whole) 

getsA :: MonadState s m => Accessor s p -> (p -> a) -> m a
getsA a f = gets (f . getVal a)

every :: Traversable t => Accessor whole part -> Accessor (t whole) (t part)
every a = accessor (fmap (getVal a)) (\parts wholes -> zipT (setVal a) parts wholes)

zipT :: Traversable t => (a -> b -> c) -> t a -> t b -> t c
zipT f ta tb = result
  where step []     _ = error "zipT: non matching structures!"
        step (b:bs) a = (bs,f a b)
        ([], result) = mapAccumL step (toList tb) ta
