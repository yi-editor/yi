-- Copyright (C) 2008 JP Bernardy

-- | A module for "rich" accesssors.

module Yi.Accessor (Accessor, mkAccessor, setA, getA, getsA, 
                    modifyA, getsAndModifyA, modifyAllA, getVal, (^:), (^=)) where
import Prelude hiding ((.), id)
import Data.Accessor
import Control.Monad.State
import Data.Traversable as Traversable
import Data.Foldable
import qualified Data.Map as M
import Control.Category
import Data.Accessor.Basic (T, self, fromLens)
import Data.Accessor.Container

instance Category T where
    id = self
    (.) = (<.)

mkAccessor :: (whole -> part) -> ((part -> part) -> (whole -> whole)) -> Accessor whole part 
mkAccessor getter modifier = fromLens $ \whole -> (getter whole, \part -> modifier (\part' -> part) whole) 

getsA :: MonadState s m => Accessor s p -> (p -> a) -> m a
getsA a f = gets (f . getVal a)

modifyA :: MonadState s m => Accessor s p -> (p -> p) -> m ()
modifyA = modA


modifyAllA :: (MonadState s m, Functor f) => Accessor s (f w) -> Accessor w p -> (p -> p) -> m ()
modifyAllA a a' f = modA a (fmap $  a' ^: f)

setA :: MonadState s m => Accessor s p -> p -> m ()
setA = putA

getsAndModifyA :: MonadState s m => Accessor s p -> (p -> (p,a)) -> m a
getsAndModifyA a f = do
  b <- getA a
  let (b',x) = f b
  setA a b'
  return x


