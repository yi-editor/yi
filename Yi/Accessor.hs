-- Copyright (C) 2008 JP Bernardy

-- | A module for "rich" accesssors.

module Yi.Accessor (Accessor, mkAccessor, setA, getA, getsA, 
                    modifyA, getsAndModifyA, getVal, (^:), (^=)) where
import Prelude hiding ((.), id)
import Data.Accessor
import Control.Monad.State
import Control.Category
import Data.Accessor.Basic (T, self, fromLens)

instance Category T where
    id = self
    (.) = (<.)

mkAccessor :: (whole -> part) -> ((part -> part) -> (whole -> whole)) -> Accessor whole part 
mkAccessor getter modifier = fromLens $ \whole -> (getter whole, \part -> modifier (const part) whole) 

getsA :: MonadState s m => Accessor s p -> (p -> a) -> m a
getsA a f = gets (f . getVal a)

modifyA :: MonadState s m => Accessor s p -> (p -> p) -> m ()
modifyA = modA

setA :: MonadState s m => Accessor s p -> p -> m ()
setA = putA

getsAndModifyA :: MonadState s m => Accessor s p -> (p -> (p,a)) -> m a
getsAndModifyA a f = do
  b <- getA a
  let (b',x) = f b
  setA a b'
  return x


