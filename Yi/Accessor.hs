-- Copyright (C) 2008 JP Bernardy
--


-- * A module for "rich" accesssors.

module Yi.Accessor where
import Control.Monad.State
import Data.Traversable as Traversable
import Data.Foldable

-- | A way to access and modify a part of a complex structure.
-- Categorically, an arrow from @whole@ to @part@.
data Accessor whole part 
    = Accessor { getter :: whole -> part,
                 modifier :: (part -> part) -> (whole -> whole)
               }

-- Should be made instance of the upcoming Control.Category class as such:
-- import qualified Prelude
-- import Prelude hiding (id,(.))
-- 
-- instance Category (->) where
-- 	id = Prelude.id
-- 	(.) = (Prelude..)
-- 
-- class Category cat where
-- 	-- | the identity morphism
-- 	id :: cat a a
-- 
-- 	-- | morphism composition
-- 	(.) :: cat b c -> cat a b -> cat a c
-- 
-- instance Category Accessor where
--     id = Accessor id id
--     (.) = (.>)

-- | Compose accessors
(.>) :: Accessor t1 t -> Accessor t2 t1 -> Accessor t2 t
Accessor g1 m1 .> Accessor g2 m2 = Accessor (g1 . g2) (m2 . m1)

getA :: MonadState s m => Accessor s p -> m p
getA = gets . getter

getsA :: MonadState s m => Accessor s p -> (p -> a) -> m a
getsA a f = gets (f . getter a)

modifyA :: MonadState s m => Accessor s p -> (p -> p) -> m ()
modifyA a f = modify (modifier a f)

modifyAllA :: (MonadState s m, Functor f) => Accessor s (f w) -> Accessor w p -> (p -> p) -> m ()
modifyAllA a a' f = modifyA a (fmap $ modifier a' f)


setA :: MonadState s m => Accessor s p -> p -> m ()
setA a p = modifyA a (const p)


allA :: forall whole part t. Traversable t => Accessor whole part -> Accessor (t whole) (t part)
allA (Accessor g m) = Accessor (fmap g) modifier'
    where modifier' mapParts wholes = distribute wholes (toList $ mapParts $ fmap g wholes)
          distribute wholes parts = fst $ runState (Traversable.mapM setOne wholes) parts
          setOne whole = do
            h' <- gets head
            modify tail
            return (m (const h') whole)

-- (#=) :: MonadState s m => Accessor s p -> p -> m ()
-- (#=) = setA

getsAndModifyA :: MonadState s m => Accessor s p -> (p -> (p,a)) -> m a
getsAndModifyA a f = do
  b <- getA a
  let (b',x) = f b
  modifyA a (const b')
  return x


