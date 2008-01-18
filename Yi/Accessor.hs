-- Copyright (C) 2008 JP Bernardy
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.


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


