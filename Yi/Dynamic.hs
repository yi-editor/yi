--- Copyright (c) Jean-Philippe Bernardy 2005-2007.
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


module Yi.Dynamic where

import Data.Dynamic
import Data.Maybe

import Yi.Accessor
import Data.Map as M
-- ---------------------------------------------------------------------
-- | Class of values that can go in the extensible state component
--
class Typeable a => Initializable a where
    initial :: a

-- | An extensible record, indexed by type
type DynamicValues = M.Map String Dynamic


-- | Accessor a dynamic component
dynamicValueA :: Initializable a => Accessor DynamicValues a
dynamicValueA = Accessor getDynamicValue modifyDynamicValue
    where
      modifyDynamicValue :: forall a. Initializable a => (a -> a) -> DynamicValues -> DynamicValues
      modifyDynamicValue f = flip M.alter (show $ typeOf (undefined::a)) $ \m ->
                                Just $ toDyn $ f $ case m of
                                                 Nothing -> initial
                                                 Just x -> fromJust $ fromDynamic x

      getDynamicValue :: forall a. Initializable a => DynamicValues -> a
      getDynamicValue dv = case M.lookup (show $ typeOf (undefined::a)) dv of
                             Nothing -> initial
                             Just x -> fromJust $ fromDynamic x

-- | The empty record
emptyDV :: DynamicValues
emptyDV = M.empty
