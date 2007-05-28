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

import Data.Map as M
-- ---------------------------------------------------------------------
-- | Class of values that can go in the extensible state component
--
class Typeable a => Initializable a where
    initial :: a

type DynamicValues = M.Map String Dynamic


-- | Retrieve a value from the extensible state
getDynamicValue :: forall a. Initializable a => DynamicValues -> a
getDynamicValue dv = case M.lookup (show $ typeOf (undefined::a)) dv of
                       Nothing -> initial
                       Just x -> fromJust $ fromDynamic x

-- | Insert a value into the extensible state, keyed by its type
setDynamicValue :: Initializable a => a -> DynamicValues -> DynamicValues
setDynamicValue x = M.insert (show $ typeOf x) (toDyn x)

emptyDV :: DynamicValues
emptyDV = M.empty
