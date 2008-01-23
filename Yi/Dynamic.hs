{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright (c) Jean-Philippe Bernardy 2005-2007.

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
