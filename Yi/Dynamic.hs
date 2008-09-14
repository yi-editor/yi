{-# LANGUAGE ScopedTypeVariables, MagicHash, ExistentialQuantification #-}
-- Copyright (c) Jean-Philippe Bernardy 2005-2007.

module Yi.Dynamic 
 (
  Initializable(..),
  toDyn, fromDynamic, dynamicValueA, emptyDV,
  Typeable, Dynamic, DynamicValues
 )
  where

import GHC.Exts
import Data.Maybe
import Data.Typeable
import Data.Binary
import Yi.Accessor
import Data.Map as M
-- ---------------------------------------------------------------------
-- | Class of values that can go in the extensible state component
--


class (Typeable a, Binary a) => Initializable a where
    initial :: a

-- Unfortunately, this is not serializable: there is no way to recover a type from a TypeRep.
data Dynamic = forall a. Initializable a => Dynamic a

-- | An extensible record, indexed by type
type DynamicValues = M.Map String Dynamic

toDyn :: Initializable a => a -> Dynamic
toDyn = Dynamic

fromDynamic :: forall a. Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic b) = if typeOf (undefined :: a) == typeOf b then Just (unsafeCoerce# b) else Nothing

instance (Binary a, Typeable a) => Initializable (Maybe a) where
    initial = Nothing

-- | Accessor for a dynamic component
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
