{-# LANGUAGE ScopedTypeVariables, MagicHash, ExistentialQuantification, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
-- Copyright (c) Jean-Philippe Bernardy 2005-2007.

module Yi.Dynamic 
 (
  -- * Nonserializable (\"config\") dynamics
  YiConfigVariable,
  ConfigVariables, configVariableA,
  -- * Serializable dynamics
  YiVariable, 
  DynamicValues, dynamicValueA,
 )
  where

import Prelude ()
import Yi.Prelude

import Data.Maybe(fromJust)
import Data.HashMap.Strict as M
import Data.Monoid
import Data.ConcreteTypeRep
import Data.Binary
import Data.Typeable(cast)
import Data.ByteString.Lazy(ByteString)
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
import qualified Data.Dynamic as D

--------------------------------- Nonserializable dynamics
-- | Class of values that can go in a 'ConfigDynamic' or a 'ConfigDynamicValues'.
-- These will typically go in a 'Config'. As the 'Config' has no mutable state,
-- there is no need to serialize these values: if needed, they will be set in the
-- user's configuration file. The 'Initializable' constraint ensures that, even if
-- the user hasn't customised this config variable, a value is stil available.
class (Initializable a, Typeable a) => YiConfigVariable a

-- | An \"extensible record\" of 'YiConfigVariable's. Can be constructed and accessed with 'initial' and 'configVariableA'.
--
-- This type can be thought of as a record containing /all/ 'YiConfigVariable's in existence.
newtype ConfigVariables = CV (M.HashMap ConcreteTypeRep D.Dynamic)
  deriving(Monoid)

instance Initializable ConfigVariables where initial = mempty

-- TODO use 'at'
-- | Lens for any 'YiConfigVariable'. Neither reader nor writer can fail:
-- if the user's config file hasn't set a value for a 'YiConfigVariable',
-- then the default value is used.
configVariableA :: forall a. YiConfigVariable a => Lens' ConfigVariables a
configVariableA = lens getCV setCV
  where
      setCV (CV m) v = CV (M.insert (cTypeOf (undefined :: a)) (D.toDyn v) m)
      getCV (CV m) = 
         case M.lookup (cTypeOf (undefined :: a)) m of
             Nothing -> initial
             Just x -> fromJust $ D.fromDynamic x

-- | Class of values that can go in a 'Dynamic' or a 'DynamicValues'. These are
-- typically for storing custom state in a 'FBuffer' or an 'Editor'.
class (Initializable a, Binary a, Typeable a) => YiVariable a

--------------------------- Serializable dynamics
{-
[Serialization and the use of unsafePerformIO]
To implement deserialization, we store the value as a ByteString (i.e. in its
serialized form) until someone tries to read from the Dynamic (at which time we
have access to the deserializer). To avoid having to repeatedly deserialize when
reading, we cheat (unsafePerformIO) and cache the deserialized value.

A pure implementation would be possible if we omitted this caching, which gives
at least some justification for the impurity.
-}

{-
Currently, we don't export 'Dynamic' as there are no users of it in Yi. It is 
hard to see where a 'Dynamic' would be preferable to a 'DynamicValues'.
-}

-- | Serializable, initializable dynamically-typed values.
newtype Dynamic = D (IORef DynamicHelper)
  deriving(Typeable)

data DynamicHelper
  = forall a. YiVariable a => Dynamic !a
  | Serial !ConcreteTypeRep !ByteString

-- | Build a 'Dynamic'
toDyn :: YiVariable a => a -> Dynamic
toDyn a = D (unsafePerformIO (newIORef $! Dynamic a))

-- | Try to extract a value from the 'Dynamic'.
fromDynamic :: forall a. YiVariable a => Dynamic -> Maybe a
fromDynamic (D r) = unsafePerformIO (readIORef r >>= f) where
   f (Dynamic b) = return $ cast b
   f (Serial tr bs) = 
      if cTypeOf (undefined :: a) == tr 
      then do
          let b = decode bs
          writeIORef r (Dynamic b)
          return (Just b)
      else return Nothing

-- | Converts a dynamic to a serializable value
toSerialRep :: Dynamic -> (ConcreteTypeRep, ByteString)
toSerialRep (D r) = 
  case unsafePerformIO (readIORef r) of
      Dynamic a -> (cTypeOf a, encode a)
      Serial ctr bs -> (ctr, bs)

-- | Converts a serializable value to a dynamic.
fromSerialRep :: (ConcreteTypeRep, ByteString) -> Dynamic
fromSerialRep (ctr, bs) = D (unsafePerformIO (newIORef (Serial ctr bs)))

instance Binary Dynamic where
    put = put . toSerialRep
    get = fromSerialRep <$> get

---------------------- Dynamic records
-- | An extensible record, indexed by type.
newtype DynamicValues = DV (M.HashMap ConcreteTypeRep Dynamic)
  deriving(Typeable, Monoid)

-- TODO use 'at'
-- | Lens for a dynamic component. If the component is not found, the value 'initial' is used.
dynamicValueA :: forall a. YiVariable a => Lens' DynamicValues a
dynamicValueA = lens getDynamicValue setDynamicValue
    where
      setDynamicValue :: DynamicValues -> a -> DynamicValues
      setDynamicValue (DV dv) v = DV (M.insert (cTypeOf (undefined :: a)) (toDyn v) dv)

      getDynamicValue :: DynamicValues -> a
      getDynamicValue (DV dv) = case M.lookup (cTypeOf (undefined::a)) dv of
                                   Nothing -> initial
                                   Just x -> fromJust $ fromDynamic x

instance Binary DynamicValues where
    put (DV dv) = put dv
    get = DV <$> get

instance Initializable DynamicValues where  initial = mempty


{-
TODO: since a 'DynamicValues' is now serialisable, it could potentially
exist for a long time (days/months?). No operations are provided to remove
entries from a 'DynamicValues'. If these start accumulating a lot of junk,
it may be necessary to prune them (perhaps keep track of access date for
'YiVariable's and remove the ones more than a month old?).
-}
