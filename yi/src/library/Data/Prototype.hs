{-# LANGUAGE Rank2Types #-}
-- | Support for OO-like prototypes.
module Data.Prototype where

import Data.Function (fix)

-- | A prototype. Typically the parameter will be a record type.
-- Fields can be defined in terms of others fields, with the
-- idea that some of these definitons can be overridden.
--
-- Example:
--
-- > data O = O {f1, f2, f3 :: Int}
-- >     deriving Show
-- > o1 = Proto $ \self -> O
-- >   {
-- >    f1 = 1,
-- >    f2 = f1 self + 1,  -- 'f1 self' refers to the overriden definition of f1.
-- >    f3 = f1 self + 2
-- >   }
newtype Proto a = Proto {fromProto :: a -> a}

-- | Get the value of a prototype.
-- This can return bottom in case some fields are recursively defined in terms of each other.
extractValue :: Proto t -> t
extractValue (Proto o) = fix o

-- | Override a prototype. Fields can be defined in terms of their definition in the base prototype.
--
-- Example:
--
-- > o2 = o1 `override` \super self -> super
-- >    {
-- >    f1 = f1 super + 10,
-- >    f3 = f3 super + 1
-- >    }

override :: Proto a -> (a -> a -> a) -> Proto a
override (Proto base) extension = Proto (\self -> let super = base self
                                                  in extension super self)

-- | Field access
(.->) :: forall t a. Proto t -> (t -> a) -> a
p .-> f = f (extractValue p)



