{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables, ExistentialQuantification, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
-- Copyright 2008 JP Bernardy
-- | Basic types useful everywhere we play with buffers.
module Yi.Buffer.Basic where

import Data.Binary    
import Yi.Prelude
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Typeable

-- | Direction of movement inside a buffer
data Direction = Backward
               | Forward
                 deriving (Eq,Ord,Typeable,Show {-! Binary !-})

reverseDir :: Direction -> Direction
reverseDir Forward = Backward
reverseDir Backward = Forward


-- | reverse if Backward
mayReverse :: Direction -> [a] -> [a]
mayReverse Forward = id
mayReverse Backward = reverse

-- | 'direction' is in the same style of 'maybe' or 'either' functions,
-- It takes one argument per direction (backward, then forward) and a
-- direction to select the output.
directionElim :: Direction -> a -> a -> a
directionElim Backward b _ = b
directionElim Forward  _ f = f

-- | A mark in a buffer
newtype Mark = Mark {markId::Int} deriving (Eq, Ord, Show, Typeable, Binary)

-- | Reference to a buffer.
newtype BufferRef = BufferRef Int
    deriving (Eq, Ord, Typeable, Binary)
deriving instance Num BufferRef

instance Show BufferRef where
    show (BufferRef r) = "B#" ++ show r

-- | A point in a buffer
newtype Point = Point {fromPoint :: Int}           -- offset in the buffer (#bytes, NOT codepoints)
    deriving (Eq, Ord, Enum, Bounded, Typeable, Binary)

deriving instance Num Point
deriving instance Real Point
deriving instance Integral Point
instance Show Point where
    show (Point p) = show p

-- | Size of a buffer region
newtype Size = Size {fromSize :: Int}             -- size in bytes (#bytes, NOT codepoints)
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Binary)

instance SemiNum Point Size where
    Point p +~ Size s = Point (p + s)
    Point p -~ Size s = Point (p - s)
    Point p ~- Point q = Size (p - q)

utf8Size :: String -> Size
utf8Size s = Size $ B.length $ UTF8.fromString s

-- fromUTF8ByteString :: B.ByteString -> String
-- fromUTF8ByteString = UTF8.toString

fromString :: String -> LB.ByteString
fromString = LazyUTF8.fromString



--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 286904097

instance Binary Direction
    where put (Backward) = putWord8 0
          put (Forward) = putWord8 1
          get = getWord8 >>= (\tag_ -> case tag_ of
                                           0 -> return Backward
                                           1 -> return Forward)
