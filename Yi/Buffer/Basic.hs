{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
-- Copyright 2008 JP Bernardy
-- | Basic types useful everywhere we play with buffers.
module Yi.Buffer.Basic where
    
import Yi.Prelude
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Typeable

-- | Direction of movement inside a buffer
data Direction = Backward
               | Forward
                 deriving (Eq,Ord,Typeable,Show)

reverseDir :: Direction -> Direction
reverseDir Forward = Backward
reverseDir Backward = Forward


-- | reverse if Backward
mayReverse :: Direction -> [a] -> [a]
mayReverse Forward = id
mayReverse Backward = reverse

-- | A mark in a buffer
newtype Mark = Mark {markId::Int} deriving (Eq, Ord, Show, Typeable)
staticInsMark, staticSelMark :: Mark
staticInsMark = Mark (-1) -- the insertion mark
staticSelMark = Mark (-2) -- the selection mark
dummyFromMark, dummyToMark :: Mark
dummyFromMark = Mark 1
dummyToMark = Mark 2

-- | Reference to a buffer.
newtype BufferRef = BufferRef Int
    deriving (Num, Eq, Ord, Typeable)

instance Show BufferRef where
    show (BufferRef r) = "B#" ++ show r

-- | A point in a buffer
newtype Point = Point {fromPoint :: Int}           -- offset in the buffer (#bytes, NOT codepoints)
    deriving (Eq, Ord, Num, Enum, Real, Integral, Bounded, Typeable)

instance Show Point where
    show (Point p) = show p

-- | Size of a buffer region
newtype Size = Size {fromSize :: Int}             -- size in bytes (#bytes, NOT codepoints)
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

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

