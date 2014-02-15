{-# LANGUAGE
  StandaloneDeriving,
  DeriveDataTypeable,
  ScopedTypeVariables,
  ExistentialQuantification,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  TemplateHaskell,
  DeriveGeneric,
  CPP #-}
-- Copyright 2008 JP Bernardy
-- | Basic types useful everywhere we play with buffers.
module Yi.Buffer.Basic where
import Data.Binary
import Data.Typeable
#if __GLASGOW_HASKELL__ < 708
import Data.DeriveTH
#else
import GHC.Generics (Generic)
#endif
import Data.Ix
import Data.Default
import Yi.Utils

-- | Direction of movement inside a buffer
data Direction = Backward
               | Forward
                 deriving (Eq,Ord,Typeable,Show,Bounded,Enum)

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''Direction)
#else
deriving instance Generic Direction
instance Binary Direction
#endif

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
    deriving (Eq, Ord, Typeable, Binary,
              Num)

instance Show BufferRef where
    show (BufferRef r) = "B#" ++ show r

-- | A point in a buffer
newtype Point = Point {fromPoint :: Int}           -- offset in the buffer (#codepoints, NOT bytes)
    deriving (Eq, Ord, Enum, Bounded, Typeable, Binary, Ix,
              Num, Real, Integral)

instance Show Point where
    show (Point p) = show p

-- | Size of a buffer region
newtype Size = Size {fromSize :: Int}             -- size in bytes (#bytes, NOT codepoints)
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Binary)

instance SemiNum Point Size where
    Point p +~ Size s = Point (p + s)
    Point p -~ Size s = Point (p - s)
    Point p ~- Point q = Size (abs (p - q))

-- | Window references
newtype WindowRef = WindowRef { unWindowRef :: Int }
  deriving(Eq, Ord, Enum, Show, Typeable, Binary)

instance Default WindowRef where def = WindowRef (-1)

