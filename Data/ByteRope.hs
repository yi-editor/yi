{-# LANGUAGE MultiParamTypeClasses #-}
-- Consider splitting off as a separate package
-- Copyright (c) 2008 Gustav Munkby

-- | This module defines a Rope representation in terms of
-- ByteStrings stored in a finger tree.
module Data.ByteRope (
  ByteRope,

  -- * Conversions to ByteRope
  fromString, fromByteString, fromLazyByteString,

  -- * Conversions from ByteRope
  toString, toReverseString,
  toLazyByteString, toReverseLazyByteString,
  toByteString, 

  -- rebalance, -- should not be needed.
  -- List-like functions
  null, empty, take, drop, append, splitAt, count, length,

  -- * searching
  elemIndices, findSubstring, findSubstrings, elemIndexEnd, elemIndicesEnd
 ) where

import Prelude hiding (null, head, tail, length, take, drop, splitAt, head, tail, foldl, reverse)
import qualified Data.List as L

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB

import qualified Data.FingerTree as T
import Data.FingerTree hiding (null, empty)

import Data.Binary
import Data.Word
import Data.Monoid
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)

chunkSize :: Int
chunkSize = 128

newtype Size = Size { fromSize :: Int }
newtype ByteRope = ByteRope { fromByteRope :: FingerTree Size ByteString }
  deriving (Eq, Show)

(-|) :: ByteString -> FingerTree Size ByteString -> FingerTree Size ByteString
b -| t | B.null b  = t
       | otherwise = b <| t

(|-) :: FingerTree Size ByteString -> ByteString -> FingerTree Size ByteString
t |- b | B.null b  = t
       | otherwise = t |> b

instance Monoid Size where
  mempty = Size 0
  (Size n) `mappend` (Size m) = Size $ n + m

instance Measured Size ByteString where
  measure = Size . B.length

toLazyByteString :: ByteRope -> LB.ByteString
toLazyByteString = LB.fromChunks . toList . fromByteRope

toReverseLazyByteString :: ByteRope -> LB.ByteString
toReverseLazyByteString = LB.fromChunks . map B.reverse . toList . T.reverse . fromByteRope


toByteString :: ByteRope -> ByteString
toByteString = B.concat . toList . fromByteRope

toString :: ByteRope -> [Word8]
toString = LB.unpack . toLazyByteString

toReverseString :: ByteRope -> [Word8]
toReverseString = LB.unpack . toReverseLazyByteString

fromLazyByteString :: LB.ByteString -> ByteRope
fromLazyByteString = ByteRope . toTree
  where
    toTree b | LB.null b = T.empty
    toTree b = let (h,t) = LB.splitAt (fromIntegral chunkSize) b in
               (B.concat $ LB.toChunks h) <| toTree t

fromByteString :: ByteString -> ByteRope
fromByteString = ByteRope . toTree
  where
    toTree b | B.null b = T.empty
    toTree b = let (h,t) = B.splitAt chunkSize b in h <| toTree t

fromString :: [Word8] -> ByteRope
fromString = ByteRope . toTree
  where
    toTree [] = T.empty
    toTree b = let (h,t) = L.splitAt chunkSize b in B.pack h <| toTree t

-- | Optimize the tree, to contain equally sized substrings
rebalance :: ByteRope -> ByteRope
rebalance = fromLazyByteString . toLazyByteString

null :: ByteRope -> Bool
null (ByteRope a) = T.null a

head :: ByteRope -> Word8
head (ByteRope a) = case T.viewl a of
  EmptyL -> error "ByteRope.head: empty string"
  x :< _ -> B.head x

tail :: ByteRope -> ByteRope
tail (ByteRope a) = case T.viewl a of
  EmptyL -> error "ByteRope.tail: empty string"
  x :< r -> ByteRope $ (B.tail x) -| r

empty :: ByteRope
empty = ByteRope T.empty

-- | Get the length of the standard string.
length :: ByteRope -> Int
length = fromSize . measure . fromByteRope

-- | Append two strings by merging the two finger trees.
append :: ByteRope -> ByteRope -> ByteRope
append (ByteRope a) (ByteRope b) = ByteRope $
    case T.viewr a of
      EmptyR -> b
      l :> x -> case T.viewl b of
                  EmptyL  -> a
                  x' :< r -> if B.length x + B.length x' < chunkSize
                               then l >< singleton (x `B.append` x') >< r
                               else a >< b

take, drop :: Int -> ByteRope -> ByteRope
take n = fst . splitAt n
drop n = snd . splitAt n

-- | Split the string at the specified position.
splitAt :: Int -> ByteRope -> (ByteRope, ByteRope)
splitAt n (ByteRope t) =
  case T.viewl c of
    x :< r | n' /= 0 ->
      let (lx, rx) = B.splitAt n' x in (ByteRope $ l |- lx, ByteRope $ rx -| r)
    _ -> (ByteRope l, ByteRope c)
  where
    (l, c) = T.split ((> n) . fromSize) t
    n' = n - fromSize (measure l)

-- | Count the number of occurrences of the specified character.
count :: Word8 -> ByteRope -> Int
count x = fromIntegral . LB.count x . toLazyByteString

-- | Get the last index of the specified character
elemIndexEnd :: Word8 -> ByteRope -> Maybe Int
elemIndexEnd x = listToMaybe . elemIndicesEnd x

-- | Get all indices of the specified character, in reverse order.
-- This function has good lazy behaviour: taking the head of the resulting list is O(1)
elemIndicesEnd :: Word8 -> ByteRope -> [Int]
elemIndicesEnd x = treeEIE . fromByteRope
  where
    treeEIE :: FingerTree Size ByteString -> [Int]
    treeEIE t = case T.viewr t of
      l :> s -> fmap (+ fromSize (measure l)) (L.reverse (B.elemIndices x s)) ++ treeEIE l
      EmptyR -> []

-- | Get all indices of the specified character
-- This function has good lazy behaviour: taking the head of the resulting list is O(1)
elemIndices :: Word8 -> ByteRope -> [Int]
elemIndices x = map fromIntegral . LB.elemIndices x . toLazyByteString

-- | Determine the first index of the ByteString in the buffer.
findSubstring :: ByteString -> ByteRope -> Maybe Int
findSubstring x = listToMaybe . findSubstrings x

-- | Determine the indices of the given ByteString in the buffer.
findSubstrings :: ByteString -> ByteRope -> [Int]
findSubstrings x m = [i | i <- elemIndices (B.head x) m, x `isPrefixOf` drop i m]

-- | Determine whether the ByteString is a prefix of the buffer.
isPrefixOf :: ByteString -> ByteRope -> Bool
isPrefixOf x = LB.isPrefixOf (LB.fromChunks [x]) . toLazyByteString

instance Binary ByteRope where
    put = put . toLazyByteString
    get = fromLazyByteString `fmap` get