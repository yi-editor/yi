{-# LANGUAGE MultiParamTypeClasses #-}

-- Copyright (c) 2008 Gustav Munkby

-- | This module defines a string representation in terms of
-- | ByteStrings stored in a finger tree.
module Yi.FingerString (
  FingerString,
  fromString, fromByteString, fromLazyByteString,
  toString, toByteString, toLazyByteString,
  rebalance,
  null, head, tail, empty, take, drop, append, splitAt, count, length,
  elemIndices, findSubstring, findSubstrings, elemIndexEnd, elemIndicesEnd
) where

import Prelude hiding (null, head, tail, length, take, drop, splitAt, head, tail, foldl, reverse)
import qualified Data.List as L

import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB

import qualified Data.FingerTree as T
import Data.FingerTree hiding (null, empty)

import Data.Monoid
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)

chunkSize :: Int
chunkSize = 128

newtype Size = Size { unSize :: Int }
newtype FingerString = FingerString { unFingerString :: FingerTree Size ByteString }
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

toLazyByteString :: FingerString -> LB.ByteString
toLazyByteString = LB.fromChunks . toList . unFingerString

toByteString :: FingerString -> ByteString
toByteString = B.concat . toList . unFingerString

toString :: FingerString -> String
toString = LB.unpack . toLazyByteString

fromLazyByteString :: LB.ByteString -> FingerString
fromLazyByteString = FingerString . toTree
  where
    toTree b | LB.null b = T.empty
    toTree b = let (h,t) = LB.splitAt (fromIntegral chunkSize) b in
               (B.concat $ LB.toChunks h) <| toTree t

fromByteString :: ByteString -> FingerString
fromByteString = FingerString . toTree
  where
    toTree b | B.null b = T.empty
    toTree b = let (h,t) = B.splitAt chunkSize b in h <| toTree t

fromString :: String -> FingerString
fromString = FingerString . toTree
  where
    toTree [] = T.empty
    toTree b = let (h,t) = L.splitAt chunkSize b in B.pack h <| toTree t

-- | Optimize the tree, to contain equally sized substrings
rebalance :: FingerString -> FingerString
rebalance = fromLazyByteString . toLazyByteString

null :: FingerString -> Bool
null (FingerString a) = T.null a

head :: FingerString -> Char
head (FingerString a) = case T.viewl a of
  EmptyL -> error "FingerString.head: empty string"
  x :< _ -> B.head x

tail :: FingerString -> FingerString
tail (FingerString a) = case T.viewl a of
  EmptyL -> error "FingerString.tail: empty string"
  x :< r -> FingerString $ (B.tail x) -| r

empty :: FingerString
empty = FingerString T.empty

-- | Get the length of the standard string.
length :: FingerString -> Int
length = unSize . measure . unFingerString

-- | Append two strings by merging the two finger trees.
append :: FingerString -> FingerString -> FingerString
append (FingerString a) (FingerString b) = FingerString $
    case T.viewr a of
      EmptyR -> b
      l :> x -> case T.viewl b of
                  EmptyL  -> a
                  x' :< r -> if B.length x + B.length x' < chunkSize
                               then l >< singleton (x `B.append` x') >< r
                               else a >< b

take, drop :: Int -> FingerString -> FingerString
take n = fst . splitAt n
drop n = snd . splitAt n

-- | Split the string at the specified position.
splitAt :: Int -> FingerString -> (FingerString, FingerString)
splitAt n (FingerString t) =
  case T.viewl c of
    x :< r | n' /= 0 ->
      let (lx, rx) = B.splitAt n' x in (FingerString $ l |- lx, FingerString $ rx -| r)
    _ -> (FingerString l, FingerString c)
  where
    (l, c) = T.split ((> n) . unSize) t
    n' = n - unSize (measure l)

-- | Count the number of occurrences of the specified character.
count :: Char -> FingerString -> Int
count x = fromIntegral . LB.count x . toLazyByteString

-- | Get the last index of the specified character
elemIndexEnd :: Char -> FingerString -> Maybe Int
elemIndexEnd x = listToMaybe . elemIndicesEnd x

-- | Get all indices of the specified character, in reverse order.
-- This function has good lazy behaviour: taking the head of the resulting list is O(1)
elemIndicesEnd :: Char -> FingerString -> [Int]
elemIndicesEnd x = treeEIE . unFingerString
  where
    treeEIE :: FingerTree Size ByteString -> [Int]
    treeEIE t = case T.viewr t of
      l :> s -> fmap (+ unSize (measure l)) (L.reverse (B.elemIndices x s)) ++ treeEIE l
      EmptyR -> []

-- | Get all indices of the specified character
-- This function has good lazy behaviour: taking the head of the resulting list is O(1)
elemIndices :: Char -> FingerString -> [Int]
elemIndices x = map fromIntegral . LB.elemIndices x . toLazyByteString

-- | Determine the first index of the ByteString in the buffer.
findSubstring :: ByteString -> FingerString -> Maybe Int
findSubstring x = listToMaybe . findSubstrings x

-- | Determine the indices of the given ByteString in the buffer.
findSubstrings :: ByteString -> FingerString -> [Int]
findSubstrings x m = [i | i <- elemIndices (B.head x) m, x `isPrefixOf` drop i m]

-- | Determine whether the ByteString is a prefix of the buffer.
isPrefixOf :: ByteString -> FingerString -> Bool
isPrefixOf x = LB.isPrefixOf (LB.fromChunks [x]) . toLazyByteString
