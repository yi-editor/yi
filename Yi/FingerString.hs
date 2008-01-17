--
-- Copyright (c) 2008 Gustav Munkby
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
--


-- | This module defines a string representation in terms of
-- | ByteStrings stored in a finger tree.
module Yi.FingerString (
  FingerString,
  fromString, toString, fromByteString, toByteString, rebalance,
  length, take, drop, append, splitAt, count,
  elemIndices, findSubstring, elemIndexEnd, elemIndicesEnd
) where

import Prelude hiding (length, take, drop, splitAt, head, tail, foldl, reverse)
import qualified Data.List as L

import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)

import Data.FingerTree as T
import Data.FingerTree ((|>), (<|), (><))

import Data.Monoid
import Data.Foldable (foldl)
import Data.Maybe (listToMaybe)

chunkSize :: Int
chunkSize = 128

data Size = Size { unSize :: Int }
data FingerString = FingerString { unFingerString :: FingerTree Size ByteString }
  deriving (Eq, Show)

instance Monoid Size where
  mempty = Size 0
  (Size n) `mappend` (Size m) = Size $ n + m

instance Measured Size ByteString where
  measure = Size . B.length

-- | Convert into a ByteString.
toByteString :: FingerString -> ByteString
toByteString = foldl B.append B.empty . unFingerString

-- | Convert from a ByteString.
fromByteString :: ByteString -> FingerString
fromByteString = FingerString . treeFromByteString
  where
    treeFromByteString b | B.null b = T.empty
    treeFromByteString b = let (h,t) = B.splitAt chunkSize b in h <| treeFromByteString t

-- | Convert into a standard String.
toString :: FingerString -> String
toString = B.unpack . toByteString

-- | Convert from a standard String.
fromString :: String -> FingerString
fromString = fromByteString . B.pack

-- | Optimize the tree, to contain equally sized substrings
rebalance :: FingerString -> FingerString
rebalance = fromByteString . toByteString

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
      let (lx, rx) = B.splitAt n' x in (FingerString $ l |> lx, FingerString $ rx <| r)
    _ -> (FingerString l, FingerString c)
  where
    (l, c) = T.split ((> n) . unSize) t
    n' = n - unSize (measure l)

-- | Count the number of occurrences of the specified character.
count :: Char -> FingerString -> Int
count x = foldl counter 0 . unFingerString
  where counter c = (c +) . (B.count x)

-- | Get the last index of the specified character
elemIndexEnd :: Char -> FingerString -> Maybe Int
elemIndexEnd x t = listToMaybe $ elemIndicesEnd x t

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
elemIndices x = treeEI . unFingerString
  where
    treeEI :: FingerTree Size ByteString -> [Int]
    treeEI t = case T.viewl t of
      s :< r -> B.elemIndices x s ++ fmap (B.length s +) (treeEI r)
      EmptyL -> []

-- | Determine the first index of the ByteString in the buffer.
findSubstring :: ByteString -> FingerString -> Maybe Int
findSubstring x m = if B.null x then Just 0 else case (findSubstrings x m) of
                                                   Nothing -> Nothing
                                                   Just a -> Just $ L.head a

-- | Determine the indices of the given ByteString in the buffer.
findSubstrings :: ByteString -> FingerString -> Maybe [Int]
findSubstrings x m = if (L.length strings == 0) then Nothing else Just strings
    where strings = [i | i <- elemIndices (B.head x) m, x `isPrefixOf` drop i m]

-- | Determine whether the ByteString is a prefix of the buffer.
isPrefixOf :: ByteString -> FingerString -> Bool
isPrefixOf x = treeIPO x . unFingerString
  where
    treeIPO :: ByteString -> FingerTree Size ByteString -> Bool
    treeIPO x' t = case T.viewl t of
      s :< r -> x' `B.isPrefixOf` s ||
        (s `B.isPrefixOf` x' && treeIPO (B.drop (B.length s) x') r)
      EmptyL -> False
