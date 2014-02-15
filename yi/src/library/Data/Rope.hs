{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- Consider splitting off as a separate package
-- Copyright (c) 2008 Gustav Munkby
-- Copyright (c) 2008 Jean-Philippe Bernardy

-- | This module defines a Rope representation.

-- While the representation are ByteStrings stored in a finger tree, the indices
-- are actually in number of characters.

-- This is currently based on utf8-string, but a couple of other packages might be
-- better: text, compact-string.

-- At the moment none of them has a lazy
-- implementation, which forces us to always export plain Strings.
-- (Utf8-string does not have a proper newtype)

module Data.Rope (
   Rope,

   -- * Conversions to Rope
   fromString,

   -- * Conversions from Rope
   toString, toReverseString,

   -- * List-like functions
   null, empty, take, drop,  length, reverse, countNewLines,

   split, splitAt, splitAtLine,

   append, concat,

   -- * IO
   readFile, writeFile,

   -- * Low level functions
   splitAtChunkBefore
  ) where

import Prelude hiding (null, head, tail, length, take, drop, splitAt, head, tail, foldl, reverse, readFile, writeFile, concat)
import qualified Data.List as L

import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as B (append, concat)
import qualified Data.ByteString as Byte
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB (toChunks, fromChunks, null, readFile, split)
import qualified Data.ByteString.Lazy.UTF8 as LB

import qualified Data.FingerTree as T
import Data.FingerTree hiding (null, empty, reverse, split)

import Data.Binary
import Data.Char (ord)
import Data.Monoid
import Data.String (IsString(..))

import System.IO.Cautious (writeFileL)

defaultChunkSize :: Int
defaultChunkSize = 128 -- in chars! (chunkSize requires this to be <= 256)

-- The FingerTree does not store measurements for single chunks, which
-- means that the length of chunks often have to be recomputed.
mkChunk :: ByteString -> Chunk
mkChunk s = Chunk (fromIntegral $ B.length s) s
data Chunk = Chunk { chunkSize :: {-# UNPACK #-} !Word8, fromChunk :: {-# UNPACK #-} !ByteString }
  deriving (Eq, Show)

data Size = Indices {charIndex :: {-# UNPACK #-} !Int, lineIndex :: {-# UNPACK #-} !Int} -- lineIndex is lazy because we do not often want the line count. However, we need this to avoid stack overflows on large files!
  deriving Show

instance Monoid Size where
    mempty = Indices 0 0
    mappend (Indices c1 l1) (Indices c2 l2) = Indices (c1+c2) (l1+l2)

newtype Rope = Rope { fromRope :: FingerTree Size Chunk }
   deriving (Eq, Show)

(-|) :: Chunk -> FingerTree Size Chunk -> FingerTree Size Chunk
b -| t | chunkSize b == 0 = t
       | otherwise        = b <| t

(|-) :: FingerTree Size Chunk -> Chunk -> FingerTree Size Chunk
t |- b | chunkSize b == 0 = t
       | otherwise        = t |> b

-- Newlines are preserved by UTF8 encoding and decoding
newline :: Word8
newline = fromIntegral (ord '\n')

instance Measured Size Chunk where
   measure (Chunk l s) = Indices (fromIntegral l)  -- note that this is the length in characters, not bytes.
                                 (Byte.count newline s)

-- | The 'Foldable' instance of 'FingerTree' only defines 'foldMap', so the 'foldr' needed for 'toList' is inefficient,
-- and can cause stack overflows. So, we roll our own (somewhat inefficient) version of 'toList' to avoid this.
toList :: Measured v a => FingerTree v a -> [a]
toList t = case viewl t of
              c :< cs -> c : toList cs
              EmptyL -> []

toLazyByteString :: Rope -> LB.ByteString
toLazyByteString = LB.fromChunks . fmap fromChunk . toList . fromRope

reverse :: Rope -> Rope
reverse = Rope . fmap' (mkChunk . B.fromString . L.reverse . B.toString . fromChunk) . T.reverse . fromRope

toReverseString :: Rope -> String
toReverseString = concatMap (L.reverse . B.toString . fromChunk) . toList . T.reverse . fromRope

toString :: Rope -> String
toString = LB.toString . toLazyByteString

fromLazyByteString :: LB.ByteString -> Rope
fromLazyByteString = Rope . toTree T.empty
   where
     toTree acc b | LB.null b = acc
                  | otherwise = let (h,t) = LB.splitAt (fromIntegral defaultChunkSize) b
                                    chunk = mkChunk $ B.concat $ LB.toChunks h
                                in acc `seq` chunk `seq` toTree (acc |> chunk) t

instance IsString Rope where
    fromString = Rope . toTree T.empty
       where
         toTree acc [] = acc
         toTree acc b  = let (h,t) = L.splitAt defaultChunkSize b
                             chunk = mkChunk $ B.fromString h
                         in acc `seq` chunk `seq` toTree (acc |> chunk) t

null :: Rope -> Bool
null (Rope a) = T.null a

empty :: Rope
empty = Rope T.empty

-- | Get the length of the string. (This information cached, so O(1) amortized runtime.)
length :: Rope -> Int
length = charIndex . measure . fromRope

-- | Count the number of newlines in the strings. (This information cached, so O(1) amortized runtime.)
countNewLines :: Rope -> Int
countNewLines = lineIndex . measure . fromRope

-- | Append two strings by merging the two finger trees.
append :: Rope -> Rope -> Rope
append (Rope a) (Rope b) = Rope $
     case T.viewr a of
       EmptyR -> b
       l :> Chunk len x -> case T.viewl b of
                   EmptyL  -> a
                   Chunk len' x' :< r -> if fromIntegral len + fromIntegral len' < defaultChunkSize
                                then l >< singleton (Chunk (len + len') (x `B.append` x')) >< r
                                else a >< b

concat :: [Rope] -> Rope
concat = L.foldl1' append

take, drop :: Int -> Rope -> Rope
take n = fst . splitAt n
drop n = snd . splitAt n

-- | Split the string at the specified position.
splitAt :: Int -> Rope -> (Rope, Rope)
splitAt n (Rope t) =
   case T.viewl c of
     Chunk len x :< r | n' /= 0 ->
       let (lx, rx) = B.splitAt n' x in (Rope $ l |> Chunk (fromIntegral n') lx, Rope $ Chunk (len - fromIntegral n') rx -| r)
     _ -> (Rope l, Rope c)
   where
     (l, c) = T.split ((> n) . charIndex) t
     n' = n - charIndex (measure l)

-- | Split the rope on a chunk, so that the desired
--   position lies within the first chunk of the second rope.
splitAtChunkBefore :: Int -> Rope -> (Rope, Rope)
splitAtChunkBefore n (Rope t) =
  let (l, c) = T.split ((> n) . charIndex) t in (Rope l, Rope c)

-- | Split before the specified line. Lines are indexed from 0.
splitAtLine :: Int -> Rope -> (Rope, Rope)
splitAtLine n | n <= 0     = \r -> (empty, r)
              | otherwise = splitAtLine' (n-1)

-- | Split after the specified line. Lines are indexed from 0.
splitAtLine' :: Int -> Rope -> (Rope, Rope)
splitAtLine' n (Rope t) =
   case T.viewl c of
     ch@(Chunk _ x) :< r ->
       let (lx, rx) = cutExcess excess x
           excess = lineIndex (measure l) + lineIndex (measure ch) - n - 1
       in (Rope $ l |- mkChunk lx, Rope $ mkChunk rx -| r)
     _ -> (Rope l, Rope c)
   where
     (l, c) = T.split ((n <) . lineIndex) t

split :: Word8 -> Rope -> [Rope]
split c = map fromLazyByteString . LB.split c . toLazyByteString

cutExcess :: Int -> ByteString -> (ByteString, ByteString)
cutExcess i s = let idx = gt i $ L.reverse $ Byte.elemIndices newline s
                in Byte.splitAt (idx+1) s -- take one extra byte to that the newline is found on the left.
    where gt _ []     = Byte.length s
          gt 0 (x:_ ) = x
          gt n (_:xs) = gt (n-1) xs


instance Binary Rope where
     put = put . toString
     get = fromString `fmap` get


writeFile :: FilePath -> Rope -> IO ()
writeFile f = writeFileL f . toLazyByteString

readFile :: FilePath -> IO Rope
readFile f = fromLazyByteString `fmap` LB.readFile f
