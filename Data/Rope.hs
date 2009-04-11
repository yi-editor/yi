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
   null, empty, take, drop, append, splitAt, splitAtLine, length, reverse, countNewLines,
 
   -- * IO
   readFile, writeFile
  ) where
 
import Prelude hiding (null, head, tail, length, take, drop, splitAt, head, tail, foldl, reverse, readFile, writeFile)
import qualified Data.List as L
 
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as B (null, append, concat, elemIndices)
import qualified Data.ByteString as Byte 
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB (toChunks, fromChunks, writeFile, null, readFile)
import qualified Data.ByteString.Lazy.UTF8 as LB 
 
import qualified Data.FingerTree as T
import Data.FingerTree hiding (null, empty, reverse)
 
import Data.Binary
import Data.Char (ord)
import Data.Monoid
import Data.Foldable (toList)
 
chunkSize :: Int
chunkSize = 128 -- in chars!
 
data Size = Indices {charIndex :: !Int, lineIndex :: Int} -- lineIndex is lazy because we do not often want the line count.
  deriving Show

instance Monoid Size where
    mempty = Indices 0 0
    mappend (Indices c1 l1) (Indices c2 l2) = Indices (c1+c2) (l1+l2)
 
newtype Rope = Rope { fromRope :: FingerTree Size ByteString }
   deriving (Eq, Show)
 
(-|) :: ByteString -> FingerTree Size ByteString -> FingerTree Size ByteString
b -| t | B.null b  = t
        | otherwise = b <| t
 
(|-) :: FingerTree Size ByteString -> ByteString -> FingerTree Size ByteString
t |- b | B.null b  = t
        | otherwise = t |> b
 
instance Measured Size ByteString where
   measure s = Indices (B.length s)  -- note that this is the length in characters, not bytes.
                       (B.foldr (\c -> if c == '\n' then (1+) else id) 0 s)
 
toLazyByteString :: Rope -> LB.ByteString
toLazyByteString = LB.fromChunks . toList . fromRope

reverse :: Rope -> Rope
reverse = Rope . fmap' (B.fromString . L.reverse . B.toString) . T.reverse . fromRope
 
toReverseString :: Rope -> String
toReverseString = L.concat . map (L.reverse . B.toString) . toList . T.reverse . fromRope
  
toString :: Rope -> String
toString = LB.toString . toLazyByteString
 
fromLazyByteString :: LB.ByteString -> Rope
fromLazyByteString = Rope . toTree
   where
     toTree b | LB.null b = T.empty
     toTree b = let (h,t) = LB.splitAt (fromIntegral chunkSize) b in (B.concat $ LB.toChunks $ h) <| toTree t

 
fromString :: String -> Rope
fromString = Rope . toTree
   where
     toTree [] = T.empty
     toTree b = let (h,t) = L.splitAt chunkSize b in B.fromString h <| toTree t
 
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
       l :> x -> case T.viewl b of
                   EmptyL  -> a
                   x' :< r -> if B.length x + B.length x' < chunkSize
                                then l >< singleton (x `B.append` x') >< r
                                else a >< b
 
take, drop :: Int -> Rope -> Rope
take n = fst . splitAt n
drop n = snd . splitAt n
 
-- | Split the string at the specified position.
splitAt :: Int -> Rope -> (Rope, Rope)
splitAt n (Rope t) =
   case T.viewl c of
     x :< r | n' /= 0 ->
       let (lx, rx) = B.splitAt n' x in (Rope $ l |- lx, Rope $ rx -| r)
     _ -> (Rope l, Rope c)
   where
     (l, c) = T.split ((> n) . charIndex) t
     n' = n - charIndex (measure l)

-- | Split before the specified line. Lines are indexed from 0.
splitAtLine :: Int -> Rope -> (Rope, Rope)
splitAtLine n | n <= 0     = \r -> (empty, r)
              | otherwise = splitAtLine' (n-1)

-- | Split after the specified line. Lines are indexed from 0.
splitAtLine' :: Int -> Rope -> (Rope, Rope)
splitAtLine' n (Rope t) =
   case T.viewl c of
     x :< r ->
       let (lx, rx) = cutExcess excess x 
           excess = lineIndex (measure l) + lineIndex (measure x) - n - 1
       in (Rope $ l |- lx, Rope $ rx -| r)
     _ -> (Rope l, Rope c)
   where
     (l, c) = T.split ((n <) . lineIndex) t


cutExcess :: Int -> ByteString -> (ByteString, ByteString)
cutExcess i s = let idx = gt i $ L.reverse $ B.elemIndices (fromIntegral $ ord $ '\n') s
                in Byte.splitAt (idx+1) s -- take one extra byte to that the newline is found on the left.
    where gt _ []     = B.length s
          gt 0 (x:_ ) = x
          gt n (_:xs) = gt (n-1) xs
          

instance Binary Rope where
     put = put . toString
     get = fromString `fmap` get



writeFile :: FilePath -> Rope -> IO ()
writeFile f r = LB.writeFile f $ toLazyByteString r

readFile :: FilePath -> IO Rope
readFile f = fromLazyByteString `fmap` LB.readFile f