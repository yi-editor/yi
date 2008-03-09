-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Digest.SHA1
-- Copyright   :  (c) Ian Lynagh, Dominic Steinitz, Jun Mukai, Spencer Janssen 2003-2006
-- License     :  BSD-style (see the file ReadMe.tex)
--
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- hash is provided for backward compatibility. It
-- takes [Octet] and returns [Octet] where the length of the result
-- is always 20.
-- See <http://www.itl.nist.gov/fipspubs/fip180-1.htm> for the specification.
--
-----------------------------------------------------------------------------

module Data.Digest.SHA1( 
   SHA1,
   -- * Function Types
   initSHA1,
   digest,
   add,
   hash',
   hash,
   fileHash
   ) where

import System.IO

import Data.Char
import Data.Bits
import Data.List
import Data.Word

import qualified Data.ByteString as BS

import Codec.Utils

data SHA1 = SHA1 { state :: !ABCDE
                 , buffer :: !BS.ByteString
                 , size :: !Integer
                 }

data ABCDE = ABCDE !Word32 !Word32 !Word32 !Word32 !Word32
data XYZ = XYZ !Word32 !Word32 !Word32
type Rotation = Int

fileHash :: FilePath -> IO BS.ByteString
fileHash fpath = 
   do h <- openFile fpath ReadMode
      loop h initSHA1
   where 
      loop h sha1 =
         do eof <- hIsEOF h
            if eof then return $ digest sha1
                   else do s <- BS.hGet h 4096
                           let sha' = add sha1 s
                           sha' `seq` loop h sha'

-- | Take [Octet] and return [Octet] according to the standard.
--   The length of the result is always 20 octets or 160 bits as required
--   by the standard.

hash :: [Octet] -> [Octet]
hash = BS.unpack . hash' . BS.pack

hash' :: BS.ByteString -> BS.ByteString
hash' = digest . add initSHA1 

initSHA1 :: SHA1
initSHA1 =
    SHA1 { state = ABCDE 0x67452301 0xefcdab89 0x98badcfe 0x10325476 0xc3d2e1f0
         , buffer = BS.empty
         , size = 0
         }

digest :: SHA1 -> BS.ByteString
digest (SHA1 abcde buf siz)
    | BS.length buf > 55 =
        let pad    = BS.pack $ 128 : replicate (63 - BS.length buf) 0
            abcde' = step abcde $ BS.append buf pad
        in f $ step abcde' $ BS.pack (replicate 56 0 ++ sizeData)
    | otherwise =
        let pad = 128 : replicate (55 - BS.length buf) 0
        in f $ step abcde (BS.append buf $ BS.pack $ (pad ++ sizeData))
    where sizeData = map fromIntegral $ i2osp 8 (siz*8)
          f (ABCDE a b c d e) = BS.concat $ map (BS.pack . i2osp 4 . toInteger) [a,b,c,d,e]

add :: SHA1 -> BS.ByteString -> SHA1
add (SHA1 abcde buf siz) str
    | siz + (toInteger $ BS.length str) > 2 ^ 61 = error "string is too long"
    | BS.length str + BS.length buf < 64 =
        SHA1 abcde (BS.append buf str) siz'
    | otherwise = let (abcde', buf') = f abcde $ BS.append buf str
                  in SHA1 abcde' buf' siz'
    where siz' = siz + toInteger (BS.length str)
          f abcde str | BS.length s2 < 64 = abcde' `seq` (abcde', s2)
                      | otherwise         = f abcde' s2
              where (s1, s2) = BS.splitAt 64 str
                    abcde' = step abcde s1

step :: ABCDE -> BS.ByteString -> ABCDE 
step abcde0@(ABCDE a b c d e) words = abcde5
    where s16 = get_word_32s words
          s80 = s16 ++ (zipWith4 f0) (drop 13 s80) (drop 8 s80) (drop 2 s80) s80
          f0 a b c d = rotL (a `xor` b `xor` c `xor` d) 1
          (s20_0, s60) = splitAt 20 s80
          (s20_1, s40) = splitAt 20 s60
          (s20_2, s20) = splitAt 20 s40
          (s20_3, _)   = splitAt 20 s20
          abcde1 = foldl (doit f1 0x5a827999) abcde0 s20_0
          abcde2 = foldl (doit f2 0x6ed9eba1) abcde1 s20_1
          abcde3 = foldl (doit f3 0x8f1bbcdc) abcde2 s20_2
          ABCDE a' b' c' d' e' = foldl (doit f2 0xca62c1d6) abcde3 s20_3
          f1 (XYZ x y z) = (x .&. y) .|. ((complement x) .&. z)
          f2 (XYZ x y z) = x `xor` y `xor` z
          f3 (XYZ x y z) = (x .&. y) .|. (x .&. z) .|. (y .&. z)
          abcde5 = ABCDE (a + a') (b + b') (c + c') (d + d') (e + e')

get_word_32s :: BS.ByteString -> [Word32]
get_word_32s s = map f [0..15]
    where f i = foldl (+) 0 $ map (\n -> toEnum (fromEnum (BS.index s (i*4+n))) `shiftL` (8 * (3-n))) [0..3]

doit :: (XYZ -> Word32) -> Word32 -> ABCDE -> Word32 -> ABCDE
doit f k (ABCDE a b c d e) w = ABCDE a' a (rotL b 30) c d
 where a' = rotL a 5 + f (XYZ b c d) + e + w + k

rotL :: Word32 -> Rotation -> Word32
rotL a s = shiftL a s .|. shiftL a (s-32)

