{-# OPTIONS -fglasgow-exts #-}

import Data.Array (elems, Array)
import Data.ByteString.Char8 (pack, unpack, ByteString) 
import Data.Char
import Data.List (nub, sort, sortBy)
import Prelude hiding (lines, readFile)
import Text.Regex.Posix
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

splitN n [] = []
splitN n l = let (c,ls) = splitAt n l in (l : splitN n ls)

tx = "Thu Oct 14 05:40:06 CEST 2004 "

name :: ByteString -> ByteString
name tag 
     | match :: Array Int ByteString <- tag =~ pack "^\"?(.+)<.*>\"?$", [_,name] <- elems match = name
     | match :: Array Int ByteString <- tag =~ pack "^<?(.*)@.*>?$", [_,user] <- elems match = user
     | otherwise = tag

trim = fst . BS.spanEnd isSpace

name' "Zapf"                  = "Bastiaan Zapf"
name' "a.d.clark"             = "Allan Clark"
name' "alson"                 = "Alson Kemp"
name' "andrii.z"              = "Andrii Zvorygin"
name' "cgibbard"              = "Cale Gibbard"
name' "dons"                  = "Don Stewart"
name' "jeanphilippe.bernardy" = "Jean-Philippe Bernardy"
name' "mlang"                 = "Mario Lang"
name' "mwotton"               = "Mark Wotton"
name' "newsham"               = "Tim Newsham"
name' "shae"                  = "Shae Erisson"
name' "sjw"                   = "Simon Winwood"
name' "vintermann"            = "Harald Korneliussen"
name' "vivian.mcphail"        = "Vivian McPhail"
name' x                       = x

nick = pack . name' . unpack

main = do
  f <- BS.getContents
  let ls = BS.lines f
      ps = filter (not . isSpace . BS.head) $ filter (not . BS.null) $ ls
      contrs = M.fromListWith (+) $ flip zip (repeat 1) $ map (trim . nick . name . BS.dropWhile isSpace . BS.drop (length tx)) $ ps
  -- print (length ps)
  -- mapM print $ sortBy (comparing fst) $ M.toList $ contrs
  mapM BS.putStrLn $ M.keys contrs
  return ()

on f g x y = f (g x) (g y)

comparing f x y = compare (f x) (f y)