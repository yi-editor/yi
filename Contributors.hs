{-# OPTIONS -fglasgow-exts #-}

import Data.Array (elems, Array)
import Data.ByteString.Char8 (pack, unpack, ByteString) 
import Data.Char
import Data.List (nub, sort, sortBy, intersperse)
import Prelude hiding (lines, readFile)
import Text.Regex.Posix
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

capitalize s = BS.cons (toUpper $ BS.head s) (BS.tail s)

splitN n [] = []
splitN n l = let (c,ls) = splitAt n l in (l : splitN n ls)

tx = "Thu Oct 14 05:40:06 CEST 2004 "

unquote x 
    | BS.head x == '\'' && BS.last x == '\'' = unquote (BS.init $ BS.tail $ x)
    | otherwise = x

name :: ByteString -> ByteString
name tag 
     | match :: Array Int ByteString <- tag =~ pack "^\"?(.+)<.*>\"?$", [_,name] <- elems match = name
     | match :: Array Int ByteString <- tag =~ pack "^<?(.*)@(.*)\\.name>?$", [_,firstname, lastname] <- elems match 
             = BS.concat . intersperse (BS.pack " ") . map capitalize $ [firstname, lastname]
     | match :: Array Int ByteString <- tag =~ pack "^<?(.*)@.*>?$", [_,user] <- elems match = user
     | otherwise = tag

trim = fst . BS.spanEnd isSpace

name' x = case map toLower x of 
            "a.d.clark"             -> "Allan Clark"
            "alson"                 -> "Alson Kemp"
            "andrii.z"              -> "Andrii Zvorygin"
            "cgibbard"              -> "Cale Gibbard"
            "coreyoconnor"          -> "Corey O'Connor"
            "dons"                  -> "Don Stewart"
            "grddev"                -> "Gustav Munkby"
            "jeanphilippe.bernardy" -> "Jean-Philippe Bernardy"
            "mlang"                 -> "Mario Lang"
            "mwotton"               -> "Mark Wotton"
            "newsham"               -> "Tim Newsham"
            "shae"                  -> "Shae Erisson"
            "sjw"                   -> "Simon Winwood"
            "vintermann"            -> "Harald Korneliussen"
            "vivian.mcphail"        -> "Vivian McPhail"
            "zapf"                  -> "Bastiaan Zapf"
            _                       -> x

nick = pack . name' . unpack

main = do
  f <- BS.getContents
  let ls = BS.lines f
      ps = filter (not . isSpace . BS.head) $ filter (not . BS.null) $ ls
      contrs = M.fromListWith (+) $ flip zip (repeat 1) $ map (trim . nick . name . unquote . BS.dropWhile isSpace . BS.drop (length tx)) $ ps
  -- print (length ps)
  -- mapM print $ sortBy (comparing fst) $ M.toList $ contrs
  mapM BS.putStrLn $ M.keys contrs
  return ()

on f g x y = f (g x) (g y)

comparing f x y = compare (f x) (f y)