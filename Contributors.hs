{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Array (elems, Array)
import Data.ByteString.Char8 (pack, ByteString) 
import Data.Char
import Data.String
import Data.List (intersperse)
import Prelude hiding (lines, readFile)
import Text.Regex.Posix
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

instance IsString ByteString where
    fromString = BS.pack

capitalize s = BS.cons (toUpper $ BS.head s) (BS.tail s)

tx :: String = "Thu Oct 14 05:40:06 CEST 2004 "

unquote x 
    | BS.head x == '\'' && BS.last x == '\'' = unquote (BS.init $ BS.tail $ x)
    | otherwise = x

type M = Array Int ByteString

mkName match | [_,firstname, lastname] <- elems match
       = BS.concat . intersperse " " . map capitalize $ [firstname, lastname]

name :: ByteString -> ByteString
name tag 
     | match :: M <- tag =~ pack "^\"?(.+)<.*>\"?$", [_,name] <- elems match = name
     | match :: M <- tag =~ pack "^<?(.*)@(.*)\\.name>?$", 
       [_,_,_] <- elems match = mkName match 
     | match :: M <- tag =~ pack "^<?(.*)@.*>?$", [_,user] <- elems match = nickToName user
     | otherwise = nickToName tag

trim = fst . BS.spanEnd isSpace

nickToName :: ByteString -> ByteString
nickToName x = case BS.map toLower x of 
            "a.d.clark"             -> "Allan Clark"
            "alson"                 -> "Alson Kemp"
            "andrii.z"              -> "Andrii Zvorygin"
            "cgibbard"              -> "Cale Gibbard"
            "coreyoconnor"          -> "Corey O'Connor"
            "dons"                  -> "Don Stewart"
            "grddev"                -> "Gustav Munkby"
            "gwern0"                -> "Walter Moreira"
            "jeanphilippe.bernardy" -> "Jean-Philippe Bernardy"
            "mlang"                 -> "Mario Lang"
            "mwotton"               -> "Mark Wotton"
            "newsham"               -> "Tim Newsham"
            "nominolo"              -> "Thomas Schilling"
            "shae"                  -> "Shae Erisson"
            "sjw"                   -> "Simon Winwood"
            "vintermann"            -> "Harald Korneliussen"
            "vivian.mcphail"        -> "Vivian McPhail"
            "zapf"                  -> "Bastiaan Zapf"
            _ | match :: M <- x =~ pack "^(.*)\\.(.*)$",
                [_,_,_] <- elems match -> mkName match
              | otherwise -> x

main = do
  f <- BS.getContents
  let ls = BS.lines f
      ps = filter (not . isSpace . BS.head) $ filter (not . BS.null) $ ls
      contrs = M.fromListWith (+) $ flip zip (repeat 1) $ map (trim . name . unquote . BS.dropWhile isSpace . BS.drop (length tx)) $ ps
  -- print (length ps)
  -- mapM print $ sortBy (comparing fst) $ M.toList $ contrs
  mapM BS.putStrLn $ M.keys contrs
  return ()

