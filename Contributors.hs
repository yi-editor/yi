{-# LANGUAGE OverloadedStrings, PatternSignatures, PatternGuards  #-}
import Data.Array (elems, Array)
import Data.ByteString.Char8 (pack, ByteString)
import Data.Char
import Data.Function
import Data.String
import Data.List 
import Prelude hiding (lines, readFile)
import Text.Regex.Base
import Text.Regex.TDFA
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

capitalize s = BS.cons (toUpper $ BS.head s) (BS.tail s)

tx :: String = "Thu Oct 14 05:40:06 CEST 2004 "

unquote x
    | BS.head x == '\'' && BS.last x == '\'' = unquote (BS.init $ BS.tail $ x)
    | otherwise = x

mkName firstname lastname = BS.concat . intersperse " " . map capitalize $ [firstname, lastname]

name :: ByteString -> ByteString
name tag
     | tag == "tora@zonetora.co.uk" = "Tristan Allwood"
     | tag == "andy@nobugs.org" = "Andrew Birkett"
     | tag == "jeff@nokrev.com" = "Jeff Wheeler"
     | AllTextSubmatches [_,name] <- tag =~ pack "^\"?(.+)<.*>\"?$" = name
     | AllTextSubmatches [_,name] <- tag =~ pack "^?(.+)<.*>?$" = name
     | AllTextSubmatches [_,firstname,lastname] <- tag =~ pack "^<?(.*)@(.*)\\.name>?$"
                                                = mkName firstname lastname
     | AllTextSubmatches [_,user] <- tag =~ pack "^<?(.*)@.*>?$" = nickToName user
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
            "gwern0"                -> "Gwern Branwen"
            "jeanphilippe.bernardy" -> "Jean-Philippe Bernardy"
            "kr.angelov"            -> "Krasimir Angelov"
            "m.gubinelli"           -> "Massimiliano Gubinelli"
            "mlang"                 -> "Mario Lang"
            "mwotton"               -> "Mark Wotton"
            "newsham"               -> "Tim Newsham"
            "nominolo"              -> "Thomas Schilling"
            "scottw"                -> "Scott Williams"
            "shae"                  -> "Shae Erisson"
            "sjw"                   -> "Simon Winwood"
            "tactics40"             -> "Michael Maloney"
            "vintermann"            -> "Harald Korneliussen"
            "vivian.mcphail"        -> "Vivian McPhail"
            "zapf"                  -> "Bastiaan Zapf"
            _ | AllTextSubmatches [_,firstname,lastname] <- x =~ pack "^(.*)\\.(.*)$"
                                                         -> mkName firstname lastname
              | otherwise -> x

main = do
  f <- BS.getContents
  let ls = BS.lines f
      ps = filter (not . isSpace . BS.head) $ filter (not . BS.null) $ ls
      contrs = M.fromListWith (+) $ flip zip (repeat 1) $ map (trim . name . unquote . BS.dropWhile isSpace . BS.drop (length tx)) $ ps
  putStrLn $ show (length ps) ++ " patches"
  putStrLn $ show (M.size contrs) ++ " contributors"
  mapM print $ sortBy (compare `on` snd) $ M.toList $ contrs
  mapM BS.putStrLn $ M.keys contrs
  return ()

