{-# OPTIONS_GHC -Wall #-}

-- Copyright (C) 2006 Benedikt Schmidt
-- see LICENSE.BSD3 for license

module Shim.Utils
  (
    processGetContents
  , splitBy
  , unSplit
  , splitElem
  , chomp
  , commonPrefix
  , dropPrefix
  , dropSuffix
  , fst3
  , snd3
  , setLogAction
  , setLogfile
  , getLogfile
  , equating
  , recurseDir
  , netEncode
  , netDecode
  , getNetstring
  , revDrop
  , safeHead
  , logS
  , whenM
  , unlessM
  , shorten
  , uncurry3
  ) where
  
import System.IO
import System.Process
import Control.Monad
import Text.Printf
import Data.Maybe
import System.FilePath ( takeDirectory )
-- import qualified Control.OldException as CE
import System.IO.Unsafe ( unsafePerformIO )
import Control.Concurrent.MVar
import Yi.Debug
import Prelude hiding (error)

processGetContents :: FilePath -> [String] -> IO String
processGetContents cmd args = do
  (_,out,_,pid) <- runInteractiveProcess cmd args Nothing Nothing
  s <- hGetContents out
  waitForProcess pid
  return s

recurseDir :: (Monad m) => (FilePath -> m (Maybe a)) -> FilePath -> m (Maybe a)
recurseDir f d 
  | d == "" = return Nothing
  | d `elem` ["/", "."] || takeDirectory d == d = f d
  | otherwise = do res <- f d
                   if isJust res
                    then return res
                    else recurseDir f $ takeDirectory d

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

safeHead :: a -> [a] -> a
safeHead a l = case l of
                 x:_ -> x
                 [] -> a

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p xs = case break p xs of
                 (l,tok:r) -> let (x:xx) = splitBy p r 
                             in [l] ++ (tok : x) : xx
                 (l, []) -> [l]

splitElem :: Eq a => a -> [a] -> [[a]]
splitElem c = splitBy (==c)

unSplit :: a -> [[a]] -> [a]
unSplit _ [] = []
unSplit _ (x:[]) = x
unSplit c (x:xs) = x ++ [c] ++ (unSplit c xs)

revDrop :: (a -> Bool) -> [a] -> [a]
revDrop p = reverse . dropWhile p . reverse

equating :: (Eq b) => (a -> b) -> a -> a -> Bool
equating p x y = p x == p y

chomp :: String -> String
chomp = revDrop (\ch -> ch == '\n' || ch == '\r')

dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix pre xs = (map snd . dropWhile fst) l2
  where l1 = zipWith (==) pre xs ++ (repeat False)
        l2 = zipWith (,) l1 xs

dropSuffix :: (Eq a) => [a] -> [a] -> [a]
dropSuffix suf = reverse . dropPrefix (reverse suf) . reverse

commonPrefix :: [String] -> String
commonPrefix [] = ""
commonPrefix (x:xs) = foldr (\a b -> map fst . takeWhile (uncurry (==))
                                       $ zip a b) x xs 

netEncode :: String -> String
netEncode s =
  hexEncode (length s) ++ s

netDecode :: String -> String
netDecode s = text
  where (lens, ':':xs) = break (==':') s
        (text, ";") = splitAt (read lens) xs

getNetstring :: Handle -> IO String
getNetstring h = do
  c <- skipWhite h -- better for testing (line buffered terminal)
  lens <- readCharN h 5
  text <- readCharN h (hexDecode (c:lens))
  return text

skipWhite :: Handle -> IO Char
skipWhite h = do
  c <- hGetChar h
  if (elem c "\n \t")
    then skipWhite h
    else return c

readCharN :: Handle -> Int -> IO String
readCharN h n = replicateM n (hGetChar h)

hexEncode :: Int -> String
hexEncode = printf "%06x"

hexDecode :: (Num a) => String -> a
hexDecode [] = 0
hexDecode (x:xs) = (h x)* 16^(length xs) + hexDecode xs
 where h '0' = 0;  h '1' = 1;  h '2' = 2;  h '3' = 3
       h '4' = 4;  h '5' = 5;  h '6' = 6;  h '7' = 7
       h '8' = 8;  h '9' = 9;  h 'a' = 10; h 'b' = 11
       h 'c' = 12; h 'd' = 13; h 'e' = 14; h 'f' = 15
       h _ = error "invalid hex-number"

logfile :: MVar FilePath
logfile = unsafePerformIO $ newMVar ""

logAction :: MVar (FilePath -> String -> IO ())
logAction = unsafePerformIO $ newMVar $ const $ const $ return ()

setLogAction ::  (FilePath -> String -> IO ()) -> IO ()
setLogAction = modifyMVar_ logAction . const . return

setLogfile :: FilePath -> IO ()
setLogfile = modifyMVar_ logfile . const . return 

getLogfile :: IO FilePath
getLogfile = readMVar logfile

logS :: String -> IO ()
logS = logPutStrLn 

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond m = do
  res <- cond
  when (res) m

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM cond = whenM (liftM not cond)

-- excToMaybe :: (NFData a) => a -> Maybe a
-- excToMaybe x = unsafePerformIO $
--   CE.catch (rnf x `seq` return $ Just x) (const $ return $ Nothing)

shorten :: Int -> String -> String
shorten n s = if length s > n then take (n-4) s ++ "..." else s
-- some tests, use quickcheck
--tester f f_inv x = if (f . f_inv) x == x then Nothing else Just (f . f_inv $ x)
--test1 = tester (unSplit sep) (splitElem sep)
--  where sep = ','
--test2 = tester hexDecode hexEncode
  
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 fn (a, b, c) = fn a b c
