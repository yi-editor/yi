
--  $Id: GenUtil.hs,v 1.27 2004/07/12 23:31:34 john Exp $
-- arch-tag: 835e46b7-8ffd-40a0-aaf9-326b7e347760


-- Copyright (c) 2002 John Meacham (john@foo.net)
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

----------------------------------------
-- | This is a collection of random useful utility functions written in pure
-- Haskell 98. In general, it trys to conform to the naming scheme put forth
-- the haskell prelude and fill in the obvious omissions, as well as provide
-- useful routines in general. To ensure maximum portability, no instances are
-- exported so it may be added to any project without conflicts.
----------------------------------------

module HEmacs.GenUtil(
    -- * Functions
    -- ** Error reporting 
    putErr,putErrLn,putErrDie,
    -- ** Simple deconstruction 
    fromLeft,fromRight,fsts,snds,splitEither,rights,lefts,
    -- ** System routines
    exitSuccess, System.exitFailure, epoch, lookupEnv,endOfTime,
    -- ** Random routines
    repMaybe,
    liftT2, liftT3, liftT4, 
    snub, snubFst, sortFst, groupFst, foldl',
    fmapLeft,fmapRight,isDisjoint,isConjoint,
    -- ** Monad routines
    repeatM, repeatM_, replicateM, replicateM_, maybeToMonad,
    toMonadM, ioM, ioMp, foldlM, foldlM_, foldl1M, foldl1M_,
    -- ** Text Routines
    -- *** Quoting
    shellQuote, simpleQuote, simpleUnquote, 
    -- *** Random
    concatInter,
    powerSet,
    indentLines,
    buildTableLL,
    buildTableRL,
    randomPermute,
    randomPermuteIO,
    trimBlankLines,
    paragraph,
    paragraphBreak,
    expandTabs,
    chunk,
    chunkText,
    rtup,
    triple,
    fromEither,
    mapFst,
    mapSnd,
    mapFsts,
    mapSnds,
    tr,
    readHex,
    overlaps,
    showDuration,
    getArgContents,
    readM,
    readsM,

    -- * Classes
    UniqueProducer(..)
    ) where

import Data.Char            (isAlphaNum, isSpace, toLower,  ord)
import Data.List            (group, sort, intersperse, sortBy, groupBy)
import Control.Monad hiding ( replicateM_, replicateM )
import qualified IO
import qualified System
import Random(StdGen, newStdGen, randomR)
import Time

{-# SPECIALIZE snub :: [String] -> [String] #-}
{-# SPECIALIZE snub :: [Int] -> [Int] #-}

-- | sorted nub of list, much more efficient than nub, but doesnt preserve ordering.
snub :: Ord a => [a] -> [a]
snub = map head . group . sort

-- | sorted nub of list of tuples, based solely on the first element of each tuple.
snubFst :: Ord a => [(a,b)] -> [(a,b)]
snubFst = map head . groupBy (\(x,_) (y,_) -> x == y) . sortBy (\(x,_) (y,_) -> compare x y)

-- | sort list of tuples, based on first element of each tuple.
sortFst :: Ord a => [(a,b)] -> [(a,b)]
sortFst = sortBy (\(x,_) (y,_) -> compare x y)

-- | group list of tuples, based only on equality of the first element of each tuple.
groupFst :: Eq a => [(a,b)] -> [[(a,b)]]
groupFst = groupBy (\(x,_) (y,_) -> x == y)

-- | write string to standard error
putErr :: String -> IO ()
putErr = IO.hPutStr IO.stderr

-- | write string and newline to standard error
putErrLn :: String -> IO ()
putErrLn s = putErr (s ++ "\n")


-- | write string and newline to standard error, 
-- then exit program with failure.
putErrDie :: String -> IO a
putErrDie s = putErrLn s >> System.exitFailure


-- | exit program successfully. 'exitFailure' is 
-- also exported from System.
exitSuccess :: IO a
exitSuccess = System.exitWith System.ExitSuccess


{-# INLINE fromRight #-}
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight"

{-# INLINE fromLeft #-}
fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _ = error "fromLeft"

-- | recursivly apply function to value until it returns Nothing
repMaybe :: (a -> Maybe a) -> a -> a
repMaybe f e = case f e of 
    Just e' -> repMaybe f e'
    Nothing -> e

{-# INLINE liftT2 #-}
{-# INLINE liftT3 #-}
{-# INLINE liftT4 #-}

liftT4 (f1,f2,f3,f4) (v1,v2,v3,v4) = (f1 v1, f2 v2, f3 v3, f4 v4)
liftT3 (f,g,h) (x,y,z) = (f x, g y, h z)
-- | apply functions to values inside a tupele. 'liftT3' and 'liftT4' also exist.
liftT2 :: (a -> b, c -> d) -> (a,c) -> (b,d)
liftT2 (f,g) (x,y) = (f x, g y)


-- | class for monads which can generate
-- unique values.
class Monad m => UniqueProducer m where
    -- | produce a new unique value
    newUniq :: m Int

--    peekUniq :: m Int
--    modifyUniq :: (Int -> Int) -> m ()
--    newUniq = do
--	v <- peekUniq
--	modifyUniq (+1) 
--	return v

rtup a b = (b,a)
triple a b c = (a,b,c)

-- | the standard unix epoch
epoch :: ClockTime
epoch = toClockTime $ CalendarTime { ctYear = 1970, ctMonth = January, ctDay = 0, ctHour = 0, ctMin = 0, ctSec = 0, ctTZ = 0, ctPicosec = 0, ctWDay = undefined, ctYDay = undefined, ctTZName = undefined, ctIsDST = undefined}

-- | an arbitrary time in the future
endOfTime :: ClockTime
endOfTime = toClockTime $ CalendarTime { ctYear = 2020, ctMonth = January, ctDay = 0, ctHour = 0, ctMin = 0, ctSec = 0, ctTZ = 0, ctPicosec = 0, ctWDay = undefined, ctYDay = undefined, ctTZName = undefined, ctIsDST = undefined}

{-# INLINE fsts #-}
-- | take the fst of every element of a list
fsts :: [(a,b)] -> [a]
fsts = map fst

{-# INLINE snds #-}
-- | take the snd of every element of a list
snds :: [(a,b)] -> [b]
snds = map snd

{-# INLINE repeatM #-}
{-# SPECIALIZE repeatM :: IO a -> IO [a] #-}
repeatM :: Monad m => m a -> m [a]
repeatM x = sequence $ repeat x

{-# INLINE repeatM_ #-}
{-# SPECIALIZE repeatM_ :: IO a -> IO () #-}
repeatM_ :: Monad m => m a -> m ()
repeatM_ x = sequence_ $ repeat x

{-# INLINE replicateM #-}
{-# SPECIALIZE replicateM :: Int -> IO a -> IO [a] #-}
replicateM :: Monad m => Int -> m a -> m [a]
replicateM n x = sequence $ replicate n x

{-# INLINE replicateM_ #-}
{-# SPECIALIZE replicateM_ :: Int -> IO a -> IO () #-}
replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ n x = sequence_ $ replicate n x

{-# SPECIALIZE maybeToMonad :: Maybe a -> IO a #-}
-- | convert a maybe to an arbitrary failable monad
maybeToMonad :: Monad m => Maybe a -> m a
maybeToMonad (Just x) = return x
maybeToMonad Nothing = fail "Nothing"

toMonadM :: Monad m => m (Maybe a) -> m a
toMonadM action = join $ liftM maybeToMonad action

foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM f v (x:xs) = (f v x) >>= \a -> foldlM f a xs
foldlM _ v [] = return v

foldl1M :: Monad m => (a -> a -> m a) ->  [a] -> m a
foldl1M f (x:xs) = foldlM f x xs
foldl1M _ _ = error "foldl1M"


foldlM_ :: Monad m => (a -> b -> m a) -> a -> [b] -> m ()
foldlM_ f v xs = foldlM f v xs >> return ()
     
foldl1M_ ::Monad m => (a -> a -> m a)  -> [a] -> m ()
foldl1M_ f xs = foldl1M f xs >> return ()

-- | partition a list of eithers.
splitEither :: [Either a b] -> ([a],[b])
splitEither  (r:rs) = case splitEither rs of 
    (xs,ys) -> case r of 
        Left x -> (x:xs,ys)
        Right y -> (xs,y:ys)
splitEither          [] = ([],[])

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

{-# INLINE mapFst #-}
{-# INLINE mapSnd #-}
mapFst  f   (x,y) = (f x,  y)
mapSnd    g (x,y) = (  x,g y)

{-# INLINE mapFsts #-}
{-# INLINE mapSnds #-}
mapFsts f xs = [(f x, y) | (x,y) <- xs] 
mapSnds g xs = [(x, g y) | (x,y) <- xs]

{-# INLINE rights #-}
-- | take just the rights
rights :: [Either a b] -> [b]
rights xs = [x | Right x <- xs]

{-# INLINE lefts #-}
-- | take just the lefts
lefts :: [Either a b] -> [a]
lefts xs = [x | Left x <- xs]

ioM :: Monad m => IO a -> IO (m a)
ioM action = catch (fmap return action) (\e -> return (fail (show e)))

ioMp :: MonadPlus m => IO a -> IO (m a)
ioMp action = catch (fmap return action) (\_ -> return mzero)

-- | reformat a string to not be wider than a given width, breaking it up
-- between words.

paragraph :: Int -> String -> String
paragraph maxn xs = drop 1 (f maxn (words xs)) where
    f n (x:xs) | lx < n = (' ':x) ++ f (n - lx) xs where
        lx = length x + 1    
    f _ (x:xs) = '\n': (x ++ f (maxn - length x) xs)
    f _ [] = "\n"

chunk :: Int -> [a] -> [[a]]
chunk mw s | length s < mw = [s]
chunk mw s = case splitAt mw s of (a,b) -> a : chunk mw b

chunkText :: Int -> String -> String 
chunkText mw s = concatMap (unlines . chunk mw) $ lines s

{-
paragraphBreak :: Int -> String -> String 
paragraphBreak  maxn xs = unlines (map ( unlines . map (unlines . chunk maxn) . lines . f maxn ) $ lines xs) where
    f _ "" = ""
    f n xs | length ss > 0 = if length ss + r rs > n then '\n':f maxn rs else ss where
        (ss,rs) = span isSpace xs  
    f n xs = ns ++ f (n - length ns) rs where
        (ns,rs) = span (not . isSpace) xs  
    r xs = length $ fst $ span (not . isSpace) xs
-}

paragraphBreak :: Int -> String -> String 
paragraphBreak  maxn xs = unlines $ (map f) $ lines xs where
    f s | length s <= maxn = s
    f s | isSpace (head b) = a ++ "\n" ++ f (dropWhile isSpace b)
        | all (not . isSpace) a = a ++ "\n" ++ f b
        | otherwise  = reverse (dropWhile isSpace sa) ++ "\n" ++ f (reverse ea ++ b) where
            (ea, sa) = span (not . isSpace) $ reverse a 
            (a,b) = splitAt maxn s

expandTabs' :: Int -> Int -> String -> String
expandTabs' 0 _ s = filter (/= '\t') s
expandTabs' sz off ('\t':s) = replicate len ' ' ++ expandTabs' sz (off + len) s where
    len = (sz - (off `mod` sz))
expandTabs' sz _ ('\n':s) = '\n': expandTabs' sz 0 s
expandTabs' sz off (c:cs) = c: expandTabs' sz (off + 1) cs
expandTabs' _ _ "" = ""


-- | expand tabs into spaces in a string
expandTabs s = expandTabs' 8 0 s



tr :: String -> String -> String -> String
tr as bs s = map (f as bs) s where
    f (a:_) (b:_) c | a == c = b
    f (_:as) (_:bs) c = f as bs c
    f [] [] c = c
    f _ _ _ = error "invalid tr"

 
-- | quote strings 'rc' style. single quotes protect any characters between
-- them, to get an actual single quote double it up. Inverse of 'simpleUnquote'
simpleQuote :: [String] -> String
simpleQuote ss = unwords (map f ss) where
    f s | any isBad s = "'" ++ dquote s ++ "'"
    f s = s
    dquote s = concatMap (\c -> if c == '\'' then "''" else [c]) s
    isBad c = isSpace c || c == '\''

-- | inverse of 'simpleQuote'
simpleUnquote :: String -> [String]
simpleUnquote s = f (dropWhile isSpace s)  where
    f [] = []
    f ('\'':xs) = case quote' "" xs of (x,y) ->  x:f (dropWhile isSpace y)
    f xs = case span (not . isSpace) xs of (x,y) ->  x:f (dropWhile isSpace y)
    quote' a ('\'':'\'':xs) = quote' ('\'':a) xs
    quote' a ('\'':xs) = (reverse a, xs)
    quote' a (x:xs) = quote' (x:a) xs
    quote' a [] = (reverse a, "")

-- | quote a set of strings as would be appropriate to pass them as 
-- arguments to a 'sh' style shell
shellQuote :: [String] -> String
shellQuote ss = unwords (map f ss) where
    f s | any (not . isGood) s = "'" ++ dquote s ++ "'"
    f s = s
    dquote s = concatMap (\c -> if c == '\'' then "'\\''" else [c]) s
    isGood c = isAlphaNum c || c `elem` "@/."


-- | looks up an enviornment variable and returns it in a 'MonadPlus' rather
-- than raising an exception if the variable is not set.
lookupEnv :: MonadPlus m => String -> IO (m String)
lookupEnv s = catch (fmap return $ System.getEnv s) (\e -> if IO.isDoesNotExistError e then return mzero else ioError e)

{-# SPECIALIZE fmapLeft :: (a -> c) -> [(Either a b)] -> [(Either c b)] #-}
fmapLeft :: Functor f => (a -> c) -> f (Either a b) -> f (Either c b)
fmapLeft fn = fmap f where
    f (Left x) = Left (fn x)
    f (Right x)  = Right x

{-# SPECIALIZE fmapRight :: (b -> c) -> [(Either a b)] -> [(Either a c)] #-}
fmapRight :: Functor f => (b -> c) -> f (Either a b) -> f (Either a c)
fmapRight fn = fmap f where
    f (Left x) = Left x
    f (Right x)  = Right (fn x)

{-# SPECIALIZE isDisjoint :: [String] -> [String] -> Bool #-}
{-# SPECIALIZE isConjoint :: [String] -> [String] -> Bool #-}
{-# SPECIALIZE isDisjoint :: [Int] -> [Int] -> Bool #-}
{-# SPECIALIZE isConjoint :: [Int] -> [Int] -> Bool #-}
-- | set operations on lists. (slow!)
isDisjoint, isConjoint :: Eq a => [a] -> [a] -> Bool
isConjoint xs ys = or [x == y | x <- xs, y <- ys] 
isDisjoint xs ys = not (isConjoint xs ys)

-- | 'concat' composed with 'List.intersperse'. 
concatInter :: String -> [String] -> String
concatInter x = concat . (intersperse x)

-- | place spaces before each line in string.
indentLines :: Int -> String -> String 
indentLines n s = unlines $ map (replicate n ' ' ++)$ lines s 

-- | trim blank lines at beginning and end of string
trimBlankLines :: String -> String
trimBlankLines cs = unlines $ reverse (tb $ reverse (tb (lines cs))) where
    tb = dropWhile (all isSpace)

buildTableRL :: [(String,String)] -> [String]
buildTableRL ps = map f ps where
    f (x,"") = x
    f (x,y) = replicate (bs - length x) ' ' ++ x ++ replicate 4 ' ' ++ y
    bs = maximum (map (length . fst) [ p | p@(_,_:_) <- ps ])

buildTableLL :: [(String,String)] -> [String]
buildTableLL ps = map f ps where
    f (x,y) = x ++ replicate (bs - length x) ' ' ++ replicate 4 ' ' ++ y
    bs = maximum (map (length . fst) ps)

{-# INLINE foldl' #-}
-- | strict version of 'foldl'
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ a []     = a
foldl' f a (x:xs) = (foldl' f $! f a x) xs


-- | randomly permute a list, using the standard random number generator.
randomPermuteIO :: [a] -> IO [a]
randomPermuteIO xs = newStdGen >>= \g -> return (randomPermute g xs)

-- | randomly permute a list given a RNG
randomPermute :: StdGen -> [a] -> [a]
randomPermute _   []  = []
randomPermute gen xs  = (head tl) : randomPermute gen' (hd ++ tail tl)
   where (idx, gen') = randomR (0,length xs - 1) gen
         (hd,  tl)   = splitAt idx xs


-- | compute the power set of a list

powerSet       :: [a] -> [[a]]
powerSet []     = [[]]
powerSet (x:xs) = xss /\/ map (x:) xss
                where xss = powerSet xs

-- | interleave two lists lazily, alternating elements from them. This can be used instead of concatination to avoid space leaks in certain situations.
(/\/)        :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)        



readHexChar a | a >= '0' && a <= '9' = return $ ord a - ord '0'
readHexChar a | z >= 'a' && z <= 'f' = return $ 10 + ord z - ord 'a' where z = toLower a
readHexChar x = fail $ "not hex char: " ++ [x]

readHex :: Monad m => String -> m Int
readHex [] = fail "empty string"
readHex cs = mapM readHexChar cs >>= \cs' -> return (rh $ reverse cs') where
    rh (c:cs) =  c + 16 * (rh cs)
    rh [] =  0


{-# SPECIALIZE overlaps :: (Int,Int) -> (Int,Int) -> Bool #-}

-- | determine if two closed intervals overlap at all. 

overlaps :: Ord a => (a,a) -> (a,a) -> Bool
(a,_) `overlaps` (_,y) | y < a = False
(_,b) `overlaps` (x,_) | b < x = False
_ `overlaps` _ = True

-- | translate a number of seconds to a string representing the duration expressed.
showDuration :: Integral a => a -> String
showDuration x = st "d" dayI ++ st "h" hourI ++ st "m" minI ++ show secI ++ "s" where
        (dayI, hourI) = divMod hourI' 24
        (hourI', minI) = divMod minI' 60
        (minI',secI) = divMod x 60
        st _ 0 = "" 
        st c n = show n ++ c

-- | behave like while(<>) in perl, go through the argument list, reading the
-- concation of each file name mentioned or stdin if '-' is on it. If no
-- arguments are given, read stdin. 

getArgContents = do
    as <- System.getArgs
    let f "-" = getContents
        f fn = readFile fn
    cs <- mapM f as
    if null as then getContents else return $ concat cs
    

readM :: (Monad m, Read a) => String -> m a
readM cs = case [x | (x,t) <-  reads cs, ("","") <- lex t] of
    [x] -> return x
    [] -> fail "readM: no parse"
    _ -> fail "readM: ambiguous parse"

readsM :: (Monad m, Read a) => String -> m (a,String)
readsM cs = case readsPrec 0 cs of
    [(x,s)] -> return (x,s)
    _ -> fail "cannot readsM"

