{-# OPTIONS -fffi #-}
-- 
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
-- 
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
-- 
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
-- USA
-- 

--
-- A Haskell reimplementation of the C mktemp/mkstemp/mkstemps library
-- based on the algorithms in:
--      "$ OpenBSD: mktemp.c,v 1.17 2003/06/02 20:18:37 millert Exp $"
-- which are available under the BSD license.
--

module HEmacs.MkTemp ( 

     mktemp,    -- :: FilePath -> Maybe FilePath
     mkstemp,   -- :: FilePath -> Maybe (FilePath, Handle)
     mkstemps,  -- :: FilePath -> Int -> Maybe (FilePath,Handle)
     mkdtemp,   -- :: FilePath -> Maybe FilePath

  ) where

import Data.List
import Data.Char

import Control.Monad            ( liftM )
import Control.Exception        ( handleJust )

import System.IO                ( isAlreadyExistsError, Handle )
import System.Directory         ( doesDirectoryExist, doesFileExist )

import GHC.IOBase               ( Exception(IOException) )

#ifndef __MINGW32__
import System.Posix.IO
import System.Posix.Files
import qualified System.Posix.Directory ( createDirectory )
import qualified System.Posix.Internals ( c_getpid )
#endif

#ifndef HAVE_ARC4RANDOM
import System.Random            ( getStdRandom, Random(randomR) )
#else
import GHC.Base
import GHC.Int
#endif

-- ---------------------------------------------------------------------

mkstemps :: FilePath -> Int -> IO (Maybe (FilePath,Handle))
mkstemp  :: FilePath        -> IO (Maybe (FilePath,Handle))
mktemp   :: FilePath        -> IO (Maybe FilePath)
mkdtemp  :: FilePath        -> IO (Maybe FilePath)

mkstemps path slen = gettemp path True False slen

mkstemp  path      = gettemp path True False 0

mktemp  path = do v <- gettemp path False False 0
                  return $ case v of Just (path',_) -> Just path'; _ -> Nothing

mkdtemp path = do v <- gettemp path False True 0
                  return $ case v of Just (path',_) -> Just path'; _ -> Nothing

-- ---------------------------------------------------------------------

gettemp :: FilePath -> Bool -> Bool -> Int -> IO (Maybe (FilePath, Handle))

gettemp [] _ _ _      = return Nothing
gettemp _ True True _ = return Nothing

gettemp path doopen domkdir slen = do
    --
    -- firstly, break up the path and extract the template
    --
    let (pref,tmpl,suff) = let (r,s) = splitAt (length path - slen) path
                               (p,t) = break (== 'X') r
                           in (p,t,s)
    --
    -- an error if there is only a suffix, it seems
    --
    if null pref && null tmpl then return Nothing else do {
    --
    -- replace end of template with process id, and rest with randomness
    --
    ;pid <- liftM show $ getProcessID
    ;let (rest, xs) = merge tmpl pid
    ;as <- randomise rest
    ;let tmpl' = as ++ xs
         path' = pref ++ tmpl' ++ suff
    --
    -- just check if we can get at the directory we might need
    --
    ;dir_ok <- if doopen || domkdir
               then let d = reverse $ dropWhile (/= '/') $ reverse path'
                    in doesDirectoryExist d
               else return True

    ;if not dir_ok then return Nothing else do {
    --
    -- We need a function for looking for appropriate temp files
    --
    ;let fn p
          | doopen    = handleJust isInUse (\_ -> return Nothing) $
                          do h <- open0600 p ; return $ Just h
          | domkdir   = handleJust alreadyExists (\_ -> return Nothing) $
                          do mkdir0700 p ; return $ Just undefined
          | otherwise = do b <- doesFileExist p
                           return $ if b then Nothing else Just undefined

    --
    -- now, try to create the tmp file, permute if we can't
    -- once we've tried all permutations, give up
    --
    ;let tryIt p t i =
            do v <- fn p
               case v of Just h  -> return $ Just (p,h)        -- it worked
                         Nothing -> let (i',t') = tweak i t
                                    in if null t' 
                                       then return Nothing     -- no more
                                       else tryIt (pref++t'++suff) t' i'
    ;tryIt path' tmpl' 0

    }}

--
-- Replace X's with pid digits. Complete rewrite
--
merge :: String -> String -> (String,String)
merge t []          = (t  ,[])
merge [] _          = ([] ,[])
merge (_:ts) (p:ps) = (ts',p:ps')
        where (ts',ps') = merge ts ps

--
-- And replace remaining X's with random chars
-- randomR is pretty slow, oh well.
--
randomise :: String -> IO String
randomise []       = return []
randomise ('X':xs) = do p <- getRandom ()
                        let c = chr $! if p < 26 
                                       then p + (ord 'A') 
                                       else (p - 26) + (ord 'a')
                        xs' <- randomise xs
                        return (c : xs')
randomise s = return s

--
-- "tricky little algorithm for backward compatibility"
-- could do with a Haskellish rewrite
--
tweak :: Int -> String -> (Int,String)
tweak i s 
    | i > length s - 1 = (i,[])                 -- no more
    | s !! i == 'Z'    = if i == length s - 1 
                         then (i,[])            -- no more
                         else let s' = splice (i+1) 'a'
                              in tweak (i+1) s' -- loop
    | otherwise = let c = s !! i in case () of {_
        | isDigit c -> (i, splice i 'a' )
        | c == 'z'  -> (i, splice i 'A' )
        | otherwise -> let c' = chr $ (ord c) + 1 in (i,splice i c')
    }
    where
        splice j c = let (a,b) = splitAt j s in a ++ [c] ++ tail b

-- ---------------------------------------------------------------------

alreadyExists e@(IOException ioe) 
        | isAlreadyExistsError ioe = Just e
        | otherwise                = Nothing
alreadyExists _ = Nothing

#ifndef __MINGW32__
isInUse (IOException ioe) 
        | isAlreadyExistsError ioe = Just ()
        | otherwise                = Nothing
isInUse _ = Nothing
#else
isInUse (IOException ioe) 
        | isAlreadyInUseError  ioe = Just ()
        | isPermissionError    ioe = Just ()
        | isAlreadyExistsError ioe = Just ()    -- we throw this
        | otherwise               = Nothing
isInUse _ = Nothing
#endif

-- ---------------------------------------------------------------------
-- Create a file mode 0600 if possible
--
open0600 :: FilePath -> IO Handle

#ifndef __MINGW32__

-- open(path, O_CREAT|O_EXCL|O_RDWR, 0600)

open0600 f = do
        openFd f ReadWrite (Just o600) excl >>= fdToHandle
   where 
        o600 = ownerReadMode `unionFileModes` ownerWriteMode
        excl = defaultFileFlags { exclusive = True }
#else

-- N.B. race condition between testing existence and opening

open0600 f = do
        b <- doesFileExist f
        if b then ioException err   -- race
             else openFile f ReadWriteMode
    where
        err = IOError Nothing AlreadyExists "open0600" "already exists" Nothing
#endif

--
-- create a directory mode 0700 if possible
--
mkdir0700 :: FilePath -> IO ()
mkdir0700 dir =
#ifndef __MINGW32__
        System.Posix.Directory.createDirectory dir ownerModes
#else
        createDirectory dir
#endif

-- ---------------------------------------------------------------------
-- | getProcessId, stolen from GHC

#ifdef __MINGW32__
foreign import ccall unsafe "_getpid" getProcessID :: IO Int
#elif __GLASGOW_HASKELL__ > 504
getProcessID :: IO Int
getProcessID = System.Posix.Internals.c_getpid >>= return . fromIntegral
#else
getProcessID :: IO Int
getProcessID = Posix.getProcessID
#endif

-- ---------------------------------------------------------------------
-- | Use a variety of random functions, if you like.
--
getRandom :: () -> IO Int

#ifndef HAVE_ARC4RANDOM
getRandom _ = getStdRandom (randomR (0,51))
#else
--
--
-- OpenBSD: "The arc4random() function provides a high quality 32-bit
-- pseudo-random number very quickly.  arc4random() seeds itself on a
-- regular basis from the kernel strong random number subsystem
-- described in random(4)." Also, it is a bit faster than getStdRandom
--
getRandom _ = do 
        (I32# i) <- c_arc4random
        return (I# (word2Int# 
             ((int2Word# i `and#` int2Word# 0xffff#) `remWord#` int2Word# 52#)))

foreign import ccall unsafe "arc4random" c_arc4random :: IO Int32
#endif
