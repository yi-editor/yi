-- Test the fast buffer implementation

module Buffer.Buffer0 where

import Yi.Buffer
import Yi.FastBuffer
import Data.Unique

import Data.List

import System.Directory
import System.IO.Unsafe
import qualified Control.Exception
import Control.Concurrent

import TestFramework

-- really test it!
contents = unsafePerformIO $  do
        s <- readFile "../README"
        forkIO (Control.Exception.evaluate (length s) >> return ())
        return s

$(tests "fastBuffer" [d| 

 testElems = unsafePerformIO $ do
        b <- newB "testbuffer" contents :: IO FBuffer
        s <- elemsB b
        return $ assertEqual s contents

 testName = unsafePerformIO $ do
        b <- newB "testbuffer" contents :: IO FBuffer
        return $ assertEqual (nameB b) "testbuffer"

 testSize = unsafePerformIO $ do
        b <- newB "testbuffer" contents :: IO FBuffer
        i <- sizeB b
        return $ assertEqual i (length contents)

 testWriteFile = unsafePerformIO $ do
        b <- newB "testbuffer" contents :: IO FBuffer
        let f = "testWriteFile.file"
        hPutB b f
        s <- readFile f
        removeFile f
        return $ assertEqual s contents

 testGetFileNameB = unsafePerformIO $ do
        b <- hNewB "../README" :: IO FBuffer
        m <- getfileB b
        return $ assertEqual m (Just "../README")

 testSetFileNameB = unsafePerformIO $ do
        b <- newB "testbuffer" contents :: IO FBuffer
        setfileB b "../README"
        m <- getfileB b
        return $ assertEqual m (Just "../README")

 testLotsOfBuffers = unsafePerformIO $ do
        bs <- sequence [ newB (show x) contents :: IO FBuffer
                       | x <- [1..100] ]   -- create a 1000 buffers
        
        bs' <- mapM elemsB bs

        return $ do assertEqual (length . nub . sort $ map keyB bs) (length bs)
                    assert $ (length . nub . sort $ bs')  == 1

 testMoveTo = unsafePerformIO $ do
        b  <- newB "testbuffer" contents :: IO FBuffer
        ps <- sequence [ moveTo b i >> pointB b >>= \j -> return (i,j) 
                       | i <- [0 .. 4000] ]
        return $ let (l1,l2) = unzip ps in assertEqual l1 l2

 |])

instance Show Unique where
    showsPrec _ u = showString "<unique>"
    
