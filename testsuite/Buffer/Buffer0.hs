-- Test the fast buffer implementation

module Buffer.Buffer0 where

import Yi.Buffer
import Yi.FastBuffer

import Data.Unique
import Data.Char
import Data.List

import System.Directory
import System.IO.Unsafe

import Control.Monad
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

 testMovement = unsafePerformIO $ do
        b  <- newB "testbuffer" contents :: IO FBuffer
        moveTo b 1000
        leftB b
        rightB b
        i <- pointB b   -- should be 1000
        rightB b
        leftB b
        j <- pointB b
        rightN b 1000
        k <- pointB b
        leftN b 2000
        l <- pointB b
        rightN b 10000  -- moving past end of buffer should only go to end
        m <- pointB b
        s <- sizeB b 
        leftN b 10000
        n <- pointB b
        return $ do assertEqual i 1000
                    assertEqual j i
                    assertEqual k (1000+1000)
                    assertEqual l 0
                    assertEqual m (s-1)
                    assertEqual n 0
                    
 testRead = unsafePerformIO $ do
        b  <- newB "testbuffer" contents :: IO FBuffer
        c  <- readAtB b 1000
        moveTo b 1000
        c' <- readB b
        writeB b 'X'
        c'' <- readB b
        return $ do assertEqual c c'
                    assertEqual c'' 'X'

 testInsert = unsafePerformIO $ do
        b <- newB "testbuffer" contents :: IO FBuffer
        s <- sizeB b
        moveTo b 1000
        p  <- pointB b
        insertB b 'X'
        p' <- pointB b
        s' <- sizeB b
        c  <- readB b
        moveTo b s'
        let str = "haskell string\n\nanother string"
        insertN b str
        s'' <- sizeB b
        return $ do assertEqual (s+1) s'
                    assertEqual p p'
                    assertEqual c 'X'
                    assertEqual s'' (s' + length str)

 testDelete = unsafePerformIO $ do
        b <- newB "testbuffer" contents :: IO FBuffer
        moveTo b 2000
        p <- pointB b
        c <- readB b
        s <- sizeB b
        deleteB b
        c' <- readB b
        p' <- pointB b
        s' <- sizeB b
        moveTo b 0
        deleteN b 10000
        s'' <- sizeB b
        p'' <- pointB b
        c'' <- readB b  -- unsafe/should fail (empty buffer)
        writeB b 'X'    -- unsafe/should fail
        insertN b contents
        s''' <- sizeB b
        deleteN b s'''
        t  <- sizeB b
        insertN b contents
        deleteNAt b (s - 100) 100
        t'  <- sizeB b
        return $ do assert (c /= c')
                    assertEqual p p'
                    assertEqual s (s'+1)
                    assertEqual s'' 0
                    assertEqual p'' 0
                    assertEqual c'' (chr 0)     -- should throw an exception really
                    assertEqual s s'''
                    assertEqual t 0
                    assertEqual t' 100

 testUndo = unsafePerformIO $ do
        b <- newB "testbuffer" contents :: IO FBuffer
        s <- sizeB b
        deleteN b s
        s' <- sizeB b
        undo b
        s'' <-sizeB b
        t   <- elemsB b
        redo b >> redo b
        s'''<- sizeB b
        return $ do 
                assertEqual s s''
                assertEqual t contents -- contents after undo should equal original contents
                assertEqual s' s'''
                
 |])

instance Show Unique where
    showsPrec _ u = showString "<unique>"
    
