-- Test the fast buffer implementation

module Tests.Buffer where

import Yi.Buffer
import Yi.FastBuffer

import Data.Unique
import Data.Char
import Data.List
import Data.Maybe

import System.Directory
import System.IO.Unsafe

import Control.Monad
import qualified Control.Exception
import Control.Concurrent

import TestFramework

instance Show Unique where
    showsPrec _ u = showString "<unique>"

-- really test it!
contents = unsafePerformIO $  do
        s <- readFile "data"
        forkIO (Control.Exception.evaluate (length s) >> return ())
        return s

lendata = length contents

str ="\n\nabc\n\ndef\n"

lenstr = length str

$(tests "fastBuffer" [d|

 testElems = do
        b <- newB "testbuffer" contents :: IO FBuffer
        s <- elemsB b
        assertEqual s contents

 testHNewB = do
        b  <- newB "testbuffer" contents :: IO FBuffer
        b' <- hNewB "data"  :: IO FBuffer
        s  <- elemsB b
        s' <- elemsB b
        assertEqual s s'
        assertEqual s contents

 testName = do
        b <- newB "testbuffer" contents :: IO FBuffer
        assertEqual (nameB b) "testbuffer"

 testSize = do
        b <- newB "testbuffer" contents :: IO FBuffer
        i <- sizeB b
        assertEqual i (length contents)

 testWriteFile = do
        b <- newB "testbuffer" contents :: IO FBuffer
        let f = "testWriteFile.file"
        hPutB b f
        s <- readFile f
        removeFile f
        assertEqual s contents

 testGetFileNameB = do
        b <- hNewB "../README" :: IO FBuffer
        m <- getfileB b
        assertEqual m (Just "../README")

 testSetFileNameB = do
        b <- newB "testbuffer" contents :: IO FBuffer
        setfileB b "../README"
        m <- getfileB b
        assertEqual m (Just "../README")

 testLotsOfBuffers = do
        bs <- sequence [ hNewB "data" :: IO FBuffer
                       | x <- [1..10] ]   -- create a 1000 buffers

        bs' <- mapM elemsB bs

        assertEqual (length . nub . sort $ map keyB bs) (length bs)
        assertEqual 1 (length . nub . sort $ bs')
        assert (all (==contents) bs')

 testMoveTo = do
        b  <- hNewB "data" :: IO FBuffer
        ps <- sequence [ moveTo b i >> pointB b >>= \j -> return (i,j)
                       | i <- [0 .. 4000] ]
        let (l1,l2) = unzip ps
        assertEqual l1 l2

 testMovement = do
        b  <- hNewB "data" :: IO FBuffer
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
        rightN b 1000000  -- moving past end of buffer should only go to end
        m <- pointB b
        s <- sizeB b
        leftN b 1000000
        n <- pointB b
        assertEqual i 1000
        assertEqual j i
        assertEqual k (1000+1000)
        assertEqual l 0
        assertEqual m (s-1)
        assertEqual n 0

 testRead = do
        b  <- hNewB "data" :: IO FBuffer
        c  <- readAtB b 1000
        moveTo b 1000
        c' <- readB b
        writeB b 'X'
        c'' <- readB b
        assertEqual c c'
        assertEqual c'' 'X'

 testInsert = do
        b <- hNewB "data" :: IO FBuffer
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
        assertEqual (s+1) s'
        assertEqual p p'
        assertEqual c 'X'
        assertEqual s'' (s' + length str)

 testDelete = do
        b <- hNewB "data" :: IO FBuffer
        moveTo b 10000
        p <- pointB b
        c <- readB b    -- read a char
        s <- sizeB b
        deleteB b
        c' <- readB b   -- read this char
        p' <- pointB b
        s' <- sizeB b
        moveTo b 0
        deleteN b 10000000
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
        assert (c /= c')
        assertEqual p p'
        assertEqual s (s'+1)
        assertEqual s'' 0
        assertEqual p'' 0
        assertEqual c'' (chr 0)     -- should throw an exception really
        assertEqual s s'''
        assertEqual t 0
        assertEqual t' 100

 testUndo = do
        b <- hNewB "data" :: IO FBuffer
        s <- sizeB b
        deleteN b s
        s' <- sizeB b
        undo b
        s'' <-sizeB b
        t   <- elemsB b
        redo b >> redo b
        s'''<- sizeB b
        assertEqual s s''
        assertEqual t contents -- contents after undo should equal original contents
        assertEqual s' s'''

 testMoveToSol = do
        b <- newB "testbuffer" "\n\nabc\n\ndef\n" :: IO FBuffer
        -- expected sol points
        let pure = [0,1,2,2,2,2,6,7,7,7,7]
        impure <- sequence [ do moveTo b i
                                moveToSol b
                                pointB b
                           | i <- [ 0 .. length pure - 1] ]
        assertEqual pure impure

 testMoveToEol = do
        b <- newB "testbuffer" "\n\nabc\n\ndef\n" :: IO FBuffer
        -- expected eol points
        let pure = [0,1,5,5,5,5,6,10,10,10,10]
        impure <- sequence [ do moveTo b i
                                moveToEol b
                                pointB b
                           | i <- [ 0 .. (length pure - 1) ] ]
        assertEqual pure impure

 testAtSol = do
        b <- newB "testbuffer" str :: IO FBuffer
        -- points where atSol is true
        let pure = [0,1,2,6,7]
        impure <- sequence [ do moveTo b i
                                b <- atSol b
                                return $ if b then Just i else Nothing
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual pure (map fromJust $ filter isJust impure)

 testAtEol = do
        b <- newB "testbuffer" str :: IO FBuffer
        -- points where atEol is true
        let pure = [0,1,5,6,10]
        impure <- sequence [ do moveTo b i
                                b <- atEol b
                                return $ if b then Just i else Nothing
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual pure (map fromJust $ filter isJust impure)

 testAtSof = do
        b <- newB "testbuffer" str :: IO FBuffer
        -- points where atSof is true
        impure <- sequence [ do moveTo b i
                                b <- atSof b
                                return $ if b then Just i else Nothing
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [0] (map fromJust $ filter isJust impure)

 testHardAtSof = do
        b <- hNewB "data" :: IO FBuffer
        -- points where atSof is true
        impure <- sequence [ do moveTo b i
                                b <- atSof b
                                return $ if b then Just i else Nothing
                           | i <- [ 0 .. (lendata - 1) ] ]
        assertEqual [0] (map fromJust $ filter isJust impure)

 testAtEof = do
        b <- newB "testbuffer" str :: IO FBuffer
        -- points where atEof is true
        impure <- sequence [ do moveTo b i
                                b <- atEof b
                                return $ if b then Just i else Nothing
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [10] (map fromJust $ filter isJust impure)

 testHardAtEof = do
        b <- hNewB "data" :: IO FBuffer
        impure <- sequence [ do moveTo b i
                                b <- atEof b
                                return $ if b then Just i else Nothing
                           | i <- [ 0 .. (lendata - 1) ] ]
        assertEqual [lendata - 1] (map fromJust $ filter isJust impure)

 testAtLastLine = do
        b <- newB "testbuffer" str :: IO FBuffer
        -- points where atEof is true
        impure <- sequence [ do moveTo b i
                                b <- atLastLine b
                                return $ if b then Just i else Nothing
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [7,8,9,10] (map fromJust $ filter isJust impure)

 testHardAtLastLine = do
        b <- hNewB "data" :: IO FBuffer
        -- points where atEof is true
        impure <- sequence [ do moveTo b i
                                b <- atLastLine b
                                return $ if b then Just i else Nothing
                           | i <- [ 0 .. (lendata - 1) ] ]
        assertEqual [256927] (map fromJust $ filter isJust impure)

 testOffsetFromSol = do
        b <- newB "testbuffer" str :: IO FBuffer
        impure <- sequence [ moveTo b i >> offsetFromSol b
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [0,0,0,1,2,3,0,0,1,2,3] impure

 testIndexOfSol = do
        b <- newB "testbuffer" str :: IO FBuffer
        impure <- sequence [ moveTo b i >> indexOfSol b
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [0,1,2,2,2,2,6,7,7,7,7] impure

 testIndexOfEol = do
        b <- newB "testbuffer" str :: IO FBuffer
        impure <- sequence [ moveTo b i >> indexOfEol b
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [0,1,5,5,5,5,6,10,10,10,10] impure

 -- the point of the next line down
 testIndexOfNLFrom = do
        b <- newB "testbuffer" str :: IO FBuffer
        impure <- sequence [ indexOfNLFrom b i
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [1,2,6,6,6,6,7,10,10,10,10] impure

 testMoveAXuntil = do
        b <- newB "testbuffer" str :: IO FBuffer
        impure <- sequence [ do moveTo b i
                                moveAXuntil b rightB 2 ((('\n' ==) `fmap`) . readB)
                                pointB b
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [0,1,4,5,5,5,6,9,10,10,10] impure

 testDeleteToEol = do
        impure <- sequence [ do b <- newB "testbuffer" str :: IO FBuffer
                                moveTo b i
                                deleteToEol b
                                elemsB b
                           | i <- [ 0 .. (lenstr - 1) ] ]
        let pure = ["\n\nabc\n\ndef\n",
                    "\n\nabc\n\ndef\n",
                    "\n\n\n\ndef\n",
                    "\n\na\n\ndef\n",
                    "\n\nab\n\ndef\n",
                    "\n\nabc\n\ndef\n",
                    "\n\nabc\n\ndef\n",
                    "\n\nabc\n\n\n",
                    "\n\nabc\n\nd\n",
                    "\n\nabc\n\nde\n",
                    "\n\nabc\n\ndef\n"]
        assertEqual pure impure

 testLineUp = do
        b <- newB "testbuffer" str :: IO FBuffer
        impure <- sequence [ do moveTo b i
                                lineUp b
                                pointB b
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [0,0,1,1,1,1,2,6,6,6,6] impure

 testLineDown = do
        b <- newB "testbuffer" str :: IO FBuffer
        impure <- sequence [ do moveTo b i
                                lineDown b
                                pointB b
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [1,2,6,6,6,6,7,10,10,10,10] impure

 testCurLn = do
        b <- newB "testbuffer" str :: IO FBuffer
        impure <- sequence [ moveTo b i >> curLn b
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [1,2,3,3,3,3,4,5,5,5,5] impure

 testGotoLn = do
        b <- newB "testbuffer" str :: IO FBuffer
        impure <- sequence [ moveTo b i >> gotoLn b 4
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [4,4,4,4,4,4,4,4,4,4,4] impure

 testHardGotoLn = do
        b <- hNewB "data" :: IO FBuffer
        impure <- sequence [ moveTo b i >> gotoLn b 4
                           | i <- [ 0 .. (lendata - 1) ] ]
        assertEqual (replicate lendata 4) impure

 testGotoLnFrom = do
        b <- newB "testbuffer" str :: IO FBuffer
        impure <- sequence [ gotoLnFrom b i
                           | i <- [ 0 .. (lenstr - 1) ] ]
        assertEqual [1,1,3,4,4,4,4,4,4,4,4] impure

 testHardGotoLnFrom = do
        b <- hNewB "data" :: IO FBuffer
        impure <- sequence [ gotoLnFrom b 100
                           | i <- [ 1 .. length [100, 200 .. 3900 ] ] ]
        assertEqual [100,200..3900] impure

 testSearch = do
        b <- newB "T" contents :: IO FBuffer
        let loop = do
                r <- searchB b "Utopia"
                case r of
                        Nothing -> return []
                        Just i  -> do moveTo b (i+1)
                                      is <- loop
                                      return (i : is)
        rs <- loop
        assertEqual [29,339,1634,4945,5971,6205,7310,7379,7961,17241,56306,65159,69398,73199,77132,80891,97946,101903,103878,110169,121298,125041,126151,126749,127213,127445,129210,129293,143137,145856,146655,148945,160049,175824,176122,176860,179745,183199,183825,185498,187063,189432,191387,191468,191960,200269,208806,214785,219347,229205,233376,235247,235922,238163] rs

 |])
