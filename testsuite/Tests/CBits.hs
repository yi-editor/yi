{-# OPTIONS -fffi #-}
-- Test the fast buffer implementation


module Tests.CBits where

import Yi.Buffer
import Yi.FastBuffer
import Yi.Process       ( popen )

import Data.Unique
import Data.Char
import Data.List
import Data.Maybe

import System.Directory
import System.IO.Unsafe

import Control.Monad
import qualified Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar

import Foreign.C.Types          ( CChar )
import Foreign.Ptr              ( Ptr )

import TestFramework

contents = unsafePerformIO $  do
        s <- readFile "data"
        forkIO (Control.Exception.evaluate (length s) >> return ())
        return s

lendata = length contents

------------------------------------------------------------------------

foreign import ccall unsafe "YiUtils.h countLines"
   ccountlns :: Ptr CChar -> Int -> Int -> IO Int

foreign import ccall unsafe "YiUtils.h findStartOfLineN"
   cgotoln :: Ptr CChar -> Int -> Int -> Int -> IO Int

foreign import ccall unsafe "YiUtils.h expandedLengthOfStr"
   ctabwidths :: Ptr CChar -> Int -> Int -> Int -> IO Int

foreign import ccall unsafe "YiUtils.h strlenWithExpandedLengthN"
   cstrlentabbed :: Ptr CChar -> Int -> Int -> Int -> Int -> IO Int

------------------------------------------------------------------------

$(tests "cbits" [d| 

 testCountLines = do
        b  <- hNewB "data" :: IO FBuffer
        s  <- sizeB b
        i  <- docount b s
        deleteN b s
        s'  <- sizeB b
        i' <- docount b s'
        insertB b '\n'
        s'' <- sizeB b
        j <- docount b s''
        k <- docount b 0
        assertEqual (i-1) (length . filter (== '\n') $ contents)
        assertEqual 1 i'
        assertEqual 2 j
        assertEqual 1 k
    where
        docount :: FBuffer -> Int -> IO Int
        docount b end = do
            let (FBuffer { rawbuf = mv }) = b
            withMVar mv $ \(FBuffer_ ptr _ _ _) -> ccountlns ptr 0 end

 -- index of first point of line n
 testFindStartOfLineNForward = do
        b <- hNewB "data" :: IO FBuffer

        -- the index of the start of each line, line 1 starts at 0
        let pure  = 0 : (init . map (+1) . (findIndices (== '\n')) $ contents)

        -- now see if the fast version matches
        let (FBuffer { rawbuf = mv }) = b

        impure <- withMVar mv $ \(FBuffer_ ptr _ end _) ->
                sequence [ cgotoln ptr 0 end i | i <- [0 .. length pure-1] ]

        let n   = 20
        let p_n = pure !! n
        i_n <- withMVar mv $ \(FBuffer_ ptr _ end _) -> cgotoln ptr 0 end n

        assertEqual pure impure
        assertEqual p_n  i_n

 -- index of first point of line n
 testFindStartOfLineNBackward = do
        b <- hNewB "data" :: IO FBuffer

        -- distance from each point back to start of line
        let pure  = [ - (fromMaybe i $ findIndex (== '\n') (reverse (take i contents))) 
                    | i <- [ 0,1024 .. lendata -1 ] ]

        -- now see if the fast version matches
        let (FBuffer { rawbuf = mv }) = b

        -- distance back to start of line
        impure <- withMVar mv $ \(FBuffer_ ptr _ end _) ->
                sequence [ cgotoln ptr i 0 (-1) | i <- [0,1024 .. lendata-1] ]

        assertEqual pure impure

 testExpandedLengthOfStr = do
        let s = "\t\n.\t\n..\t\n...\t\n....\t\n.....\t\n......\t\n.......\t\n........\t\n" 
        b <- newB "testbuffer" s :: IO FBuffer
        let lns  = 0 : (init . map (+1) . (findIndices (== '\n')) $ s)
        let (FBuffer { rawbuf = mv }) = b
        impure <- withMVar mv $ \(FBuffer_ ptr _ end _) ->
                sequence [ liftM (+1) $ ctabwidths ptr i end 8 | i <- lns  ]
        assertEqual [8,7,6,5,4,3,2,1,8] impure

 testStrlenWithExpandedLengthN = do
        let s = "\t\n.\t\n..\t\n...\t\n....\t\n.....\t\n......\t\n.......\t\n........\t\n" 
        b <- newB "testbuffer" s :: IO FBuffer
        let lns  = 0 : (init . map (+1) . (findIndices (== '\n')) $ s)
        let (FBuffer { rawbuf = mv }) = b
        impure <- withMVar mv $ \(FBuffer_ ptr _ end _) ->
                sequence [ cstrlentabbed ptr i end 8 8 | i <- lns  ]
        assertEqual [1,2,3,4,5,6,7,8,8] impure

 |])
    
