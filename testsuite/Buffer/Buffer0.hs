-- Test the fast buffer implementation

module Buffer.Buffer0 where

import Yi.Buffer
import Yi.FastBuffer

import System.IO.Unsafe

import TestFramework

contents = "contents\nstring"

$(tests "fastBuffer" [d| 

 testElemsB = unsafePerformIO $ do
             b <- newB "testbuffer" contents :: IO FBuffer
             s <- elemsB b
             return $ assertEqual s contents

 testNameB = unsafePerformIO $ do
             b <- newB "testbuffer" contents :: IO FBuffer
             return $ assertEqual (nameB b) "testbuffer"

 testSizeB = unsafePerformIO $ do
             b <- newB "testbuffer" contents :: IO FBuffer
             i <- sizeB b
             return $ assertEqual i (length contents)

 |])
