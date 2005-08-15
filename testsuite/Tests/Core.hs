-- Test the core operations

module Tests.Core where

import Yi.Core
import Yi.Window
import Yi.Editor
import Yi.Buffer

import Data.Char
import Data.List
import Data.Maybe

import System.Directory
import System.IO.Unsafe

import Control.Monad
import qualified Control.Exception
import Control.Concurrent

import GHC.Exception            ( Exception(ExitException) )

import TestFramework

-- interesting window state
getW :: IO [Int]
getW = withWindow $ \w b -> do
        p <- pointB b
        let q     = pnt w 
            (i,j) = cursor w
            ln    = lineno w
            tp    = tospnt w
            tln   = toslineno w
        return (w, [p,q,i,j,ln,tp,tln])

$(tests "core" [d| 

 testQuit = do
    v <- Control.Exception.catch
        (quitE >> return False {- shouldn't return! -})
        (\e -> return $ case e of
                ExitException _ -> True
                _ -> False)
    assert v

 testNopE = do
   v <- nopE
   assertEqual () v

 testTopE = do
   emptyE >> fnewE "data"
   topE
   v <- getW
   downE 
   u <- getW
   upE
   v' <- getW
   assertEqual [0,0,0,0,1,0,1] v
   assertEqual v v'
   assertEqual [76,76,1,0,2,0,1] u

 testBotE = do
   emptyE >> fnewE "data"
   botE >> solE
   v <- getW
   upE
   downE >> solE
   u <- getW
   assertEqual [256927,256927,30,0,3926,255597,3896] v
   assertEqual u v

 testSolEolE = do
   emptyE >> fnewE "data"
   gotoLnE 20
   solE
   v <- getW
   eolE
   u <- getW
   solE
   v' <- getW
   eolE 
   u' <- getW
   assertEqual v v'
   assertEqual u u'

 testGotoLnE  = do
   emptyE >> fnewE "data"
   ws <- sequence [ gotoLnE i >> getW >>= \i -> return (i !! 4) | i <- [1 .. 3926] ]
   assertEqual [1..3926] ws

 testGotoLnFromE  = do
   emptyE >> fnewE "data"
   ws <- sequence [ do gotoLnE i
                       gotoLnFromE 2
                       i <- getW
                       return (i !! 4) 
                  | i <- [1 .. 3925] ]
   assertEqual ([2..3925]++[3925]) ws

 |])
