-- Test the core operations

module Tests.Core where

import Yi.Core

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

$(tests "core" [d| 

 testQuit = do
    v <- Control.Exception.catch
        (quitE >> return False {- shouldn't return! -})
        (\e -> return $ case e of
                ExitException _ -> True
                _ -> False)
    assert v

 |])
