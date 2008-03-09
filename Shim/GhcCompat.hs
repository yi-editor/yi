{-# OPTIONS_GHC -cpp #-}
module Shim.GhcCompat where


import GHC hiding ( load, newSession )
import qualified GHC
import StaticFlags
import Panic
import qualified Distribution.PackageDescription as DP
import qualified Distribution.Simple.Utils as DSU
import Distribution.Verbosity
import Control.Concurrent.MVar ( tryTakeMVar, modifyMVar_, newMVar,
                                 readMVar, putMVar )
import Data.List ( nub )
import System.IO.Unsafe ( unsafePerformIO )


#if __GLASGOW_HASKELL__ == 606
{- needed to work around a bug in ghc 6.6: newSession hangs when
   called for the second time because 6.6 release has:
   interruptTargetThread = unsafePerformIO newEmptyMVar
   ...
   putMVar interruptTargetThread [main_thread]
   ghc > 6.6 has:
   interruptTargetThread = unsafePerformIO (newMVar [])
   ...
   modifyMVar_ interruptTargetThread (return . (main_thread :))
-}
newSession :: GhcMode -> Maybe FilePath -> IO Session
newSession mode mb_top_dir = do
  old <- tryTakeMVar interruptTargetThread
  case old of
    Nothing -> -- =6.6, first newSession and empty MVar
      do modifyMVar_ haveNewSessionBug (\_ -> return True)
         GHC.newSession mode mb_top_dir
    Just tids -> -- both ghc versions, nonempty MVar
      do bug <- readMVar haveNewSessionBug
         if bug
          then do ses <- GHC.newSession mode mb_top_dir
                  modifyMVar_ interruptTargetThread (return . (++tids))
                  return ses
          else do putMVar interruptTargetThread tids
                  GHC.newSession mode mb_top_dir

{-# NOINLINE haveNewSessionBug #-}
haveNewSessionBug = unsafePerformIO (newMVar False)

#else
-- Hack to get parseStaticFlags called only once
initGhc = unsafePerformIO$ StaticFlags.parseStaticFlags [] 

newSession :: Maybe FilePath -> IO Session
newSession fp = initGhc `seq` GHC.newSession fp

#endif

readPackageDescription = DP.readPackageDescription silent
findPackageDesc = DSU.findPackageDesc silent

