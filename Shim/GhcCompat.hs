{-# OPTIONS_GHC -cpp #-}
module Shim.GhcCompat
  ( load
  , getModuleGraph
  , getSessionDynFlags
  , getRdrNamesInScope
  , findModule
  , exprType
  , getPrintUnqual
  , compileExpr
  , setSessionDynFlags
  , setTargets
  , setContext
  , getModuleInfo
  , lookupName
  , newSession
  , getContext
  , modInfoLookupName
  , checkModule
  , parseDynamicFlags
  , workingDirectoryChanged
  , getNamesInScope
  ) where

#if __GLASGOW_HASKELL__ >= 610
import GHC hiding ( load, getModuleGraph, getSessionDynFlags, getRdrNamesInScope,
                    findModule, exprType, getPrintUnqual, compileExpr, setSessionDynFlags,
                    setTargets, setContext, load, getModuleInfo, lookupName, getContext,
                    modInfoLookupName, parseDynamicFlags, workingDirectoryChanged,
                    getNamesInScope)
#else
import GHC hiding ( load, newSession, parseDynamicFlags )
#endif

import qualified GHC as GHC
import StaticFlags
import Panic
import HscTypes

-- FIX: we should check for Cabal version instead
#if __GLASGOW_HASKELL__ >= 610
import qualified Distribution.PackageDescription.Parse as DP
#else
import qualified Distribution.PackageDescription as DP
#endif

import Distribution.Verbosity
import Control.Concurrent.MVar ( tryTakeMVar, modifyMVar_, newMVar,
                                 readMVar, putMVar )
import Data.List ( nub, find )
import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef


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

#elif __GLASGOW_HASKELL__ == 608 
-- Hack to get parseStaticFlags called only once
initGhc = unsafePerformIO$ parseStaticFlags [] 

newSession :: Maybe FilePath -> IO Session
newSession fp = initGhc `seq` GHC.newSession fp

-- >= 6.10
#else

newSession :: Maybe FilePath -> IO Session
newSession fp = runGhc fp getRealSession


getRealSession :: Ghc Session
getRealSession = do
  hscEnv <- getSession
  warns  <- getWarnings
  ref1 <- liftIO $ newIORef hscEnv
  ref2 <- liftIO $ newIORef warns
  return $ Session ref1 ref2 

#endif

readPackageDescription = DP.readPackageDescription silent

#if __GLASGOW_HASKELL__ < 610
type TypecheckedModule = CheckedModule
parseDynamicFlags ses a b = GHC.parseDynamicFlags a b
#endif

#if __GLASGOW_HASKELL__ >= 610
getModuleGraph session = reflectGhc GHC.getModuleGraph session
getSessionDynFlags session = reflectGhc GHC.getSessionDynFlags session
setSessionDynFlags session f = reflectGhc (GHC.setSessionDynFlags f) session
findModule session a b = reflectGhc (GHC.findModule a b) session
getRdrNamesInScope session = reflectGhc GHC.getRdrNamesInScope session

-- FIX: we should catch the exception
exprType session e = fmap Just $ reflectGhc (GHC.exprType e) session

getPrintUnqual session = reflectGhc GHC.getPrintUnqual session

-- FIX: we should catch the exception
compileExpr session e = fmap Just $ reflectGhc (GHC.compileExpr e) session

setTargets session ts = reflectGhc (GHC.setTargets ts) session
setContext session a b = reflectGhc (GHC.setContext a b) session
load session a = reflectGhc (GHC.load a) session
getModuleInfo session a = reflectGhc (GHC.getModuleInfo a) session
lookupName session a = reflectGhc (GHC.lookupName a) session
modInfoLookupName session a b = reflectGhc (GHC.modInfoLookupName a b) session
parseDynamicFlags session a b = fmap (\(a,b,_) -> (a,b)) $ reflectGhc (GHC.parseDynamicFlags a (map noLoc b)) session
getContext session = reflectGhc GHC.getContext session
workingDirectoryChanged session = reflectGhc GHC.workingDirectoryChanged session
getNamesInScope session = reflectGhc GHC.getNamesInScope session

checkModule :: Session -> ModuleName -> Bool -> IO (Maybe TypecheckedModule)
checkModule session modname _ = do
  graph <- getModuleGraph session
  let res = find ((modname ==) . moduleName . ms_mod) graph
  case res of
    Just modsum -> fmap Just $ reflectGhc (typecheckModule =<< parseModule modsum) session
    Nothing -> return Nothing 
#endif

