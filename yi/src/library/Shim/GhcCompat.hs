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

import GHC hiding ( load, getModuleGraph, getSessionDynFlags, getRdrNamesInScope,
                    findModule, exprType, getPrintUnqual, compileExpr, setSessionDynFlags,
                    setTargets, setContext, load, getModuleInfo, lookupName, getContext,
                    modInfoLookupName, parseDynamicFlags, workingDirectoryChanged,
                    getNamesInScope)

import qualified GHC as GHC
import StaticFlags
import Panic
import HscTypes

-- FIX: we should check for Cabal version instead
import qualified Distribution.PackageDescription.Parse as DP

import Distribution.Verbosity
import Control.Concurrent.MVar ( tryTakeMVar, modifyMVar_, newMVar,
                                 readMVar, putMVar )
import Data.List ( nub, find )
import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef



newSession :: Maybe FilePath -> IO Session
newSession fp = runGhc fp getRealSession


getRealSession :: Ghc Session
getRealSession = do
  hscEnv <- getSession
  warns  <- getWarnings
  ref1 <- liftIO $ newIORef hscEnv
  ref2 <- liftIO $ newIORef warns
  return $ Session ref1 ref2


readPackageDescription = DP.readPackageDescription silent

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

