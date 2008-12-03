{-# LANGUAGE CPP #-}
module Shim.SHM where

import Data.Typeable

#if __GLASGOW_HASKELL__ >= 610
import GHC hiding ( load )
#else
import GHC hiding ( load, newSession )
#endif

import qualified GHC
import HscTypes
import Outputable
import ErrUtils

import qualified Control.OldException as CE
import Control.Monad.State
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

import qualified Data.Digest.Pure.MD5 as MD5

import Shim.SessionMonad
import Shim.Utils
import Shim.GhcCompat
--------------------------------------------------------------
-- SHM Monad
--------------------------------------------------------------


data CompilationResult
  = FileCompiled
  | ImportsOnly
  | PreludeOnly
  | NothingCompiled String
    deriving Show

replaceWith :: CompilationResult -> CompilationResult -> Bool
replaceWith _ FileCompiled = True
replaceWith FileCompiled _ = False
replaceWith _ ImportsOnly = True
replaceWith _ _ = False

type IdData = [(String, String)]

type Hash = MD5.MD5Digest


type CachedMod = (Hash,TypecheckedModule)
type CompBuffer = M.Map FilePath (CompilationResult, IdData, Maybe CachedMod)

type SessionMap = M.Map FilePath Session

data ShimState = ShimState
  { tempSession :: Session,
    sessionMap :: SessionMap,
    compBuffer :: CompBuffer,
    compLogAction :: Severity -> SrcSpan -> PprStyle -> Message -> IO () }
  deriving Typeable


type SHM = StateT ShimState IO

instance SessionMonad SHM where
  getSession = gets tempSession

lookupSession :: FilePath -> SHM (Maybe Session)
lookupSession cabalfile = do m <- gets sessionMap
                             return $ M.lookup cabalfile m

addSession :: FilePath -> Session -> SHM ()
addSession cabalfile ses =
  modify (\s ->
            s{sessionMap=(M.insert cabalfile ses (sessionMap s))})

shmHandle :: (CE.Exception -> SHM a) -> SHM a -> SHM a
shmHandle h m = StateT $ \s ->
                CE.catch (runStateT m s)
                         (\e -> runStateT (h e) s)

shmCatch :: SHM a -> (CE.Exception -> SHM a) -> SHM a
shmCatch = flip shmHandle

getCompBuffer :: SHM CompBuffer
getCompBuffer = gets compBuffer

addCompBuffer :: FilePath -> IdData -> CompilationResult -> Maybe (Hash,TypecheckedModule) -> SHM ()
addCompBuffer sourcefile id_data compilation_result checked_mod =
  modify (\s ->
            s{compBuffer=(M.insert sourcefile (compilation_result,id_data,checked_mod)
                                   (compBuffer s))})

io :: IO a -> SHM a
io = lift

runSHM :: Session -> FilePath -> (Severity -> SrcSpan -> PprStyle -> Message -> IO ()) -> SHM a -> IO a
runSHM ses ghc compLogAction m = liftM fst $ runStateT m
                             $ ShimState {tempSession=ses,
                                          sessionMap=M.empty,
                                          compBuffer=M.empty,
                                          compLogAction=compLogAction}

logInfo :: String -> SHM ()
logInfo = io . logS

getCompLogAction :: SHM (Severity -> SrcSpan -> PprStyle -> Message -> IO ())
getCompLogAction = gets compLogAction
