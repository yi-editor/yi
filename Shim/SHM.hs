{-# OPTIONS -fglasgow-exts #-}
module Shim.SHM where

import GHC hiding ( load, newSession )
import qualified GHC
import Outputable
import ErrUtils

import qualified Control.Exception as CE
import Control.Monad.State
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

import Shim.SessionMonad
import Shim.Utils
--------------------------------------------------------------
-- SHM Monad
--------------------------------------------------------------

data CompileNote = CompileNote {severity :: Severity,
                                srcSpan :: SrcSpan,
                                pprStyle :: PprStyle,
                                message :: Message,
                                projectdir :: FilePath}


instance Show CompileNote where
    show n = show $
               (hang (ppr (srcSpan n) <> colon) 4  (message n)) (pprStyle n)
                     

data CompilationResult
  = FileCompiled [CompileNote]
  | ImportsOnly [CompileNote]
  | PreludeOnly [CompileNote]
  | NothingCompiled String [CompileNote]
    deriving Show


replaceWith :: CompilationResult -> CompilationResult -> Bool
replaceWith _ (FileCompiled _ ) = True
replaceWith (FileCompiled _ ) _ = False
replaceWith _ (ImportsOnly _ ) = True
replaceWith _ _ = False

type IdData = [(String, String)]

type Hash = BC.ByteString
type CachedMod = (Hash,CheckedModule)
type CompBuffer = M.Map FilePath (CompilationResult, IdData, Maybe CachedMod)

type SessionMap = M.Map FilePath Session

data ShimState = ShimState
  { ghcProgram :: String,
    tempSession :: Session,
    sessionMap :: SessionMap,
    compBuffer :: CompBuffer }

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

addCompBuffer :: FilePath -> IdData -> CompilationResult -> Maybe (Hash,CheckedModule) -> SHM ()
addCompBuffer sourcefile id_data compilation_result checked_mod =
  modify (\s ->
            s{compBuffer=(M.insert sourcefile (compilation_result,id_data,checked_mod)
                                   (compBuffer s))})

io :: IO a -> SHM a
io = lift

runSHM :: Session -> String -> SHM a -> IO a
runSHM ses ghc m = liftM fst $ runStateT m
                             $ ShimState {ghcProgram=ghc,
                                          tempSession=ses,
                                          sessionMap=M.empty,
                                          compBuffer=M.empty}

logInfo :: String -> SHM ()
logInfo = io . logS
