{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yi.Process (runProgCommand, runShellCommand, shellFileName,
                   createSubprocess, readAvailable, SubprocessInfo(..), SubprocessId) where

import           Control.Exc           (orException)
import           Foreign.C.String      (peekCStringLen)
import           Foreign.Marshal.Alloc (allocaBytes)
import           System.Directory      (findExecutable)
import           System.Environment    (getEnv)
import           System.Exit           (ExitCode (ExitFailure))
import           System.IO             (BufferMode (NoBuffering), Handle, hGetBufNonBlocking, hSetBuffering)
import           System.Process        (ProcessHandle, readProcessWithExitCode, runProcess)
import           Yi.Buffer.Basic       (BufferRef)
import           Yi.Monad              (repeatUntilM)

#ifndef mingw32_HOST_OS
import System.Posix.IO (createPipe, fdToHandle)
#endif

runProgCommand :: String -> [String] -> IO (ExitCode,String,String)
runProgCommand prog args = do loc <- findExecutable prog
                              case loc of
                                  Nothing -> return (ExitFailure 1,"","")
                                  Just fp -> readProcessWithExitCode fp args ""

------------------------------------------------------------------------
-- | Run a command using the system shell, returning stdout, stderr and exit code

shellFileName :: IO String
shellFileName = orException (getEnv "SHELL") (return "/bin/sh")

shellCommandSwitch :: String
shellCommandSwitch = "-c"

runShellCommand :: String -> IO (ExitCode,String,String)
runShellCommand cmd = do
      sh <- shellFileName
      readProcessWithExitCode sh [shellCommandSwitch, cmd] ""


--------------------------------------------------------------------------------
-- Subprocess support (ie. async processes whose output goes to a buffer)

type SubprocessId = Integer

data SubprocessInfo = SubprocessInfo {
      procCmd :: FilePath,
      procArgs :: [String],
      procHandle :: ProcessHandle,
      hIn  :: Handle,
      hOut :: Handle,
      hErr :: Handle,
      bufRef :: BufferRef,
      separateStdErr :: Bool
      }

{-
Simon Marlow said this:

 It turns out to be dead easy to bind stderr and stdout to the same pipe. After a couple of minor tweaks the following now works:

 createProcess (proc cmd args){ std_out = CreatePipe,
                                std_err = UseHandle stdout }

Therefore it should be possible to simplifiy the following greatly with the new process package.

-}
createSubprocess :: FilePath -> [String] -> BufferRef -> IO SubprocessInfo
createSubprocess cmd args bufref = do

#ifdef mingw32_HOST_OS
    (inp,out,err,handle) <- runInteractiveProcess cmd args Nothing Nothing
    let separate = True
#else
    (inpReadFd,inpWriteFd) <- System.Posix.IO.createPipe
    (outReadFd,outWriteFd) <- System.Posix.IO.createPipe
    [inpRead,inpWrite,outRead,outWrite] <- mapM fdToHandle [inpReadFd,inpWriteFd,outReadFd,outWriteFd]

    handle <- runProcess cmd args Nothing Nothing (Just inpRead) (Just outWrite) (Just outWrite)
    let inp = inpWrite
        out = outRead
        err = outRead
        separate = False
#endif
    hSetBuffering inp NoBuffering
    hSetBuffering out NoBuffering
    hSetBuffering err NoBuffering
    return SubprocessInfo { procCmd=cmd, procArgs=args, procHandle=handle, hIn=inp, hOut=out, hErr=err, bufRef=bufref, separateStdErr=separate }


-- Read as much as possible from handle without blocking
readAvailable :: Handle -> IO String
readAvailable handle = fmap concat $ repeatUntilM $ readChunk handle

-- Read a chunk from a handle, bool indicates if there is potentially more data available
readChunk :: Handle -> IO (Bool,String)
readChunk handle = do
    let bufferSize = 1024
    allocaBytes bufferSize $ \buffer -> do
                 bytesRead <- hGetBufNonBlocking handle buffer bufferSize
                 s <- peekCStringLen (buffer,bytesRead)
                 let mightHaveMore = bytesRead == bufferSize
                 return (mightHaveMore, s)
