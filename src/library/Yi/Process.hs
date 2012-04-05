{-# LANGUAGE CPP #-}
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
module Yi.Process (popen, runProgCommand, runShellCommand, shellFileName,
                   createSubprocess, readAvailable, SubprocessInfo(..), SubprocessId) where

import System.Exit (ExitCode(ExitFailure))
import System.Directory (findExecutable)
import System.IO
import System.Process
import System.Environment ( getEnv )

import Control.Concurrent       (forkIO)
import qualified Control.OldException as Control.Exception

import Foreign.Marshal.Alloc(allocaBytes)
import Foreign.C.String

import Prelude(length, catch)
import Yi.Prelude
import Yi.Buffer (BufferRef)

#ifndef mingw32_HOST_OS
import System.Posix.IO
#endif


-- | A Posix.popen compatibility mapping.
-- Based on PosixCompat, originally written by Derek Elkins for lambdabot
-- TODO: this will probably be called readProcess in the new process package (2.0)
popen :: FilePath -> [String] -> Maybe String -> IO (String,String,ExitCode)
popen file args minput =
    Control.Exception.handle (\e -> return ([],show e,error (show e))) $ do

    (inp,out,err,pid) <- runInteractiveProcess file args Nothing Nothing
    hSetBuffering out LineBuffering
    hSetBuffering err LineBuffering
    case minput of
        Just input -> hPutStr inp input >> hClose inp -- important!
        Nothing    -> return ()

    -- Now, grab the input
    output <- hGetContents out
    errput <- hGetContents err

    -- SimonM sez:
    --  ... avoids blocking the main thread, but ensures that all the
    --  data gets pulled as it becomes available. you have to force the
    --  output strings before waiting for the process to terminate.
    --
    discard $ forkIO (Control.Exception.evaluate (length output) >> return ())
    discard $ forkIO (Control.Exception.evaluate (length errput) >> return ())

    -- And now we wait. We must wait after we read, unsurprisingly.
    exitCode <- waitForProcess pid -- blocks without -threaded, you're warned.

    return (output,errput,exitCode)

-- | Run a command. This looks up a program name in \$PATH, but then calls it
-- directly with the argument.
runProgCommand :: String -> [String] -> IO (String,String,ExitCode)
runProgCommand prog args = do loc <- findExecutable prog
                              case loc of 
                                  Nothing -> return ("","",ExitFailure 1)
                                  Just fp -> popen fp args Nothing

------------------------------------------------------------------------
-- | Run a command using the system shell, returning stdout, stderr and exit code

shellFileName :: IO String
shellFileName = catch (getEnv "SHELL") (const $ return "/bin/sh")

shellCommandSwitch :: String
shellCommandSwitch = "-c"

runShellCommand :: String -> IO (String,String,ExitCode)
runShellCommand cmd = do
      sh <- shellFileName
      popen sh [shellCommandSwitch, cmd] Nothing


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
    (inpReadFd,inpWriteFd) <- createPipe
    (outReadFd,outWriteFd) <- createPipe
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
    return $ SubprocessInfo { procCmd=cmd, procArgs=args, procHandle=handle, hIn=inp, hOut=out, hErr=err, bufRef=bufref, separateStdErr=separate }


-- Read as much as possible from handle without blocking
readAvailable :: Handle -> IO String
readAvailable handle = (fmap concat) $ repeatUntilM $ read_chunk handle

-- Read a chunk from a handle, bool indicates if there is potentially more data available
read_chunk :: Handle -> IO (Bool,String)  
read_chunk handle = do 
    let bufferSize = 1024
    allocaBytes bufferSize $ \buffer -> do
                 bytesRead <- hGetBufNonBlocking handle buffer bufferSize
                 s <- peekCStringLen (buffer,bytesRead)
                 let mightHaveMore = (bytesRead == bufferSize)
                 return (mightHaveMore, s)


