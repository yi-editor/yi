--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
--

--
-- | A Posix.popen compatibility mapping.
-- Based on PosixCompat, originally written by Derek Elkins for lambdabot
--
module Yi.Process (popen, runShellCommand, shellFileName,
                   createSubprocess, readAvailable, SubprocessInfo(..), SubprocessId) where
import System.IO
import System.Process
import System.Exit ( ExitCode )
import System.Environment ( getEnv )

import Control.Concurrent       (forkIO)
import Control.Monad(liftM)
import qualified Control.Exception

import Foreign.Marshal.Alloc(allocaBytes)
import Foreign.C.String

import Yi.Buffer (BufferRef)
import Yi.Monad(repeatUntilM)

popen :: FilePath -> [String] -> Maybe String -> IO (String,String,ExitCode)
popen file args minput =
    Control.Exception.handle (\e -> return ([],show e,error (show e))) $ do

    (inp,out,err,pid) <- runInteractiveProcess file args Nothing Nothing

    case minput of
        Just input -> hPutStr inp input >> hClose inp -- importante!
        Nothing    -> return ()

    -- Now, grab the input
    output <- hGetContents out
    errput <- hGetContents err

    -- SimonM sez:
    --  ... avoids blocking the main thread, but ensures that all the
    --  data gets pulled as it becomes available. you have to force the
    --  output strings before waiting for the process to terminate.
    --
    forkIO (Control.Exception.evaluate (length output) >> return ())
    forkIO (Control.Exception.evaluate (length errput) >> return ())

    -- And now we wait. We must wait after we read, unsurprisingly.
    exitCode <- waitForProcess pid -- blocks without -threaded, you're warned.

    return (output,errput,exitCode)

------------------------------------------------------------------------
-- | Run a command using the system shell, returning stdout, stderr and exit code

shellFileName :: IO String
shellFileName = Prelude.catch (getEnv "SHELL") (\_ -> return "/bin/sh")

shellCommandSwitch :: String
shellCommandSwitch = "-c"

runShellCommand :: String -> IO (String,String,ExitCode)
runShellCommand cmd = do
      shell <- shellFileName
      popen shell [shellCommandSwitch, cmd] Nothing


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
      bufRef :: BufferRef
      }

createSubprocess :: FilePath -> [String] -> BufferRef -> IO SubprocessInfo
createSubprocess cmd args bufref = do
    (inp,out,err,handle) <- runInteractiveProcess cmd args Nothing Nothing
    hSetBuffering inp NoBuffering
    hSetBuffering out NoBuffering
    hSetBuffering err NoBuffering
    return $ SubprocessInfo { procCmd=cmd, procArgs=args, procHandle=handle, hIn=inp, hOut=out, hErr=err, bufRef=bufref }

-- Read as much as possible from handle without blocking
readAvailable :: Handle -> IO String
readAvailable handle = (liftM concat) $ repeatUntilM $ read_chunk handle

-- Read a chunk from a handle, bool indicates if there is potentially more data available
read_chunk :: Handle -> IO (Bool,String)  
read_chunk handle = do 
    let bufferSize = 1024
    allocaBytes bufferSize $ \buffer -> do
                 bytesRead <- hGetBufNonBlocking handle buffer bufferSize
                 s <- peekCStringLen (buffer,bytesRead)
                 let mightHaveMore = (bytesRead == bufferSize)
                 return (mightHaveMore, s)


