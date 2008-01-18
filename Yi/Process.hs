--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
--

--
-- | A Posix.popen compatibility mapping.
-- Based on PosixCompat, originally written by Derek Elkins for lambdabot
--
module Yi.Process (popen, runShellCommand) where

import System.IO
import System.Process
import System.Exit ( ExitCode )
import Control.Concurrent       (forkIO)
import System.Environment ( getEnv )

import qualified Control.Exception

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
