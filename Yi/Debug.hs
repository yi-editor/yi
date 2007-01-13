module Yi.Debug (
        initDebug       -- :: FilePath -> IO () 
       ,trace           -- :: String -> a -> a
       ,logPutStrLn
    ) where

import Control.Concurrent
import Data.IORef
import System.IO        
import System.IO.Unsafe ( unsafePerformIO )
import System.Time

dbgHandle :: IORef Handle
dbgHandle = unsafePerformIO $ newIORef stderr

-- Set the file to which debugging output should be written. Though this
-- is called /init/Debug. This function should be called at most once. 
-- Debugging output is sent to stderr by default (i.e., if this function
-- is never called.
initDebug :: FilePath -> IO ()
initDebug f = do 
  openFile f WriteMode >>= writeIORef dbgHandle
  logPutStrLn "Logging initialized."

-- Outputs the given string before returning the second argument.
trace :: String -> a -> a
trace s e = unsafePerformIO $ do logPutStrLn s
                                 return e
{-# NOINLINE trace #-}




logPutStrLn :: String -> IO ()
logPutStrLn s = do time <- toCalendarTime =<< getClockTime
                   tId <- myThreadId
                   h <- readIORef dbgHandle
                   hPutStrLn h $ calendarTimeToString time ++ " " ++ show tId ++ " " ++ s
                   hFlush h
