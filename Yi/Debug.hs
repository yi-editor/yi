module Yi.Debug (
        initDebug       -- :: FilePath -> IO ()
       ,trace           -- :: String -> a -> a
       ,logPutStrLn
       ,logError
       ,logStream
       ,Yi.Debug.error
    ) where

import Control.Concurrent
import Control.Monad.Trans
import Data.IORef
import System.IO
import System.IO.Unsafe ( unsafePerformIO )
import System.Time

dbgHandle :: IORef (Maybe Handle)
dbgHandle = unsafePerformIO $ newIORef Nothing
{-# NOINLINE dbgHandle #-}

-- | Set the file to which debugging output should be written. Though this
-- is called /init/Debug. 
-- Debugging output is not created by default (i.e., if this function
-- is never called.)
-- The target file can not be changed, nor debugging disabled.
initDebug :: FilePath -> IO ()
initDebug f = do
  hndl <- readIORef dbgHandle
  case hndl of
    Nothing -> do openFile f WriteMode >>= writeIORef dbgHandle . Just
                  logPutStrLn "Logging initialized."
    Just _ -> do logPutStrLn "Attempt to re-initialize the logging system."
                     
              
               

-- | Outputs the given string before returning the second argument.
trace :: String -> a -> a
trace s e = unsafePerformIO $ do logPutStrLn s
                                 return e
{-# NOINLINE trace #-}


error :: String -> a
error s = unsafePerformIO $ do logPutStrLn s
                               Prelude.error s

logPutStrLn :: (MonadIO m) => [Char] -> m ()
logPutStrLn s = liftIO $ do
                   mh <- readIORef dbgHandle
                   case mh of
                     Nothing -> return ()
                     Just h -> do
                       time <- toCalendarTime =<< getClockTime
                       tId <- myThreadId
                       hPutStrLn h $ calendarTimeToString time ++ " " ++ show tId ++ " " ++ s
                       hFlush h

logError :: (MonadIO m) => String -> m ()
logError s = logPutStrLn $ "error: " ++ s

logStream :: Show a => String -> Chan a -> IO ()
logStream msg ch = do
  logPutStrLn $ "Logging stream " ++ msg
  forkIO $ logStreamThread msg  ch
  return ()

logStreamThread :: Show a => String -> Chan a -> IO ()
logStreamThread msg ch = do
  stream <- getChanContents =<< dupChan ch
  mapM_ logPutStrLn [msg ++ "(" ++ show i ++ ")" ++ show event | (event, i) <- zip stream [(0::Int)..]]
