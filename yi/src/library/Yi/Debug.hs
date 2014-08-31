{-# LANGUAGE FlexibleContexts #-}
module Yi.Debug (
        initDebug       -- :: FilePath -> IO ()
       ,trace           -- :: String -> a -> a
       ,traceM
       ,traceM_
       ,logPutStrLn
       ,logError
       ,logStream
       ,Yi.Debug.error
    ) where

import Control.Concurrent
import Control.Monad.Base
import Data.IORef
import GHC.Conc (labelThread)
import System.IO
import System.IO.Unsafe ( unsafePerformIO )
import Data.Time
import System.Locale

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
    Just _ -> logPutStrLn "Attempt to re-initialize the logging system."




-- | Outputs the given string before returning the second argument.
trace :: String -> a -> a
trace s e = unsafePerformIO $ do logPutStrLn s
                                 return e
{-# NOINLINE trace #-}


error :: String -> a
error s = unsafePerformIO $ do logPutStrLn s
                               Prelude.error s

logPutStrLn :: (MonadBase IO m) => String -> m ()
logPutStrLn s = liftBase $ do
                   mh <- readIORef dbgHandle
                   case mh of
                     Nothing -> return ()
                     Just h -> do
                       time <-  getCurrentTime
                       tId <- myThreadId
                       hPutStrLn h $ formatTime defaultTimeLocale rfc822DateFormat' time ++ " " ++ show tId ++ " " ++ s
                       hFlush h
    where
      -- A bug in rfc822DateFormat makes us use our own format string
      rfc822DateFormat' = "%a, %d %b %Y %H:%M:%S %Z"

logError :: (MonadBase IO m) => String -> m ()
logError s = logPutStrLn $ "error: " ++ s

logStream :: Show a => String -> Chan a -> IO ()
logStream msg ch = do
  logPutStrLn $ "Logging stream " ++ msg
  logThreadId <- forkIO $ logStreamThread msg ch
  labelThread logThreadId "LogStream"

logStreamThread :: Show a => String -> Chan a -> IO ()
logStreamThread msg ch = do
  stream <- getChanContents =<< dupChan ch
  mapM_ logPutStrLn [msg ++ "(" ++ show i ++ ")" ++ show event | (event, i) <- zip stream [(0::Int)..]]

-- | Traces @x@ and returns @y@.
traceM :: Monad m => String -> a -> m a
traceM x y = trace x $ return y

-- | Like traceM, but returns ().
traceM_ :: Monad m => String -> m ()
traceM_ x = traceM x ()
