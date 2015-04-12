{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Debug
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Debug utilities used throughout Yi.

module Yi.Debug ( initDebug, trace, traceM, traceM_, logPutStrLn
                , logError, logStream, Yi.Debug.error ) where

import Control.Concurrent
    ( dupChan, getChanContents, forkIO, myThreadId, Chan )
import Control.Monad.Base ( liftBase, MonadBase )
import Data.IORef ( readIORef, writeIORef, IORef, newIORef )
import Data.Monoid ( (<>) )
import qualified Data.Text as T ( pack, snoc, unpack, Text )
import GHC.Conc ( labelThread )
import System.IO
    ( hFlush, hPutStrLn, IOMode(WriteMode), openFile, Handle )
import System.IO.Unsafe ( unsafePerformIO )

#if __GLASGOW_HASKELL__ < 710
import Data.Time (formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale)
#else
import Data.Time (formatTime, getCurrentTime, defaultTimeLocale)
#endif

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
trace :: T.Text -> a -> a
trace s e = unsafePerformIO $ logPutStrLn s >> return e
{-# NOINLINE trace #-}

error :: T.Text -> a
error s = unsafePerformIO $ logPutStrLn s >> Prelude.error (T.unpack s)

logPutStrLn :: MonadBase IO m => T.Text -> m ()
logPutStrLn s = liftBase $
  readIORef dbgHandle >>= \case
    Nothing -> return ()
    Just h -> do
      time <-  getCurrentTime
      tId <- myThreadId
      let m = show tId ++ " " ++ T.unpack s
      hPutStrLn h $ formatTime defaultTimeLocale rfc822DateFormat' time ++ m
      hFlush h
  where
    -- A bug in rfc822DateFormat makes us use our own format string
    rfc822DateFormat' = "%a, %d %b %Y %H:%M:%S %Z"

logError :: MonadBase IO m => T.Text -> m ()
logError s = logPutStrLn $ "error: " <> s

logStream :: Show a => T.Text -> Chan a -> IO ()
logStream msg ch = do
  logPutStrLn $ "Logging stream " <> msg
  logThreadId <- forkIO $ logStreamThread msg ch
  labelThread logThreadId "LogStream"

logStreamThread :: Show a => T.Text -> Chan a -> IO ()
logStreamThread msg ch = do
  stream <- getChanContents =<< dupChan ch
  mapM_ logPutStrLn [ msg `T.snoc` '(' <> T.pack (show i) `T.snoc` ')'
                     <> T.pack (show event)
                    | (event, i) <- zip stream [(0::Int)..]
                    ]

-- | Traces @x@ and returns @y@.
traceM :: Monad m => T.Text -> a -> m a
traceM x y = trace x $ return y

-- | Like traceM, but returns ().
traceM_ :: Monad m => T.Text -> m ()
traceM_ x = traceM x ()
