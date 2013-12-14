{-# LANGUAGE CPP #-}
-- Copyright (C) 2006 Benedikt Schmidt
-- see LICENSE.BSD3 for license

module Shim.Udproxy where

import Control.Concurrent
import Network
import System.IO
import System
import Control.Monad

main :: IO ()
main = withSocketsDo$ do
  args <- getArgs
  case args of
    [socketfile] -> do
      h <- connectServer socketfile -- portStr in win32
      hSetBuffering stdout NoBuffering
      hSetBuffering h LineBuffering
      forkIO(stdinToSocket h)
      stdoutFromSocket h
    _ -> do
      usage
      exitFailure

usage :: IO ()
usage = putStrLn "shim-udproxy socketfile"

stdinToSocket :: Handle -> IO ()
stdinToSocket h = getContents >>= hPutStr h

stdoutFromSocket :: Handle -> IO ()
stdoutFromSocket h = do
  --hPutStrLn stderr $ "waiting: "
  c <- hGetChar h
  --hPutStrLn stderr $ "received: " ++ [c]
  putStr [c]
  stdoutFromSocket h

#ifdef mingw32_HOST_OS
connectServer :: String -> IO Handle
connectServer portStr = connectTo "127.0.0.1"
                         (PortNumber (fromIntegral (read portStr :: Integer)))
#else
connectServer :: String -> IO Handle
connectServer socketfile = connectTo "127.0.0.1" (UnixSocket socketfile)
#endif
