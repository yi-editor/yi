{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module     : InteroAPI
-- Copyright  : 2017, Jaro Reinders
-- License    : GPL-2
-- Maintainer : yi-devel@googlegroups.com
--
-- A Haskell API to intero. This is not Yi-specific.

module InteroAPI (
    Request
  , start
  , locAt, uses, typeAt, eval
  ) where

import Control.Exception  (IOException, catch, handle)
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar, forkIO)
import System.IO          (hSetBuffering, hGetChar, BufferMode (NoBuffering), hPutStrLn, hReady)
import System.Process     (runInteractiveProcess)
import Control.Monad      (void, forever)

-- | The request that are send to the background process.
data Request = Request
  String        -- ^ The content of the query.
  (MVar String) -- ^ The result will be put into this MVar.

-- Code taken from:
-- https://github.com/commercialhaskell/intero/blob/28609611c9f7c7d63370ce66e8ebb97676a8374e/src/test/Main.hs#L219-L268
-- Modified to keep the intero process running in the background
-- and accept commands via an MVar.
start :: FilePath -> IO (MVar Request)
start path = do
  req <- newEmptyMVar
  (inp,out,err,pid) <- catch
    (runInteractiveProcess "stack" ["ghci","--with-ghc","intero"] (Just path) Nothing)
    (\(_ :: IOException) -> error "Couldn't launch intero process.")
  hSetBuffering inp NoBuffering
  hSetBuffering out NoBuffering
  hSetBuffering err NoBuffering
  let repl instr = handle (\(e :: IOException) -> return (show e)) $ do
        hPutStrLn inp instr
        let getReply = do
              mc <- catch (fmap Just (hGetChar out))
                          (\(_ :: IOException) -> return Nothing)
              case mc of
                Nothing -> hGetAvailable err
                Just '\4' -> hGetAvailable err
                Just c -> fmap (c:) getReply
        getReply
  void $ repl ":set prompt \"\\4\""
  forkIO $ forever $ do
    Request query res <- takeMVar req
    repl query >>= putMVar res
  return req
  where
    hGetAvailable h = do
      available <- catch (hReady h) (\(_ :: IOException) -> return False)
      if available
        then catch ((:) <$> hGetChar h <*> hGetAvailable h)
                   (\(_ :: IOException) -> return [])
        else return []

uses :: MVar Request -> FilePath -> (Int,Int,Int,Int) -> String -> IO String
uses req file (line,col,line',col') name = eval req $ unwords $ concat
  [[":uses",file], map show [line,col,line',col'], [name]]

typeAt :: MVar Request -> FilePath -> (Int,Int,Int,Int) -> String -> IO String
typeAt req file (line,col,line',col') name = eval req $ unwords $ concat
  [[":type-at",file], map show [line,col,line',col'], [name]]

locAt :: MVar Request -> FilePath -> (Int,Int,Int,Int) -> String -> IO String
locAt req file (line,col,line',col') name = eval req $ unwords $ concat
  [[":loc-at",file], map show [line,col,line',col'], [name]]

eval :: MVar Request -> String -> IO String
eval req q = do
  result <- newEmptyMVar
  putMVar req (Request q result)
  takeMVar result
