{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module InteroAPI (Request, Intero (Intero), start, locAt, uses, typeAt, eval) where

import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent
import System.IO
import System.Process
import Control.Monad

-- TODO: remove the duplication
data Request
  = Uses   FilePath (Int,Int,Int,Int) String (MVar String)
  | TypeAt FilePath (Int,Int,Int,Int) String (MVar String)
  | LocAt  FilePath (Int,Int,Int,Int) String (MVar String)
  | Eval   String                            (MVar String)

type Repl = (String -> IO String)
newtype Intero = Intero (Maybe (MVar Request))

instance Show (MVar Request) where
  show _ = "Intero"

query :: Request -> String
query (Uses   file (line,col,line',col') name _) =
  concat [":uses "   , file, " ", unwords (map show [line,col,line',col']), " ", name]
query (TypeAt file (line,col,line',col') name _) =
  concat [":type-at ", file, " ", unwords (map show [line,col,line',col']), " ", name]
query (LocAt  file (line,col,line',col') name _) =
  concat [":loc-at " , file, " ", unwords (map show [line,col,line',col']), " ", name]
query (Eval q _) = q

handleReq :: Repl -> Request -> IO ()
handleReq repl req@(Uses   _ _ _ res) = putMVar res =<< id (repl (query req))
handleReq repl req@(TypeAt _ _ _ res) = putMVar res =<< id (repl (query req))
handleReq repl req@(LocAt  _ _ _ res) = putMVar res =<< id (repl (query req))
handleReq repl req@(Eval   _     res) = putMVar res =<< id (repl (query req))

-- Code taken from:
-- https://github.com/commercialhaskell/intero/blob/28609611c9f7c7d63370ce66e8ebb97676a8374e/src/test/Main.hs#L219-L268
start :: FilePath -> IO (MVar Request)
start path = do
  req <- newEmptyMVar
  (inp,out,err,pid) <-
    catch (runInteractiveProcess
             "stack"
             ["ghci","--with-ghc","intero"]
             (Just path)
             Nothing)
          (\(_ :: IOException) ->
             error "Couldn't launch intero process.")
  hSetBuffering inp NoBuffering
  hSetBuffering out NoBuffering
  hSetBuffering err NoBuffering
  let repl instr = do
        catch (do
                 hPutStrLn inp instr
                 let getReply = do
                       mc <- catch (fmap Just (hGetChar out))
                                   (\(_ :: IOException) ->
                                      return Nothing)
                       case mc of
                         Nothing -> hGetAvailable err
                         Just '\4' -> hGetAvailable err
                         Just c -> fmap (c:) getReply
                 getReply)
              (\(_ :: IOException) -> return "")
  void $ repl ":set prompt \"\\4\""
  forkIO $ forever $ takeMVar req >>= handleReq repl
  return req
  where
    hGetAvailable h = do
      available <- catch (hReady h) (\(_ :: IOException) -> return False)
      if available
        then catch (do
                      c <- hGetChar h
                      cs <- hGetAvailable h
                      return (c:cs))
                   (\(_ :: IOException) -> return [])
        else return []

uses :: MVar Request -> FilePath -> (Int,Int,Int,Int) -> String -> IO String
uses req file range name = do
  result <- newEmptyMVar
  putMVar req (Uses file range name result)
  takeMVar result

typeAt :: MVar Request -> FilePath -> (Int,Int,Int,Int) -> String -> IO String
typeAt req file range name = do
  result <- newEmptyMVar
  putMVar req (TypeAt file range name result)
  takeMVar result

locAt :: MVar Request -> FilePath -> (Int,Int,Int,Int) -> String -> IO String
locAt req file range name = do
  result <- newEmptyMVar
  putMVar req (LocAt file range name result)
  takeMVar result

eval :: MVar Request -> String -> IO String
eval req q = do
  result <- newEmptyMVar
  putMVar req (Eval q result)
  takeMVar result
