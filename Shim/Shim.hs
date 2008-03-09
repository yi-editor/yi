{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -Wall #-}

-- Copyright (C) 2006 Benedikt Schmidt
-- see LICENSE.BSD3 for license

module Shim.Shim where

import Panic

import System.IO
import Prelude hiding ( catch )
import Control.Applicative
import Control.Monad.State
import Control.Exception ( catch )
import Data.List ( intersperse )
import Data.Maybe
import System.Exit

import Shim.Sexp
import Shim.SHM
import Shim.Messages
import Shim.MessagesTH
import Shim.Utils
import Shim.Comms
import Shim.Hsinfo

startServer :: String -> String -> IO ExitCode
startServer socketfile ghcProg = do
  logS "shim started"
  withServerConnection socketfile (\h -> do
     logS "accepted connection"
     sess <- ghcInit ghcProg
     runSHM sess ghcProg (cmdLoop h))
 `catch` (\e -> do hPutStrLn stderr $ "shim failed with: "++(showException e)
                   return $ ExitFailure 1)

cmdLoop :: Handle -> SHM ExitCode
cmdLoop shimh = do
  s <- io $ getNetstring shimh
  logInfo $ "received:\n``" ++ shorten 160 s ++ "``"
  case parseS s of
    Left err -> do logInfo $ "Error parsing request: " ++ (show err)
                   return $ ExitFailure 3
    Right sexp -> do mres <- handleEvent sexp
                     case mres of
                       Just res ->
                         do logInfo $ "sending:\n" ++ res
                            io $ hPutStr shimh res >> hFlush shimh
                       Nothing -> logInfo "sending nothing"
                     cmdLoop shimh

handleEvent :: Sexp -> SHM (Maybe String)
handleEvent sexp =
  case sexp of
    SList ((SSymbol ":emacs-rex"):(SInt rexid):[call]) ->
      do val0 <- shmCatch (handleCall call)
                  (\e -> return $ Error $ "exception: "++(showException e))
         case val0 of 
           Response val ->
             return $ Just $ encode $ Se (S ":return", (S ":ok", val) , rexid)
           Error msg ->
             return $ Just $ encode $ Se (S ":return", (S ":abort", msg), rexid)
    SList ((SSymbol ":vim-rex"):[call]) -> handleVimCall call
    _ ->
      do logInfo $ "unknown request" ++ (show sexp)
         return Nothing

handleVimCall :: Sexp -> SHM (Maybe String)
handleVimCall call
  | Just (S "fuzzy-complete-identifier", filename, pref) <- fromS call =
      do l <- findIdPrefix filename pref
         return . Just . netEncode $ formatCompletions pref l
  | otherwise = do logInfo $ "unknown request" ++ (show call)
                   return Nothing

formatCompletions :: String -> [(String,String)] -> String
formatCompletions pref comps = "[" ++ (concat . intersperse "," $ s) ++ "]"
 where s = map (\(n,ty) -> "{\"abbr\":\""++n++"\","++
                              "\"word\":\""++dropPrefix pref n++"\","++
                              "\"menu\":\""++ty++"\"}")
               comps


{-
type family TupleToSexp a
--type instance TupleToSexp (a) = (Symbol,a)
type instance TupleToSexp (a,b) = (Symbol,a,b)
type instance TupleToSexp (a,b,c) = (Symbol,a,b,c)
-}

handleCall :: Sexp -> SHM (Response Se)
handleCall call = fromMaybe failure (msum$ map (\(Pack msg) -> f msg) messages) where
 f :: forall args res . (ConvSexp args, ConvSexp res) => Message args res -> Maybe (SHM (Response Se))
 f (Message n callb)
    | Just ((SSymbol msgName):argsexp) <- fromS call, msgName == n,
      Just args <- fromS $ SList argsexp
    = Just (fmap Se <$> callb args)
    | otherwise = Nothing 
 failure = do logInfo $ "received unknown command: " ++ (pp call)
              return  $  Error "unknown command"

encode :: Se -> String
encode = netEncode . pp . toS
