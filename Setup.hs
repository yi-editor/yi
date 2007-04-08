#!/usr/bin/env runhaskell
module Main where

import Distribution.Simple
import Distribution.PackageDescription
import System.Info
import System.Process
import Data.List
import System.IO

main :: IO ()
main = defaultMainWithHooks defaultUserHooks
       { preBuild = setConfigInfo }

setConfigInfo args _
    = getLibDir args >>= \libdir ->
      return
      (Nothing,
       [("yi", emptyBuildInfo
         { options = [(GHC,[mkOpt ("GHC_LIBDIR",show libdir)])] })])
    where mkOpt (name,def) = "-D"++name++"="++def

getLibDir [arg]
    | "--with-ghc" `isPrefixOf` arg
        = do (_, out, _, pid) <- runInteractiveProcess ghcPath ["--print-libdir"]
                                                       Nothing Nothing
             libDir <- hGetLine out
             waitForProcess pid
             return libDir
    where ghcPath = drop 1 (dropWhile (/='=') arg)

getLibDir _ = error "failed to extract ghc path from command line: use --with-ghc=<GHC_PATH>"
