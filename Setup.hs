#!/usr/bin/env runhaskell
module Main where

import Distribution.Simple
import Distribution.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.Info
import System.Process
import Data.List
import System.IO
import System.Exit
import Data.Maybe

main :: IO ()
main = defaultMainWithHooks defaultUserHooks
       { buildHook = bHook }

getLibDir ghcPath = do 
          (_, out, _, pid) <- runInteractiveProcess ghcPath ["--print-libdir"]
                                                           Nothing Nothing
          libDir <- hGetLine out
          waitForProcess pid
          return libDir

mkOpt (name,def) = "-D"++name++"="++def

bHook :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> BuildFlags -> IO ()
bHook pd lbi hooks bfs = do
  libdir <- getLibDir (compilerPath . compiler $ lbi)
  let pbi = (Nothing,
       [("yi", emptyBuildInfo
         { options = [(GHC,[mkOpt ("GHC_LIBDIR",show libdir)])] })])
      pd' = updatePackageDescription pbi pd
  buildHook defaultUserHooks pd' lbi hooks bfs

    