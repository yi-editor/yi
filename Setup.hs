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
       { postConf = pC, preBuild = setConfigInfo }

pC :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode
pC args a b lbi = do
    fmap (const ()) $ (postConf defaultUserHooks) args a b lbi  -- call default function (necessary ?)
    getLibDir (compilerPath . compiler $ lbi) >>= \libdir ->
      writeFile ".libdir" libdir
    return $ ExitSuccess
  where getLibDir ghcPath = do 
          (_, out, _, pid) <- runInteractiveProcess ghcPath ["--print-libdir"]
                                                           Nothing Nothing
          libDir <- hGetLine out
          waitForProcess pid
          return libDir

setConfigInfo args _
    = readFile ".libdir" >>= \libdir ->
      return
      (Nothing,
       [("yi", emptyBuildInfo
         { options = [(GHC,[mkOpt ("GHC_LIBDIR",show libdir)])] })])
    where mkOpt (name,def) = "-D"++name++"="++def

-- Marc Weber: I don't like this patch because it's using the .libdir
-- file to store the libdir. There must be a better way but I don't
-- want to spend more time on this. 
