#!/usr/bin/env runhaskell
module Main where

import Control.Monad(when, filterM, unless)
import Data.List
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Setup
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import System.Exit
import System.IO
import System.Info
import System.Process

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
  let ghc = compilerPath . compiler $ lbi
  libdir <- getLibDir ghc
  let pbi = (Nothing,
       [("yi", emptyBuildInfo
         { options = [(GHC,[mkOpt ("GHC_LIBDIR",show libdir)])] })])
      pd' = updatePackageDescription pbi pd
  buildHook defaultUserHooks pd' lbi hooks bfs

  let preCompile flavour = do
         putStrLn ("Precompiling " ++ flavour)
         exitCode <- rawSystemVerbose 0 ghc $ precompArgs flavour
         when (exitCode /= ExitSuccess) $
              putStrLn $ "Precompile of " ++ flavour ++ " failed. Install the corresponding library if needed."
         return exitCode
  exitCodes <- mapM preCompile ["vty", "gtk"]
  let exitCode = foldr1 optimistic exitCodes
  unless (exitCode == ExitSuccess) $ exitWith exitCode

optimistic ExitSuccess x = ExitSuccess
optimistic x ExitSuccess = ExitSuccess      
optimistic (ExitFailure x) (ExitFailure y) = ExitFailure (min x y)

precompArgs flavour = ["-DGHC_LIBDIR=\"dummy\"", 
                       "-i" ++ flavour,
                       "-odir " ++ flavour,
                       "-hidir "++flavour,
                       "-fglasgow-exts", 
                       "-package ghc",
                       "-cpp",
                       "--make","Static.hs"]
                      -- please note: These args must match the args given in Yi.Boot
                      -- TODO: factorize.