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
import System.Directory
import System.Exit
import System.IO
import System.Info
import System.Process
import System.FilePath

main :: IO ()
main = defaultMainWithHooks defaultUserHooks
       { buildHook = bHook, instHook = install }

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
  let dataPref = mkDataDir pd lbi NoCopyDest 
  print dataPref
  libdir <- getLibDir ghc
  let pbi = (Nothing,
       [("yi", emptyBuildInfo
         { options = [(GHC,[mkOpt ("GHC_LIBDIR",show libdir), mkOpt ("YI_LIBDIR", show dataPref)])] })])
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

precompArgs flavour = ["-DGHC_LIBDIR=\"dummy1\"", 
                       "-DYI_LIBDIR=\"dummy2\"", 
                       "-i" ++ flavour,
                       "-odir " ++ flavour,
                       "-hidir "++flavour,
                       "-fglasgow-exts", 
                       "-package ghc",
                       "-cpp",
                       "--make","Static.hs"]
                      -- please note: These args must match the args given in Yi.Boot
                      -- TODO: factorize.

install :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> InstallFlags -> IO ()
install pd lbi hooks bfs = do
  curdir <- getCurrentDirectory
  allFiles0 <- mapM unixFind $ map (curdir </>) $ ["vty", "gtk", "Yi"]
  let allFiles = map (makeRelative curdir) $ concat allFiles0
      sourceFiles = filter ((`elem` [".hs-boot",".hs"]) . takeExtension) allFiles      
      targetFiles = filter ((`elem` [".hi",".o"]) . takeExtension) allFiles
      --NOTE: It's important that source files are copied before target files,
      -- otherwise GHC think it has to recompile them.
      pd' = pd {dataFiles = dataFiles pd ++ sourceFiles ++ targetFiles}
  instHook defaultUserHooks pd' lbi hooks bfs
  

unixFind dir = do
  contents0 <- getDirectoryContents dir
  let contents = map (dir </>) $ filter (not . (`elem` [".", ".."])) contents0
  -- putStrLn $ dir ++ " > " ++ show contents0
  dirs <- filterM doesDirectoryExist contents
  files <- filterM doesFileExist contents
  rec <- mapM unixFind dirs
  return (files ++ concat rec)