#!/usr/bin/env runhaskell
module Main where

import Control.Monad(when, filterM, unless)
import Data.List (intersect)
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

  res <- mapM (precompile ghc) precompiles
  let sucessfuls = [m | (m,code) <- res, code == ExitSuccess]
      nok = null $ intersect sucessfuls ["Yi.Vty.UI", "Yi.Gtk.UI"]
  putStrLn $ "Sucessfully compiled: " ++ show sucessfuls
  when nok $ do
       putStrLn "No frontend was compiled sucessfully. Giving up."
       exitWith (ExitFailure 1)

precompile ghc (moduleName, dependencies) = do
  putStrLn ("Precompiling " ++ moduleName)
  exitCode <- rawSystemVerbose 0 ghc (precompArgs ++ map ("-package "++) dependencies ++ [moduleName])
  when (exitCode /= ExitSuccess) $
       putStrLn $ "Precompiling failed: " ++ moduleName
  return (moduleName, exitCode)
  
precompiles = [("Yi.Keymap.Emacs", []),
               ("Yi.Keymap.Vim", []),
               ("Yi.Vty.UI", ["vty"]),
               ("Yi.Gtk.UI", ["gtk", "sourceview"]),
               ("Yi.Dired", ["unix"])]

precompArgs = ["-DGHC_LIBDIR=\"dummy1\"", 
               "-DYI_LIBDIR=\"dummy2\"", 
               "-fglasgow-exts", 
               "-package ghc",
               "-package filepath",
               "-cpp",
               "-Wall",
               "-hide-all-packages", -- otherwise wrong versions of packages will be picked.
               "-package base",
               "-package mtl",
               "-package regex-posix-0.71",
               "--make"]
               -- please note: These args must match the args given in Yi.Boot
               -- TODO: factorize.

install :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> InstallFlags -> IO ()
install pd lbi hooks bfs = do
  curdir <- getCurrentDirectory
  allFiles0 <- mapM unixFind $ map (curdir </>) $ ["Yi"]
  let allFiles = map (makeRelative curdir) $ concat allFiles0
      sourceFiles = filter ((`elem` [".hs-boot",".hs",".hsinc"]) . takeExtension) allFiles      
      targetFiles = filter ((`elem` [".hi",".o"]) . takeExtension) allFiles
      --NOTE: It's important that source files are copied before target files,
      -- otherwise GHC (via Yi) thinks it has to recompile them.
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
