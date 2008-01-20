#!/usr/bin/env runhaskell
module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import System.Directory hiding (copyFile)
import System.FilePath
import System.IO
import Distribution.Simple.Utils (copyFileVerbose)
import Distribution.Verbosity (Verbosity)

main :: IO ()
main = defaultMainWithHooks defaultUserHooks
       { buildHook = bHook, instHook = install }

mkOpt :: (String, String) -> String
mkOpt (name,def) = "-D" ++ name ++ "=" ++ def

bHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
bHook pd lbi hooks flags = do
  let verbosity = buildVerbose flags
  let dataPref = mkDataDir pd lbi NoCopyDest
      pkgOpts = concat [ ["-package", showPackageId pkg] | pkg <- packageDeps lbi ]
      ghcOut = rawSystemProgramStdoutConf verbosity ghcProgram (withPrograms lbi)
  print dataPref
  libdr <- head . lines <$> ghcOut ["--print-libdir"]
  putStrLn $ "GHC libdir = " ++ show libdr
  let pbi = (Nothing,
       [("yi", emptyBuildInfo
         { options = [(GHC,[mkOpt ("GHC_LIBDIR",show libdr),
                            mkOpt ("YI_LIBDIR", show dataPref),
                            mkOpt ("YI_PKG_OPTS", show pkgOpts)])] })])
      pd' = updatePackageDescription pbi pd
  buildHook defaultUserHooks pd' lbi hooks flags
  -- Copy compiled files to avoid duplicated precompilation
  curdir <- getCurrentDirectory
  let rel = map . makeRelative
      buildDir = curdir </> "dist" </> "build"
      yiBuildDir = buildDir </> "yi" </> "yi-tmp"
  compiledFiles <- rel yiBuildDir <$> unixFindExt (yiBuildDir </> "Yi") [".hi",".o"]
  mapM_ (copyFile verbosity yiBuildDir buildDir) compiledFiles
  mapM_ (precompile pd' lbi verbosity flags) precompiles

dependencyName :: Dependency -> String
dependencyName (Dependency name _) = name

precompile :: PackageDescription -> LocalBuildInfo -> t -> BuildFlags -> ([String], [String]) -> IO ()
precompile pd lbi _ bflags (moduleNames, dependencies) = when ok $ do
  -- just pretend that we build a library with the given modules
  putStrLn ("Precompiling " ++ show moduleNames)
  let [Executable "yi" _ yiBuildInfo] = executables pd
      pd' = pd {package = PackageIdentifier "main" (Version [] []),
                          -- we pretend we build package main, so that GHCi
                          -- can associate the source files and the precompiled modules
                executables = [],
                library = Just (Library {exposedModules = moduleNames,
                                         libBuildInfo = yiBuildInfo})}
  buildHook defaultUserHooks pd' lbi defaultUserHooks bflags -- {buildVerbose = deafening }
     where availablePackages = map dependencyName $ buildDepends pd
           ok = all (`elem` availablePackages) dependencies

precompiles :: [([String], [String])]
precompiles = [(["Yi.Main",
                 "Yi.Keymap.Normal",
                 "Yi.Keymap.Emacs",
                 "Yi.Keymap.Vim",
                 "Yi.Dired"], [])]

install :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
install pd lbi hooks flags = do
  curdir <- getCurrentDirectory
  let rel = map . makeRelative
      buildDir = curdir </> "dist" </> "build"
  sourceFiles <- rel curdir   <$> unixFindExt               "Yi"  [".hs-boot",".hs",".hsinc"]
  targetFiles <- rel buildDir <$> unixFindExt (buildDir </> "Yi") [".hs", ".hi",".o"]
  print targetFiles
  let InstallDirs {datadir = dataPref} = absoluteInstallDirs pd lbi NoCopyDest
      verbosity = installVerbose flags
  -- NOTE: It's important that source files are copied before target files,
  -- otherwise GHC (via Yi) thinks it has to recompile them when Yi is started.

  mapM_ (copyFile verbosity curdir dataPref) sourceFiles
  mapM_ (copyFile verbosity buildDir dataPref) targetFiles
  instHook defaultUserHooks pd lbi hooks flags


copyFile :: Verbosity -> FilePath -> FilePath -> FilePath -> IO ()
copyFile verbosity srcDir dstDir file = do
                         let destination = dstDir </> file
                         createDirectoryIfMissing True (dropFileName destination)
                         copyFileVerbose verbosity (srcDir </> file) destination


unixFindExt :: FilePath -> [String] -> IO [FilePath]
unixFindExt dir exts = filter ((`elem` exts) . takeExtension) <$> unixFind dir

unixFind :: FilePath -> IO [FilePath]
unixFind dir = do
  contents0 <- getDirectoryContents dir
  let contents = map (dir </>) $ filter (not . (`elem` [".", ".."])) contents0
  dirs <- filterM doesDirectoryExist contents
  files <- filterM doesFileExist contents
  rec <- mapM unixFind dirs
  return (files ++ concat rec)
