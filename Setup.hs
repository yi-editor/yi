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
       { buildHook = bHook, instHook = install, haddockHook = hdHook }

bHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
bHook pd lbi hooks flags = do
  pd' <- addPackageOptions pd lbi (buildVerbose flags)
  -- Compile main executable
  buildHook defaultUserHooks pd' lbi hooks flags
  -- Copy compiled files to avoid duplicated precompilation
  -- Attn: we cannot do this after all because it does not copy the preprocessed files.
  -- A pain.
  curdir <- getCurrentDirectory
  let rel = map . makeRelative
      buildDir = curdir </> "dist" </> "build"
      yiBuildDir = buildDir </> "yi" </> "yi-tmp"
  compiledFiles <- rel yiBuildDir <$> unixFindExt (yiBuildDir </> "Yi") [".hi",".o"]
  -- mapM_ (copyFile (buildVerbose flags) yiBuildDir buildDir) compiledFiles

  -- Precompile loadable modules, by compiling a pseudo package
  -- we pretend we build package main, so that GHCi
  -- can associate the source files and the precompiled modules
  let pd'' = pseudoLibraryPkg pd' "main" ["Yi.Main", "Yi.Dired", "Yi.Keymap.Vim", "Yi.Keymap.Emacs"]
  buildHook defaultUserHooks pd'' lbi defaultUserHooks flags

hdHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO ()
hdHook pd lbi hooks flags = do
  pd' <- addPackageOptions pd lbi (haddockVerbose flags)
  let pd'' = pseudoLibraryPkg pd' "yi" ["Yi.Yi"]
  (conf,_) <- requireProgram (haddockVerbose flags) haddockProgram (orLaterVersion (Version [0,6] [])) (withPrograms lbi)
  let Just version = programVersion conf
  let have_src_hyperlink_flags = version >= Version [0,8] []
  putStrLn $ "Haddock is to old?"
  when (True && not have_src_hyperlink_flags) $
    putStrLn $ "Haddock is to old... " ++ show (version)
  haddockHook defaultUserHooks pd'' lbi hooks flags

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


mkOpt :: (String, String) -> String
mkOpt (name,def) = "-D" ++ name ++ "=" ++ def

-- Add our special package options to
addPackageOptions pd lbi verbosity = do
  let dataPref = datadir $ absoluteInstallDirs pd lbi NoCopyDest
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
  return $ updatePackageDescription pbi pd

-- just pretend that we build a library with the given modules
pseudoLibraryPkg :: PackageDescription -> String -> [String] -> PackageDescription
pseudoLibraryPkg pd name mods =
  pd {package = PackageIdentifier name (Version [] []),
      executables = [],
      library = Just (Library {
        exposedModules = mods,
        libBuildInfo = yiBuildInfo})}
  where [Executable "yi" _ yiBuildInfo] = executables pd

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
