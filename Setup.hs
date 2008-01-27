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
       { buildHook = bHook, instHook = install, haddockHook = hdHook, sDistHook = sDist }

bHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
bHook pd lbi hooks flags = do
  pd' <- addPackageOptions pd lbi (buildVerbose flags)
  buildHook defaultUserHooks pd' lbi hooks flags

hdHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO ()
hdHook pd lbi hooks flags = do
  pd' <- addPackageOptions pd lbi (haddockVerbose flags)
  let pd'' = pseudoLibraryPkg pd' "yi" ["Yi.Yi"]
  haddockHook defaultUserHooks pd'' lbi hooks flags

install :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
install pd lbi hooks flags = do
  curdir <- getCurrentDirectory
  let rel = map . makeRelative
      buildDir = curdir </> "dist" </> "build" </> "yi" </> "yi-tmp"
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


sDist :: PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> SDistFlags -> IO ()
sDist pd lbi hooks flags = do
  curdir <- getCurrentDirectory
  sources <- (map . makeRelative) curdir <$> unixFindExt (curdir </> "Yi") [".hs",".hsinc",".x"]
  -- Make run-inplace injects some files into the source directory,
  -- we need to make sure not to include those...
  let ss = [s | s <- sources, not (".hs" `isSuffixOf` s) ||
                              "Syntax.hs" `isSuffixOf` s ||
                              "Table.hs" `isSuffixOf` s ||
                              not ("Syntax" `isInfixOf` s)]
  -- Ugly hack that adds all the files we need as dataFiles, since
  -- Cabal seriously don't want to play our game...
  let pd' = pd{
    dataFiles = "Main.hs" : ss ++ dataFiles pd, executables = [], library=Nothing }
  
  -- Run the standard hook for our cripled package
  sDistHook defaultUserHooks pd' lbi hooks flags
  

mkOpt :: (String, String) -> String
mkOpt (name,def) = "-D" ++ name ++ "=" ++ def

-- Add our special package options to
addPackageOptions :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO PackageDescription
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
