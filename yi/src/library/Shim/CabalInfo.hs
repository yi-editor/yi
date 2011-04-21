module Shim.CabalInfo where

import Shim.Utils

import qualified Control.OldException as CE
import System.FilePath
import Control.Monad.State
import Data.Maybe

import Control.Applicative
import Distribution.ModuleName
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as Library (Library(..))
import qualified Distribution.PackageDescription as BuildInfo (BuildInfo(..))
import System.Directory
import System.FriendlyPath

guessCabalFile :: String -> IO (Maybe FilePath)
guessCabalFile sourcefile = do
  let dir = takeDirectory $ dropFileName sourcefile
  recurseDir findCabalFile dir  -- "/bar/foo/s.hs" -> "/bar/foo"
 where findCabalFile dir = do
         logS $ "looking in: " ++ dir
         pdfile <- CE.try (findPackageDesc dir) :: IO (Either CE.Exception (Maybe FilePath))
         case pdfile of
           Right (Just f) -> return . Just $ dir </> f
           _ -> return Nothing

-- | Guess what lib\/exe the sourcefile belongs to.
guessCabalStanza :: FilePath -> FilePath -> PackageDescription -> IO (Maybe String, BuildInfo)
guessCabalStanza projpath sourcefile pkg_descr = do
  matchingStanzas <- filterM matchingStanza allStanzas'
  let ((name, _, bi):_) = matchingStanzas ++ allStanzas'
  return (name, bi)
  where allStanzas =
            [ (Nothing, concatMap moduleFiles (Library.exposedModules lib) , libBuildInfo lib)
             | Just lib <- [library pkg_descr] ]
         ++ [ (Just (exeName exe), [modulePath exe], buildInfo exe)
             | exe <- executables pkg_descr ]
        moduleFiles modl = [toFilePath modl <.> ext | ext <- ["hs", "lhs"] ]
        allStanzas' = [(name, [projpath </> dir </> file | dir <- hsSourceDirs bi, file <- files ++ concatMap moduleFiles (BuildInfo.otherModules bi)], bi)
                       | (name, files, bi) <- allStanzas, buildable bi]
        eqPath p1 p2 = equalFilePath <$> canonicalizePathFix p1 <*> canonicalizePathFix p2
        matchingStanza (_,files,_) = or <$> mapM (eqPath sourcefile) files


-- Taken from Cabal and modified so that nothing is printed to stdout.
-- TODO: Perhaps export something better from Cabal

-- |Find a package description file in the given directory.  Looks for
-- @.cabal@ files.
findPackageDesc :: FilePath            -- ^Where to look
                -> IO (Maybe FilePath) -- ^<pkgname>.cabal
findPackageDesc dir
 = do files <- getDirectoryContents dir
      -- to make sure we do not mistake a ~/.cabal/ dir for a <pkgname>.cabal
      -- file we filter to exclude dirs and null base file names:
      cabalFiles <- filterM doesFileExist
                       [ dir </> file
                       | file <- files
                       , let (name, ext) = splitExtension file
                       , not (null name) && ext == ".cabal" ]
      case cabalFiles of
        []          -> return Nothing
        [cabalFile] -> return (Just cabalFile)
        _    -> return Nothing
