module Shim.CabalInfo where

import Shim.Utils

import qualified Control.Exception as CE
import System.FilePath ( takeDirectory, (</>), (<.>), dropFileName, equalFilePath )
import Control.Monad.State
import Data.Maybe

import Control.Applicative
import Distribution.Simple.Utils
import Distribution.ModuleName
import Distribution.PackageDescription 
import qualified Distribution.PackageDescription as Library (Library(..))
import qualified Distribution.PackageDescription as BuildInfo (BuildInfo(..))
import System.Directory (canonicalizePath)

guessCabalFile :: String -> IO (Maybe FilePath)
guessCabalFile sourcefile = do
  let dir = takeDirectory $ dropFileName sourcefile 
  recurseDir findCabalFile dir  -- "/bar/foo/s.hs" -> "/bar/foo"
 where findCabalFile dir = do
         logS $ "looking in: " ++ dir
         pdfile <- CE.try (findPackageDesc dir)
         case pdfile of
           Right f -> return . Just $ dir </> f
           Left _ -> return Nothing

-- | Guess what lib/exe the sourcefile belongs to.
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
        moduleFiles mod = [toFilePath mod <.> ext | ext <- ["hs", "lhs"] ]
        allStanzas' = [(name, [projpath </> dir </> file | dir <- hsSourceDirs bi, file <- files ++ concatMap moduleFiles (BuildInfo.otherModules bi)], bi)
                       | (name, files, bi) <- allStanzas, buildable bi]
        eqPath p1 p2 = equalFilePath <$> canonicalizePath p1 <*> canonicalizePath p2
        matchingStanza (_,files,_) = or <$> mapM (eqPath sourcefile) files
