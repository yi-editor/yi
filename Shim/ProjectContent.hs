module Shim.ProjectContent
         ( loadFile
         , ProjectItem(..)
         , FileKind(..)
         ) where

import Control.Monad
import Control.Monad.State
import Data.Tree
import Data.Tree.Zipper
import Data.List (partition, nub)
import Distribution.Version
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Configure
import Distribution.Simple.Utils(dotToSep)
import Distribution.Simple.PreProcess(knownSuffixHandlers)
import System.FilePath
import System.Directory

data ProjectItem
  = ProjectItem
      { itemName   :: !String
      , itemFKind  :: !FileKind
      , itemVersion:: !Version
      }
  | DependenciesItem 
      { itemName   :: !String
      }
  | FolderItem 
      { itemName   :: !String
      , itemFKind  :: !FileKind
      }
  | FileItem
      { itemName   :: !String
      , itemFKind  :: !FileKind
      , itemFPath  :: FilePath
      }
  | PackageItem
      { itemName   :: !String
      , itemVersion:: !Version
      }
  deriving (Eq, Ord, Show)

data FileKind
  = ExposedModule
  | HiddenModule
  | CSource
  | HSource
  | TextFile
  | SetupScript
  | LicenseText
  | HsSourceFolder
  | PlainFolder
  deriving (Eq, Ord, Show)

loadFile :: FilePath -> IO (Tree ProjectItem)
loadFile projPath = do
  lbi <- fmap read $ readFile (projPath </> localBuildInfoFile)
  let pkgDescr = localPkgDescr lbi

      root  = Node (ProjectItem (pkgName (package pkgDescr)) PlainFolder (pkgVersion (package pkgDescr))) []
      tloc1 = execState (addDependenciesTree (packageDeps lbi)) (getTop root)
  tloc2 <- case library pkgDescr of
             Just lib -> addLibraryTree projPath tloc1 lib
             Nothing  -> return tloc1
  tloc2 <- foldM (addExecutableTree projPath) tloc2 (executables pkgDescr)
  tloc3 <- checkAndAddFile projPath "Setup.hs"  SetupScript tloc2
  tloc4 <- checkAndAddFile projPath "Setup.lhs" SetupScript tloc3
  let (hsources,extra_sources) = partition (\fpath -> takeExtension fpath == ".h") (extraSrcFiles pkgDescr)
      tloc5 = execState (do mapM_ (addFilePath HSource  projPath)  hsources
                            mapM_ (addFilePath TextFile projPath) extra_sources
                            mapM_ (addFilePath TextFile projPath) (dataFiles pkgDescr)
                            addFilePath LicenseText projPath (licenseFile pkgDescr)) tloc4
  return (tree tloc5)

addLibraryTree projPath tloc (Library {libBuildInfo=binfo, exposedModules=exp_mods}) = do
  (exp_mods,hid_mods,tloc1) <- foldM (\st dir -> addSourceDir projPath dir st)
                                     (exp_mods,otherModules binfo,tloc)
                                     (hsSourceDirs binfo)
  return $ execState (mapM_ (addFilePath CSource projPath) (cSources binfo)) tloc1
  
addExecutableTree projPath tloc (Executable {modulePath=mainIs, buildInfo=binfo}) = do
  let tloc1 = execState (addFilePath ExposedModule projPath mainIs) tloc
  (exp_mods,hid_mods,tloc2) <- foldM (\st dir -> addSourceDir projPath dir st)
                                     ([],otherModules binfo,tloc1)
                                     (hsSourceDirs binfo)
  return $ execState (mapM_ (addFilePath CSource projPath) (cSources binfo)) tloc2

addDependenciesTree deps = do
  insertDown (DependenciesItem "Dependencies")
  mapM_ addDependency deps
  up
  where
    addDependency dep = do
      insertDown (PackageItem (pkgName dep) (pkgVersion dep)) >> up

addSourceDir :: FilePath -- ^ project location
             -> FilePath -- ^ source sub-directory
             ->    ([String],[String],TreeLoc ProjectItem)
             -> IO ([String],[String],TreeLoc ProjectItem)
addSourceDir projPath srcDir (exp_mods,hid_mods,tloc) = do
  let dir = projPath </> srcDir
  (exp_paths,exp_mods) <- findModules dir exp_mods
  (hid_paths,hid_mods) <- findModules dir hid_mods
  let tloc1 = execState (addFilePath' (\c -> FolderItem c HsSourceFolder) (splitPath' srcDir)
                            (mapM_ (addFilePath ExposedModule dir) exp_paths >>
                             mapM_ (addFilePath HiddenModule  dir) hid_paths))
                        tloc
  return (exp_mods,hid_mods,tloc1)

addFilePath :: FileKind -> FilePath -> FilePath -> State (TreeLoc ProjectItem) ()
addFilePath kind root fpath = addFilePath' (\c -> FileItem c kind (root </> fpath)) (splitPath' fpath) (return ())

addFilePath' :: (String -> ProjectItem) -> [String] 
             -> State (TreeLoc ProjectItem) ()
             -> State (TreeLoc ProjectItem) ()
addFilePath' mkItem []     cont = cont
addFilePath' mkItem (c:cs) cont
  | c == "."  = addFilePath' mkItem cs cont
  | otherwise = do let item | null cs   = mkItem c
                            | otherwise = FolderItem c PlainFolder
                   children <- gets hasChildren
                   if children
                     then firstChild >> insertItem c item
                     else insertDown item
                   addFilePath' mkItem cs cont
                   up >> return ()
  where
    insertItem c item = do
      item' <- getLabel
      case compare item item' of
        LT -> insertLeft item
        EQ -> return ()
        GT -> do last <- gets isLast
                 if last
                   then insertRight item
                   else right >> insertItem c item


splitPath' fpath = [removeSlash c | c <- splitPath fpath]
  where
    removeSlash c
      | null c                   = c
      | isPathSeparator (last c) = init c
      | otherwise                = c

checkAndAddFile projPath fpath kind tloc = do
  let fullPath = projPath </> fpath
  exists <- doesFileExist fullPath
  if exists
    then return $ execState (addFilePath kind fullPath fpath) tloc
    else return tloc

-------------------------------------------------------------------------
-- Module Finder
-------------------------------------------------------------------------

findModules :: FilePath                  -- ^project location
            -> [String]                  -- ^module names
            -> IO ([FilePath],[String])  -- ^locations and unknown modules
findModules location []         = return ([],[])
findModules location (mod:mods) = do
  mb_paths <- findFileWithExtension' (map fst knownSuffixHandlers ++ ["hs", "lhs"]) [location] (dotToSep mod)
  (locs,unks) <- findModules location mods
  case mb_paths of
    Just (_,loc) -> return (loc:locs,unks)
    Nothing      -> return (locs,mod:unks)


-- FIXME: The bellow two functions are copy+paste from the latest version of
-- Cabal. Unfortunatelly they aren't exported in the current version of Cabal.
-- Fix that after the next Cabal release.

findFileWithExtension' :: [String]
                       -> [FilePath]
                       -> FilePath
                       -> IO (Maybe (FilePath, FilePath))
findFileWithExtension' extensions searchPath baseName =
  findFirstFile (uncurry (</>))
    [ (path, baseName <.> ext)
    | path <- nub searchPath
    , ext <- nub extensions ]

findFirstFile :: (a -> FilePath) -> [a] -> IO (Maybe a)
findFirstFile file = findFirst
  where findFirst []     = return Nothing
        findFirst (x:xs) = do exists <- doesFileExist (file x)
                              if exists
                                then return (Just x)
                                else findFirst xs
