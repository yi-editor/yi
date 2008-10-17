--
-- Copyright (c) Krasimir Angelov 2008.
--
-- Extraction of the "Project View" from
-- already configured Cabal package.
--

module Shim.ProjectContent
         ( loadProject
         , itemName
         , ProjectItem(..)
         , FileKind(..)
         , FolderKind(..)
         , ModuleKind(..)
         ) where

import Control.Monad.State
import Data.Tree
import Data.Tree.Zipper
import qualified Data.Set as Set
import Data.List (partition, nub)
import Distribution.ModuleName
import Distribution.Version
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Configure
import Distribution.Simple.Utils(findFileWithExtension')
import Distribution.Simple.PreProcess(knownSuffixHandlers)
import Distribution.Simple.Setup (defaultDistPref)
import Distribution.Text
import System.FilePath
import System.Directory

data ProjectItem
  = ProjectItem
      { projItemName :: String
      , itemVersion :: Version
      }
  | DependenciesItem 
      { depItemName   :: String
      }
  | FolderItem 
      { folderItemName   :: String
      , folderKind :: FolderKind
      }
  | FileItem
      { fileItemName   :: String
      , itemFPath  :: FilePath
      , fileKind   :: FileKind
      }
  | PackageItem
      { pkgItemName   :: PackageName
      , itemVersion:: Version
      }
  | ModuleItem
      { modItemName   :: ModuleName
      , itemFPath  :: FilePath
      , moduleKind :: ModuleKind
      }
  deriving (Eq, Ord, Show)

itemName :: ProjectItem -> String
itemName ProjectItem {projItemName = n} = n
itemName DependenciesItem {depItemName = n} = n
itemName FolderItem {folderItemName = n} = n
itemName FileItem {fileItemName = n} = n
itemName PackageItem {pkgItemName = p} = display p
itemName ModuleItem {modItemName = m} = display m



data FileKind
  = HsSource ModuleKind
  | CSource
  | HSource
  | TextFile
  | SetupScript
  | LicenseText
  deriving (Eq, Ord, Show)

data FolderKind
  = HsSourceFolder
  | PlainFolder
  deriving (Eq, Ord, Show)

data ModuleKind
  = ExposedModule
  | HiddenModule
  deriving (Eq, Ord, Show)


loadProject :: FilePath -> IO (Tree ProjectItem, Tree ProjectItem)
loadProject projPath = do
  Right lbi <- tryGetConfigStateFile (projPath </> localBuildInfoFile defaultDistPref)
  let pkgDescr = localPkgDescr lbi

      root  = PackageItem (pkgName (package pkgDescr)) (pkgVersion (package pkgDescr))
      tloc1 = execState (addDependenciesTree (packageDeps lbi)) (getTop (Node root []))
  (mod_items,tloc2) <- case library pkgDescr of
             Just lib -> addLibraryTree projPath (Set.empty,tloc1) lib
             Nothing  -> return (Set.empty,tloc1)
  (mod_items,tloc2) <- foldM (addExecutableTree projPath) (mod_items,tloc2) (executables pkgDescr)
  tloc3 <- checkAndAddFile projPath "Setup.hs"  SetupScript tloc2
  tloc4 <- checkAndAddFile projPath "Setup.lhs" SetupScript tloc3
  let (hsources,extra_sources) = partition (\fpath -> takeExtension fpath == ".h") (extraSrcFiles pkgDescr)
      tloc5 = execState (do mapM_ (addFilePath HSource  projPath)  hsources
                            mapM_ (addFilePath TextFile projPath) extra_sources
                            mapM_ (addFilePath TextFile projPath) (dataFiles pkgDescr)
                            addFilePath LicenseText projPath (licenseFile pkgDescr)) tloc4
      tloc6 = execState (mapM_ (\item -> insertDown item >> up) (Set.toList mod_items)) tloc1
  return (tree tloc5, tree tloc6)
                            
getTop :: Tree a -> TreeLoc a
getTop = fromTree

insertDown :: forall a. a -> State (TreeLoc a) ()
insertDown label = modify (insertDownLast $ Node label [])

up :: State (TreeLoc a) ()
up = modify' parent

modify' :: (a -> Maybe a) -> State a ()
modify' f = modify (\x -> maybe (error "impossible movement!") id (f x))


addLibraryTree projPath (mod_items,tloc) (Library {libBuildInfo=binfo, exposedModules=exp_mods}) = do
  (exp_mods,hid_mods,mod_items1,tloc1) <- foldM (\st dir -> addSourceDir projPath dir st)
                                                (exp_mods,otherModules binfo,mod_items,tloc)
                                                (hsSourceDirs binfo)
  return $ (mod_items1,execState (mapM_ (addFilePath CSource projPath) (cSources binfo)) tloc1)
  
addExecutableTree projPath (mod_items,tloc) (Executable {modulePath=mainIs, buildInfo=binfo}) = do
  let tloc1 = execState (addFilePath (HsSource ExposedModule) projPath mainIs) tloc
  (exp_mods,hid_mods,mod_items2,tloc2) <- foldM (\st dir -> addSourceDir projPath dir st)
                                                ([],otherModules binfo,mod_items,tloc1)
                                                (hsSourceDirs binfo)
  return $ (mod_items2,execState (mapM_ (addFilePath CSource projPath) (cSources binfo)) tloc2)

addDependenciesTree deps = do
  insertDown (DependenciesItem "Dependencies")
  mapM_ addDependency deps
  up
  where
    addDependency dep = do
      insertDown (PackageItem (pkgName dep) (pkgVersion dep))
      up

addSourceDir :: FilePath -- ^ project location
             -> FilePath -- ^ source sub-directory
             ->    ([ModuleName],[ModuleName],Set.Set ProjectItem,TreeLoc ProjectItem)
             -> IO ([ModuleName],[ModuleName],Set.Set ProjectItem,TreeLoc ProjectItem)
addSourceDir projPath srcDir (exp_mods,hid_mods,mod_items,tloc) = do
  let dir = projPath </> srcDir
  (exp_paths,exp_mods) <- findModules dir exp_mods
  (hid_paths,hid_mods) <- findModules dir hid_mods
  let tloc1 = execState (addFilePath' (\c -> FolderItem c HsSourceFolder) (splitPath' srcDir)
                            (mapM_ (\(mod,loc) -> addFilePath (HsSource ExposedModule) dir loc) exp_paths >>
                             mapM_ (\(mod,loc) -> addFilePath (HsSource HiddenModule)  dir loc) hid_paths))
                        tloc
      mod_items1 = foldr (\(mod,loc) -> Set.insert (ModuleItem mod (dir </> loc) ExposedModule)) mod_items  exp_paths
      mod_items2 = foldr (\(mod,loc) -> Set.insert (ModuleItem mod (dir </> loc) HiddenModule )) mod_items1 hid_paths
  return (exp_mods,hid_mods,mod_items2,tloc1)

addFilePath :: FileKind -> FilePath -> FilePath -> State (TreeLoc ProjectItem) ()
addFilePath kind root fpath = addFilePath' (\c -> FileItem c (root </> fpath) kind) (splitPath' fpath) (return ())

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
                     then modify' firstChild >> insertItem c item
                     else insertDown item
                   addFilePath' mkItem cs cont
                   up >> return ()
  where
    insertItem :: Ord a => x -> a -> State (TreeLoc a) ()
    insertItem c item = do
      item' <- gets getLabel
      case compare item item' of
        LT -> modify $ insertLeft $ simpleNode item
        EQ -> return ()
        GT -> do last <- gets isLast
                 if last
                   then modify $ insertRight $ simpleNode item
                   else modify' right >> insertItem c item

simpleNode item = Node item []

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

findModules :: FilePath                           -- ^source directory location
            -> [ModuleName]                           -- ^module names
            -> IO ([(ModuleName,FilePath)],[ModuleName])  -- ^found modules and unknown modules
findModules location []         = return ([],[])
findModules location (mod:mods) = do
  mb_paths <- findFileWithExtension' (map fst knownSuffixHandlers ++ ["hs", "lhs"]) [location] (toFilePath mod)
  (locs,unks) <- findModules location mods
  case mb_paths of
    Just (_,loc) -> return ((mod,loc) : locs,unks)
    Nothing      -> return (locs,mod:unks)

findFirstFile :: (a -> FilePath) -> [a] -> IO (Maybe a)
findFirstFile file = findFirst
  where findFirst []     = return Nothing
        findFirst (x:xs) = do exists <- doesFileExist (file x)
                              if exists
                                then return (Just x)
                                else findFirst xs
