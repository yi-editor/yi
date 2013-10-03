{-# LANGUAGE Rank2Types #-}
-- Copyright (c) Krasimir Angelov 2008.
--
-- Extraction of the "Project View" from
-- already configured Cabal package.
--


module Shim.ProjectContent
         ( {- loadProject
         , -} itemName
         , ProjectItem(..)
         , FileKind(..)
         , FolderKind(..)
         , ModuleKind(..)
         ) where


import Distribution.ModuleName
import Distribution.Version
import Distribution.Package
import Distribution.Text

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
