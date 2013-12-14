--
-- Copyright (c) Krasimir Angelov 2008.
--
-- GTK implementation for "Project View".
-- It uses the Shim.ProjectContent extraction
-- algorithm.
--

module Yi.UI.Pango.ProjectTree
         ( projectTreeNew
         , loadProjectTree
         ) where

import Data.Tree
import Shim.ProjectContent
import qualified Graphics.UI.Gtk.ModelView as MView
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf)
import Graphics.UI.Gtk.ModelView.TreeView ( treeViewHeadersVisible
                                          , onRowActivated
                                          )
import System.Glib.Attributes
import Yi.Keymap
import Yi.Dired
import Yi.File (editFile)
import Yi.UI.Pango.Utils

projectTreeNew :: (Action -> IO ()) -> Gtk.TreeStore ProjectItem -> IO Gtk.TreeView
projectTreeNew post store = do
  projectTree <- MView.treeViewNewWithModel store
  set projectTree [treeViewHeadersVisible := False]

  col1 <- MView.treeViewColumnNew
  col2 <- MView.treeViewColumnNew

  renderer1 <- MView.cellRendererPixbufNew
  renderer2 <- MView.cellRendererTextNew

  MView.cellLayoutPackStart col1 renderer1 True
  MView.cellLayoutPackStart col2 renderer2 True

  icoProject          <- loadIcon "project.png"
  icoDependencies     <- loadIcon "dependencies.png"
  icoPlainFolder      <- loadIcon "plain-folder.png"
  icoHsSourceFolder   <- loadIcon "hs-source-folder.png"
  icoExposedModule    <- loadIcon "exposed-module.png"
  icoHiddenModule     <- loadIcon "hidden-module.png"
  icoExposedFileModule<- loadIcon "exposed-file-module.png"
  icoHiddenFileModule <- loadIcon "hidden-file-module.png"
  icoCSource          <- loadIcon "c-source.png"
  icoHSource          <- loadIcon "h-source.png"
  icoTextFile         <- loadIcon "text-file.png"
  icoLicenseFile      <- loadIcon "license-file.png"
  icoPackage          <- loadIcon "package.png"
  icoSetupScript      <- loadIcon "setup-script.png"
  let icos = Icons icoProject
                   icoDependencies
                   icoPlainFolder
                   icoHsSourceFolder
                   icoExposedModule
                   icoHiddenModule
                   icoExposedFileModule
                   icoHiddenFileModule
                   icoCSource
                   icoHSource
                   icoTextFile
                   icoLicenseFile
                   icoPackage
                   icoSetupScript

  MView.cellLayoutSetAttributes col1 renderer1 store $ \row -> [MView.cellPixbuf := itemIcon icos row]
  MView.cellLayoutSetAttributes col2 renderer2 store $ \row -> [MView.cellText   := itemName row]

  MView.treeViewAppendColumn projectTree col1
  MView.treeViewAppendColumn projectTree col2

  onRowActivated projectTree $ \path _col -> do
    item <- MView.treeStoreGetValue store path
    case item of
      FileItem  {itemFPath=path} -> post (makeAction (editFile path))
      ModuleItem{itemFPath=path} -> post (makeAction (editFile path))
      _                          -> return ()

  return projectTree

loadProjectTree :: Gtk.TreeStore a -> Tree a -> IO ()
loadProjectTree projectStore tree = do
  MView.treeStoreClear projectStore
  MView.treeStoreInsertTree projectStore [] 0 tree

data Icons
  = Icons
      { icoProject          :: Pixbuf
      , icoDependencies     :: Pixbuf
      , icoPlainFolder      :: Pixbuf
      , icoHsSourceFolder   :: Pixbuf
      , icoExposedModule    :: Pixbuf
      , icoHiddenModule     :: Pixbuf
      , icoExposedFileModule:: Pixbuf
      , icoHiddenFileModule :: Pixbuf
      , icoCSource          :: Pixbuf
      , icoHSource          :: Pixbuf
      , icoTextFile         :: Pixbuf
      , icoLicenseFile      :: Pixbuf
      , icoPackage          :: Pixbuf
      , icoSetupScript      :: Pixbuf
      }

itemIcon icos (ProjectItem{})              = icoProject      icos
itemIcon icos (DependenciesItem{})         = icoDependencies icos
itemIcon icos (FolderItem{folderKind=kind})= folderIcon      icos kind
itemIcon icos (FileItem{fileKind=kind})    = fileIcon        icos kind
itemIcon icos (ModuleItem{moduleKind=kind})= moduleIcon      icos kind icoExposedModule icoHiddenModule
itemIcon icos (PackageItem{})              = icoPackage      icos

fileIcon icos (HsSource kind) = moduleIcon        icos kind icoExposedFileModule icoHiddenFileModule
fileIcon icos CSource         = icoCSource        icos
fileIcon icos HSource         = icoHSource        icos
fileIcon icos TextFile        = icoTextFile       icos
fileIcon icos SetupScript     = icoSetupScript    icos
fileIcon icos LicenseText     = icoLicenseFile    icos

folderIcon icos HsSourceFolder  = icoHsSourceFolder icos
folderIcon icos PlainFolder     = icoPlainFolder    icos

moduleIcon icos ExposedModule icoE _    = icoE icos
moduleIcon icos HiddenModule  _    icoH = icoH icos
