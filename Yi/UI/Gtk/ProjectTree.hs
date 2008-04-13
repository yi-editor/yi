module Yi.UI.Gtk.ProjectTree where

import Distribution.Version
import Distribution.Package
import Shim.ProjectContent
import qualified Data.Tree as Tree
import qualified Graphics.UI.Gtk.ModelView as MView
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk hiding ( Window, Event, Point, Style )
import System.FilePath
import Paths_yi
import Yi.Keymap
import Yi.Dired

projectTreeNew post = do
  projectStore <- MView.treeStoreNew []
  projectTree <- MView.treeViewNewWithModel projectStore
  set projectTree [treeViewHeadersVisible := False]

  -- add three columns
  col1 <- MView.treeViewColumnNew
  col2 <- MView.treeViewColumnNew

  renderer1 <- MView.cellRendererPixbufNew
  renderer2 <- MView.cellRendererTextNew

  MView.cellLayoutPackStart col1 renderer1 True 
  MView.cellLayoutPackStart col2 renderer2 True

  icoProject      <- loadIcon "project.png"
  icoDependencies <- loadIcon "dependencies.png"
  icoPlainFolder  <- loadIcon "plain-folder.png"
  icoHsSourceFolder<-loadIcon "hs-source-folder.png"
  icoFile         <- loadIcon "hsfile.png"
  icoExposedModule<- loadIcon "exposed-module.png"
  icoHiddenModule <- loadIcon "hidden-module.png"
  icoCSource      <- loadIcon "c-source.png"
  icoHSource      <- loadIcon "h-source.png"
  icoTextFile     <- loadIcon "text-file.png"
  icoLicenseFile  <- loadIcon "license-file.png"
  icoPackage      <- loadIcon "package.png"
  icoSetupScript  <- loadIcon "setup-script.png"
  let icos = Icons icoProject
                   icoDependencies
                   icoPlainFolder
                   icoHsSourceFolder
                   icoExposedModule
                   icoHiddenModule
                   icoCSource
                   icoHSource
                   icoTextFile
                   icoLicenseFile
                   icoPackage
                   icoSetupScript

  MView.cellLayoutSetAttributes col1 renderer1 projectStore $ \row -> [MView.cellPixbuf := itemIcon icos row]
  MView.cellLayoutSetAttributes col2 renderer2 projectStore $ \row -> [MView.cellText   := itemDisplayName row]

  MView.treeViewAppendColumn projectTree col1
  MView.treeViewAppendColumn projectTree col2
  
  onRowActivated projectTree $ \path col -> do
    item <- MView.treeStoreGetValue projectStore path
    case item of
      FileItem{itemFPath=path} -> post (makeAction (fnewE path))
      _                        -> return ()

  return (projectTree, projectStore)

loadProjectTree projectStore tree = do
  MView.treeStoreClear projectStore
  MView.treeStoreInsertTree projectStore [] 0 tree

loadIcon fpath = do
  datadir <- getDataDir
  icoProject <- pixbufNewFromFile (datadir </> "icons" </> fpath)
  pixbufAddAlpha icoProject (Just (0,255,0))


data Icons
  = Icons
      { icoProject      :: Pixbuf
      , icoDependencies :: Pixbuf
      , icoPlainFolder  :: Pixbuf
      , icoHsSourceFolder::Pixbuf
      , icoExposedModule:: Pixbuf
      , icoHiddenModule :: Pixbuf
      , icoCSource      :: Pixbuf
      , icoHSource      :: Pixbuf
      , icoTextFile     :: Pixbuf
      , icoLicenseFile  :: Pixbuf
      , icoPackage      :: Pixbuf
      , icoSetupScript  :: Pixbuf
      }

itemIcon icos (ProjectItem{})              = icoProject      icos
itemIcon icos (DependenciesItem{})         = icoDependencies icos
itemIcon icos (FolderItem{itemFKind=kind}) = kindIcon        icos kind
itemIcon icos (FileItem{itemFKind=kind})   = kindIcon        icos kind
itemIcon icos (PackageItem{})              = icoPackage      icos

kindIcon icos ExposedModule = icoExposedModule  icos
kindIcon icos HiddenModule  = icoHiddenModule   icos
kindIcon icos CSource       = icoCSource        icos
kindIcon icos HSource       = icoHSource        icos
kindIcon icos TextFile      = icoTextFile       icos
kindIcon icos SetupScript   = icoSetupScript    icos
kindIcon icos LicenseText   = icoLicenseFile    icos
kindIcon icos HsSourceFolder= icoHsSourceFolder icos
kindIcon icos PlainFolder   = icoPlainFolder    icos

itemDisplayName item =
  let pkg_id = PackageIdentifier (itemName item) (itemVersion item)

      disp_name = case item of
                    ProjectItem{} -> showPackageId pkg_id
                    PackageItem{} -> showPackageId pkg_id
                    item          -> itemName item
  in disp_name
