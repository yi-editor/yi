{-# OPTIONS -fglasgow-exts -Wall -cpp #-}

-- Copyright (C) 2006,2007 Benedikt Schmidt
--               2007 Pepe Iborra
-- see LICENSE.BSD3 for license

module Shim.Hsinfo (ghcInit, findTypeOfPos, getSessionFor,
                    evaluate, findDefinition, load) where

import Shim.CabalInfo
import Shim.ExprSearch
import Shim.SHM
import Shim.SessionMonad
import Shim.Utils
import qualified Shim.GhcCompat as GhcCompat

import Control.Applicative
import Control.Monad.State
import Data.List ( isPrefixOf, find, nubBy, sort, (\\) )
import Data.Maybe
import System.Directory
import System.FilePath ( takeDirectory, (</>), (<.>), dropFileName, takeExtension, equalFilePath )
import System.Time ( getClockTime, ClockTime )
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.Map as M

import qualified GHC

import GHC hiding ( load, getSession, getModuleGraph, getSessionDynFlags,
                    findModule, getRdrNamesInScope, compileExpr, exprType,
                    getPrintUnqual, setSessionDynFlags )
import Outputable
import Panic
import UniqFM ( eltsUFM )
import Packages ( pkgIdMap, exposed, exposedModules )
import Id
import Name
import HscTypes hiding ( getSession )
import SrcLoc
import PprTyThing
import StringBuffer ( stringToStringBuffer, StringBuffer )
import HeaderInfo ( getOptions )
import DriverPhases ( Phase(..), startPhase )
import Yi.Debug (logPutStrLn)
import Distribution.Text
import Distribution.Simple ( pkgName )
import Distribution.Compiler ( CompilerFlavor (..) )
import Distribution.Simple.GHC
import Distribution.Simple.Configure
import Distribution.Verbosity
import Distribution.PackageDescription
  ( buildDepends, PackageDescription, BuildInfo,
    library, executables, hsSourceDirs, extensions, includeDirs, extraLibs,
    libBuildInfo, buildInfo, options, hcOptions, modulePath, allBuildInfo,
    buildable, exeName)
import Distribution.Simple.Setup (defaultDistPref)
import Distribution.Simple.LocalBuildInfo ( packageDeps, buildDir, localPkgDescr )
import Distribution.Package ( Dependency (..) )
import GHC.Exts (unsafeCoerce#)
import qualified GHC.Paths

--------------------------------------------------------------
-- GHC-API helpers
--------------------------------------------------------------

ghcInit :: IO Session
ghcInit = do
  ses <- GhcCompat.newSession (Just ghclibdir)
  dflags0 <- GhcCompat.getSessionDynFlags ses
  let ignore _ _ _ _ = return ()
      dflags1 = dflags0{ hscTarget = HscNothing,
                         verbosity = 1,
                         ghcLink    = NoLink,
                         log_action = ignore}
  GhcCompat.setSessionDynFlags ses dflags1
  return ses

ghclibdir = GHC.Paths.libdir

getCabalOpts :: FilePath -> SHM (Maybe ([String], FilePath))
getCabalOpts sourcefile = do
  cf <- io $ guessCabalFile sourcefile
  logInfo $ "guessed cabal file: " ++ show cf
  case cf of
    Nothing -> return Nothing
    Just cabalfile -> do
      let projPath = takeDirectory cabalfile
      Right lbi <- io $ tryGetConfigStateFile (projPath </> localBuildInfoFile defaultDistPref)
      let pkg = localPkgDescr lbi
      (exe, bi) <- io $ guessCabalStanza projPath sourcefile pkg
      logInfo $ show (allBuildInfo pkg)
      logInfo $ show bi
      logInfo $ show exe

      let pref = buildDir lbi

      return $ case exe of
        Just name -> let targetDir = pref </> name
                         oDir = targetDir </> (name ++ "-tmp")
                         opts = ghcOptions lbi bi oDir
                     in Just (opts, cabalfile)
        Nothing -> let targetDir = pref
                       oDir = targetDir
                       opts = ghcOptions lbi bi oDir
                   in Just (opts, cabalfile)


ghcSetDir :: FilePath -> SHM ()
ghcSetDir projectroot = do
  ses <- getSession
  oldDir <- io $ getCurrentDirectory
  io $ setCurrentDirectory projectroot
  newDir <- io $ getCurrentDirectory
  when (newDir /= oldDir) $
    io $ GhcCompat.workingDirectoryChanged ses

findModuleInFile :: Session -> FilePath -> IO Module
findModuleInFile ses sourcefile = do
  l <- GhcCompat.getModuleGraph ses
  let modq =  ms_mod $ fromMaybe (error "findModuleInFile") $
               find (\ms -> msHsFilePath ms == sourcefile) l
  return modq

idsInScope :: Session -> SHM [String]
idsInScope ses = do
  rdrs <- io $ GhcCompat.getRdrNamesInScope ses
  return $ map (showSDoc.ppr) rdrs

getPrelude :: Session -> IO Module
getPrelude ses = GhcCompat.findModule ses prel_name Nothing
 where prel_name = GHC.mkModuleName "Prelude"

pprIdent :: Id -> String
pprIdent ident = showSDocUnqual $ pprTyThing False (AnId ident)

hashSource :: FilePath -> Maybe String -> IO Hash
hashSource _ (Just source) = return $ MD5.md5 $ BC.pack source -- FIXME: encoding
hashSource sourcefile Nothing = MD5.md5 <$> BC.readFile sourcefile



bufferNeedsPreprocessing :: FilePath -> String -> SHM Bool
bufferNeedsPreprocessing sourcefile source = do
  sourcebuf <- io $ stringToStringBuffer source
  ses <- getSessionFor sourcefile
  dflags <- io $ GhcCompat.getSessionDynFlags ses
  let local_opts = map unLoc (getOptions dflags sourcebuf sourcefile)
  (dflags', _) <- io $ GhcCompat.parseDynamicFlags ses dflags local_opts
  let src_ext = takeExtension sourcefile
      needs_preprocessing
        | Unlit _ <- startPhase src_ext = True
        -- note: local_opts is only required if there's no Unlit phase
        | dopt Opt_Cpp dflags'		= True
        | dopt Opt_Pp  dflags'		= True
        | otherwise			= False
  return needs_preprocessing

--------------------------------------------------------------
-- loading files and populating the compBuffer
--------------------------------------------------------------

load :: FilePath -> Bool -> Maybe String -> SHM (CompilationResult,Session)
load sourcefile store source = do
  (load_succ,ses) <- load' sourcefile source
  case load_succ of
    Succeeded -> do
      let cres = FileCompiled
      logInfo "typecheck successful, storing environment"
      storeIfNeeded cres ses
      return (cres,ses)
    Failed ->
      shmHandle
        (\e -> do logInfo (showException e)
                  return (NothingCompiled (showException e),ses)) $
        do logInfo "first parse failed"
           source' <- readSourceIfNeeded sourcefile source
           let importsOnly = dropExports . dropDefinitions $ source'
           (load_succ',_) <- load' sourcefile (Just importsOnly)
           case load_succ' of
             Succeeded -> do
               let cres = ImportsOnly
               storeIfNeeded cres ses
               return (cres,ses)
             Failed -> do
               logInfo "parse without exports failed too, using prelude only"
               let cres = PreludeOnly
               storeIfNeeded cres ses
               return (cres,ses)
 where storeIfNeeded cres ses = do
         storedres <- M.lookup sourcefile `fmap` getCompBuffer
         when (store && (isNothing storedres ||
                         ((fst3 . fromJust) storedres `replaceWith` cres))) $
           do logInfo $ "compBuffer for "++ sourcefile++" replaced"
              id_data <- io$ getIdData ses
              m <- io $ findModuleInFile ses sourcefile
              cm0 <- io $ GhcCompat.checkModule ses (moduleName m) False
              h <- io $ hashSource sourcefile source
              let cm = do {c <- cm0; return (h, c)}
              storeFileInfo sourcefile cres cm id_data

load' :: FilePath -> Maybe String -> SHM (SuccessFlag,Session)
load' sourcefile source = do
  source' <- addTime source
  ses <- getSessionFor sourcefile
  dflags0 <- io $ GhcCompat.getSessionDynFlags ses
  logAction <- getCompLogAction
  let dflags1 = dflags0{ log_action = logAction, flags = Opt_ForceRecomp : flags dflags0 }
  io $ GhcCompat.setSessionDynFlags ses dflags1
  io $ GhcCompat.setTargets ses [Target (TargetFile sourcefile Nothing) False source']
  loadResult <- io $ GhcCompat.load ses LoadAllTargets
  case loadResult of
       Succeeded -> do -- GHC takes care of setting the right context
         modq <- io $ findModuleInFile ses sourcefile
         io $ GhcCompat.setContext ses [modq] []
         return (Succeeded,ses)
       Failed    -> do   -- We take care of getting at least the Prelude
         io(GhcCompat.setContext ses [] =<< fmap (:[]) (getPrelude ses))
         return (Failed,ses)

addTime :: Maybe String -> SHM (Maybe (StringBuffer, ClockTime))
addTime (Just s) = do now <- io $ getClockTime
                      sb <- io $ stringToStringBuffer s
                      return $ Just (sb,now)
addTime Nothing = return Nothing


getSessionFor :: FilePath -> SHM Session
getSessionFor sourcefile = do
  opts0 <- getCabalOpts sourcefile
  case opts0 of
    Just (opts, cabalfile) ->
      getCabalSession opts cabalfile
    Nothing -> do
      ses <- getSession
      logInfo $ concat ["No cabal file found. ",
                        "Using default options and current directory"]
      ghcSetDir $ dropFileName sourcefile
      return ses

checkModuleCached :: FilePath -> Maybe String -> SHM (TypecheckedModule, Session)
checkModuleCached sourcefile source = do
  l0 <- M.lookup sourcefile `fmap` getCompBuffer
  hash <- io $ hashSource sourcefile source
  case l0 of -- ToDo: check hash of all files, or just speed up ghc-api
    Just (_,_,Just (h, checkedModule)) | h == hash -> do
      ses <- getSessionFor sourcefile
      return (checkedModule,ses)
    _ -> do
      (_,ses) <- load sourcefile True source
      l1 <- M.lookup sourcefile `fmap` getCompBuffer
      case l1 of
        Just (_,_,Just (_, checkedModule)) -> return (checkedModule,ses)
        _ -> error "CheckModuleCached: no checked_module"

getCabalSession :: [String] -> String -> SHM Session
getCabalSession opts cabalfile = do
  mses <- lookupSession cabalfile
  ses <- case mses of
           Just ses -> return ses
           Nothing  -> do ses <- io $ ghcInit
                          addSession cabalfile ses
                          return ses
  logInfo $ concat ["Using options ", unSplit ',' opts,
                    " and cabal file ", cabalfile]
  dflags0 <- io $ GhcCompat.getSessionDynFlags ses
  ghcSetDir $ dropFileName cabalfile
  (dflags1, _) <- io $ GhcCompat.parseDynamicFlags ses dflags0 opts
  io $ GhcCompat.setSessionDynFlags ses dflags1
  return ses

storeFileInfo :: FilePath -> CompilationResult -> Maybe CachedMod -> IdData -> SHM ()
storeFileInfo sourcefile compile_res cm id_data = do
  addCompBuffer sourcefile id_data compile_res cm

getIdData :: Session -> IO IdData
getIdData ses = do
  things <- GhcCompat.getNamesInScope ses >>= mapM (GhcCompat.lookupName ses)
  return [(s $ nameOccName $ idName ident, s $ idType ident)
              | Just(AnId ident) <- things]
      where s x = showSDocUnqual $ ppr x

--------------------------------------------------------------
-- shim commands
--------------------------------------------------------------

findModulesPrefix :: FilePath -> String -> SHM [String]
findModulesPrefix sourcefile pref = do
  ses <- getSessionFor sourcefile
  dflags <- io $ GhcCompat.getSessionDynFlags ses
  let pkg_mods = allExposedModules dflags
  return $ filter (pref `isPrefixOf`) (map (showSDoc.ppr) pkg_mods)

allExposedModules :: DynFlags -> [ModuleName]
allExposedModules dflags =
  concatMap Packages.exposedModules (filter exposed (eltsUFM pkg_db))
 where pkg_db = pkgIdMap (pkgState dflags)

findIdPrefix :: FilePath -> String -> SHM [(String, String)]
findIdPrefix sourcefile pref = do
  l0 <- M.lookup sourcefile `fmap` getCompBuffer
  case l0 of
    Just (_,l,_) -> return . (filterPrefix pref) $ l
    Nothing -> do
      load sourcefile True Nothing
      l1 <- M.lookup sourcefile `fmap` getCompBuffer
      maybe (return []) (return . (filterPrefix pref) . snd3) l1


findTypeOfName :: Session -> String -> SHM String
findTypeOfName ses n = do
  -- prints an error to stderr for things that aren't expressions
  maybe_tything <- io $ GhcCompat.exprType ses n
  maybe (return "") (showForUser . ppr) maybe_tything

 where showForUser doc = do
         unqual <- io $ GhcCompat.getPrintUnqual ses
         return $ showSDocForUser unqual doc

evaluate :: Session -> String -> SHM String
evaluate ses n = do
  maybe_hvalue <- io $ GhcCompat.compileExpr ses ("show (" ++ n ++ ")")
  -- prints errors to stderr?
  return $ maybe "" unsafeCoerce# maybe_hvalue


getModuleExports :: FilePath -> String -> String -> SHM IdData
getModuleExports sourcefile0 modname pref = do
  ses <- getSessionFor sourcefile0
  let currentmod = "AHJEXLJLLKJIUOHGJ"
      sourcefile = (dropFileName sourcefile0) </> currentmod ++ ".hs"
      minSrc = unlines ["module "++currentmod++" where",
                        "import Prelude ()",
                        "import "++modname]
  load sourcefile False (Just minSrc)
  modl <- io $ GhcCompat.findModule ses (GHC.mkModuleName modname) Nothing
  prel_mod <- io $ getPrelude ses
  (as,bs) <- io (GhcCompat.getContext ses)
  io $ GhcCompat.setContext ses [] [prel_mod,modl]
  unqual <- io (GhcCompat.getPrintUnqual ses)
  io (GhcCompat.setContext ses as bs)
  mb_mod_info <- io $ GhcCompat.getModuleInfo ses modl
  case mb_mod_info of
    Nothing -> error "unknown module"
    Just mod_info -> do
      let names = modInfoExports mod_info
      things <- io $ forM names
                       (\n -> ((,) n) `fmap` GhcCompat.lookupName ses n)
      return
        $ filterPrefix pref
        $ map (\(n,t) ->
                 (showSDocForUser unqual . ppr $ n,
                  maybe ""
                    (showSDocForUser unqual . pprTyThingInContext False)
                    t))
              things

findDefinition :: FilePath -> Int -> Int -> Maybe String -> SHM SrcLoc
findDefinition sourcefile line col source = do
  (cm,_) <- checkModuleCached sourcefile source
  case findExprInCheckedModule line col cm of
    FoundName name -> return $ nameSrcLoc name
    FoundId ident  -> return $ nameSrcLoc (getName ident)
    _              -> return $ noSrcLoc

pprExplicitForAlls :: SHM Bool
pprExplicitForAlls = do
  s <- getSession
  dflags <- io $ GhcCompat.getSessionDynFlags s
  return$ dopt Opt_PrintExplicitForalls dflags

findTypeOfPos :: FilePath -> Int -> Int -> Maybe String -> SHM String
findTypeOfPos sourcefile line col source = do
  (cm,ses) <- checkModuleCached sourcefile source
  let modInfo = GHC.moduleInfo cm
  unqual <- io$ GhcCompat.getPrintUnqual ses
  case findExprInCheckedModule line col cm of
    FoundName name -> do
      mb_tyThing <- io $ GhcCompat.modInfoLookupName ses modInfo name
      case mb_tyThing of
            Just tyThing -> return $! showSDocForUser unqual
                              (pprTyThingInContextLoc True tyThing)
            Nothing      -> return $ "<not found>"
    FoundId ident ->  return $ pprIdent ident
    _ -> return $ "<not found>"

--------------------------------------------------------------
-- utility functions
--------------------------------------------------------------

filterPrefix :: (Eq [a], Eq a) => [a] -> [([a], b)] -> [([a], b)]
filterPrefix pref ids =
  nubBy (equating fst) . filter ((pref `isPrefixOf`) . fst) $ ids

readSourceIfNeeded :: FilePath -> Maybe String -> SHM String
readSourceIfNeeded sourcefile source =
  case source of
    Nothing ->
      io $ readFile sourcefile
    Just s ->
      return s

lineNotPrefixed :: [String] -> String -> Bool
lineNotPrefixed prefs s = not (any (`isPrefixOf` s) prefs)


dropDefinitions :: String -> String
dropDefinitions = unlines . reverse .
                  dropWhile (lineNotPrefixed ["module ", "import "]) .
                  reverse . lines

dropExports :: String -> String
dropExports s = imports_only ++ "\nmain = undefined" -- ToDo: don't use ad-hoc parser
  where imports_only = unlines . dropWhile (lineNotPrefixed ["import "]) . lines $ s


--------------------------------------------------------------
-- tests
--------------------------------------------------------------

runTest :: FilePath -> String -> SHM a -> IO a
runTest sourcefile source m = do
  writeFile sourcefile source
  ghcInit >>= (\ses -> runSHM ses "ghc" (\_ _ _ _ -> return ()) m)


testSource :: String
testSource = unlines ["module ShimTest where",
                      "import Prelude (print, Integer)",
                      "import Data.Maybe",
                      "main = print \"foobar\"",
                      "fromTest = 5",
                      "someFun = 3"]

brokenSource :: String
brokenSource = testSource ++ "\nfoo = unknownFunction"

testFilename :: String
testFilename = "/tmp/ShimTest.hs"

assertLEq :: (Show a, Monad m, Ord a) => [a] -> [a] -> m ()
assertLEq expected got = do
  unless ((sort expected)==(sort got)) $
    error $ "\nexpected: " ++ (show expected) ++ "\ngot:      " ++ (show got)
            ++ "\ndiff: " ++ (show $ (expected \\ got) ++ (got \\ expected))

t1 :: IO ()
t1 = do
  mods <- runTest testFilename testSource $
            findModulesPrefix testFilename "Data.May"
  assertLEq ["Data.Maybe"] mods

t_findIdPrefix :: String -> String -> [(String, String)] -> IO ()
t_findIdPrefix source pref expected = do
  comps <- runTest testFilename source $ do
              load testFilename True (Just source)
              findIdPrefix testFilename pref
  assertLEq expected comps

t2 :: IO ()
t2 = t_findIdPrefix testSource "from"
       ([("fromTest","Integer"),
         ("fromJust","Maybe a -> a"),
         ("fromMaybe","a -> Maybe a -> a")])

t3 :: IO ()
t3 = t_findIdPrefix testSource "someF" [("someFun","Integer")]

t4 :: IO ()
t4 = t_findIdPrefix brokenSource "from"
       [("fromJust","Maybe a -> a"),("fromMaybe","a -> Maybe a -> a")]

t5 :: IO ()
t5 = t_findIdPrefix brokenSource "someF" []

-- works because last successful compile is used
t6 :: IO ()
t6 = do
  comps <- runTest testFilename testSource $ do
              load testFilename True (Just testSource)
              io $ writeFile testFilename brokenSource
              load testFilename True (Just brokenSource)
              findIdPrefix testFilename "someF"
  assertLEq [("someFun","Integer")] comps

runTests :: IO ()
runTests = sequence_ [t1,t2,t3,t4,t5,t6]
