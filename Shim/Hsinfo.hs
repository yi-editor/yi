{-# OPTIONS -fglasgow-exts -Wall -cpp #-}

-- Copyright (C) 2006,2007 Benedikt Schmidt
--               2007 Pepe Iborra
-- see LICENSE.BSD3 for license

module Shim.Hsinfo where

import Shim.SHM
import Shim.Utils
import Shim.ExprSearch
import Shim.SessionMonad
import Shim.GhcCompat

import Control.Applicative
import qualified Control.Exception as CE
import qualified Data.Map as M
import qualified Control.Concurrent.MVar as MVar
import List ( isPrefixOf, find, nubBy,
              sort, (\\), nub )
import Directory
import Time ( getClockTime, ClockTime )
import System.FilePath ( takeDirectory, (</>) , dropFileName, takeExtension )
import Control.Monad.State
import Data.Maybe
import qualified Data.ByteString.Char8 as BC
import qualified Data.Digest.SHA1 as SHA1

import qualified GHC
import GHC hiding ( load, newSession )
import Outputable
import Panic
import UniqFM ( eltsUFM )
import Packages ( pkgIdMap, exposed, exposedModules )
import Id
import Name
import HscTypes
import SrcLoc
import PprTyThing
import ErrUtils ( mkLocMessage )
import StringBuffer ( stringToStringBuffer, StringBuffer )
import HeaderInfo ( getOptions )
import DriverPhases ( Phase(..), startPhase )

import Distribution.Simple ( showPackageId )
import Distribution.Compiler ( CompilerFlavor (..) )
import Distribution.Simple.Compiler ( extensionsToFlags )
import Distribution.Simple.Program ( defaultProgramConfiguration )
import Distribution.Simple.Configure
import Distribution.Verbosity
import Distribution.PackageDescription 
  ( buildDepends, PackageDescription, flattenPackageDescription, BuildInfo,
    library, executables, hsSourceDirs, extensions, includeDirs, extraLibs,
    libBuildInfo, buildInfo, options, hcOptions, modulePath)

import Distribution.Simple.LocalBuildInfo ( packageDeps )
import Distribution.Version ( Dependency (..) )

--------------------------------------------------------------
-- GHC-API helpers
--------------------------------------------------------------

ghcInit :: String -> IO Session
ghcInit ghc = do
  ghclibdir <- getLibdir ghc
#if __GLASGOW_HASKELL__ == 606
  ses <- newSession JustTypecheck (Just ghclibdir)
#else
  ses <- newSession (Just ghclibdir)
#endif
  dflags0 <- GHC.getSessionDynFlags ses
  let ignore _ _ _ _ = return ()
      dflags1 = dflags0{ hscTarget = HscNothing,
                         verbosity = 1,
#if __GLASGOW_HASKELL__ > 606
                         ghcLink    = NoLink,
#endif
                         log_action = ignore}
  GHC.setSessionDynFlags ses dflags1
  return ses

getLibdir :: String -> IO String
getLibdir ghc = chomp `liftM` commandGetContents (ghc ++ " --print-libdir")

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

getCabalOpts :: FilePath -> SHM (Maybe ([String], FilePath))
getCabalOpts sourcefile = do
  cf <- io $ guessCabalFile sourcefile
  logInfo $ "guessed cabal file: " ++ show cf
  case cf of
    Nothing -> return Nothing
    Just cabalfile -> do
      pkg <- flattenPackageDescription <$> 
               (io $ readPackageDescription cabalfile)
      lbi0 <- io $ maybeGetPersistBuildConfig
      let bi = guessCabalStanza sourcefile pkg
          pdeps = case lbi0 of
                    Just lbi -> map showPackageId $ packageDeps lbi
                    Nothing -> map (\x -> case x of
                                            Dependency s _ -> s)
                                   (buildDepends pkg)
      opts <- createOptsFromBuildInfo pdeps bi
      return $ Just (opts, cabalfile)

guessCabalStanza :: FilePath -> PackageDescription -> BuildInfo
guessCabalStanza _sourcefile pkg =
  case library pkg of
    Nothing ->
      case executables pkg of
        [] -> error "guessCabalStanza: there is no lib or exe in here"
        exes@(first:_) ->
          case find (("Main" `isPrefixOf`) . modulePath) exes of
            Just exe -> buildInfo exe
            Nothing -> buildInfo first
    Just lib ->
      libBuildInfo lib

createOptsFromBuildInfo :: [String] -> BuildInfo -> SHM [String]
createOptsFromBuildInfo pdeps bi = do
  (comp,_pc) <- io$ configCompiler (Just GHC) Nothing Nothing defaultProgramConfiguration silent
  return (extensionsToFlags comp (extensions bi)
    ++ (filterOptions $ hcOptions GHC (options bi))
--  ++ ["-hide-all-packages"]
--  ++ ["-i"]
    ++ ["-l" ++lib | lib <- extraLibs bi]
    ++ ["-i" ++ l | l <- nub (hsSourceDirs bi)]
    ++ ["-I" ++ dir | dir <- includeDirs bi]
--  ++ [ "-#include \"" ++ inc ++ "\"" | inc <- includes bi ]
    ++ (concat [ ["-package", pkg] | pkg <- pdeps]))

-- ghc options that can be used with JustTypecheck
filterOptions :: [String] -> [String]
filterOptions [] = []
filterOptions (o:a:os)
  | o `elem` allowedWithArg = o:a:(filterOptions os)
  where allowedWithArg = ["-pgmF"]
filterOptions (o:os)
  | o `elem` allowedArgs = o:(filterOptions os)
  | any (`isPrefixOf` o) allowedPrefixes = o:(filterOptions os)
  | otherwise = filterOptions os
  where allowedArgs = ["-fglasgow-exts", "-Wall", "-Werror","-F", "-fth"]
        allowedPrefixes = ["-fno-warn-","-i","-I","-M"]

ghcSetDir :: FilePath -> SHM ()
ghcSetDir projectroot = do
  ses <- getSession
  oldDir <- io $ getCurrentDirectory
  io $ setCurrentDirectory projectroot
  newDir <- io $ getCurrentDirectory
  when (newDir /= oldDir) $
    io $ workingDirectoryChanged ses

findModuleInFile :: Session -> FilePath -> IO Module
findModuleInFile ses sourcefile = do
  l <- GHC.getModuleGraph ses
  let modq =  ms_mod $ fromMaybe (error "findModuleInFile") $
               find (\ms -> msHsFilePath ms == sourcefile) l
  return modq

idsInScope :: Session -> SHM [String]
idsInScope ses = do
  rdrs <- io $ GHC.getRdrNamesInScope ses
  return $ map (showSDoc.ppr) rdrs

getPrelude :: Session -> IO Module
getPrelude ses = GHC.findModule ses prel_name Nothing
 where prel_name = GHC.mkModuleName "Prelude"

pprIdent :: Id -> String
pprIdent ident = showSDocUnqual $ pprTyThing False (AnId ident)

hashSource :: FilePath -> Maybe String -> IO BC.ByteString
hashSource _ (Just source) = return . SHA1.hash' $ BC.pack source
hashSource sourcefile Nothing = SHA1.fileHash sourcefile



bufferNeedsPreprocessing :: FilePath -> String -> SHM Bool
bufferNeedsPreprocessing sourcefile source = do
  sourcebuf <- io $ stringToStringBuffer source
  ses <- getSessionFor sourcefile
  dflags <- io $ getSessionDynFlags ses
  let local_opts = map unLoc (getOptions sourcebuf sourcefile)
  (dflags', _) <- io $ parseDynamicFlags dflags local_opts
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
  (load_succ,cnotes,ses) <- load' sourcefile source
  case load_succ of
    Succeeded -> do
      let cres = FileCompiled cnotes
      logInfo "typecheck successful, storing environment"
      storeIfNeeded cres ses
      return (cres,ses)
    Failed ->
      shmHandle
        (\e -> do logInfo (showException e)
                  return (NothingCompiled (showException e) cnotes,ses)) $
        do logInfo "first parse failed"
           source' <- readSourceIfNeeded sourcefile source
           let importsOnly = dropExports . dropDefinitions $ source'
           (load_succ',_,_) <- load' sourcefile $ Just importsOnly
           case load_succ' of
             Succeeded -> do
               let cres = ImportsOnly cnotes
               storeIfNeeded cres ses
               return (cres,ses)
             Failed -> do
               logInfo "parse without exports failed too, using prelude only"
               let cres = PreludeOnly cnotes
               storeIfNeeded cres ses
               return (cres,ses)
 where storeIfNeeded cres ses = do
         storedres <- M.lookup sourcefile `liftM` getCompBuffer
         when (store && (isNothing storedres ||
                         ((fst3 . fromJust) storedres `replaceWith` cres))) $
           do logInfo $ "compBuffer for "++ sourcefile++" replaced"
              id_data <- io$ getIdData ses
              m <- io $ findModuleInFile ses sourcefile
#if __GLASGOW_HASKELL__ > 606
              cm0 <- io $ checkModule ses (moduleName m) False
#else
              cm0 <- io $ checkModule ses $ moduleName m
#endif
              h <- io $ hashSource sourcefile source
              let cm = do {c <- cm0; return (h, c)}
              storeFileInfo sourcefile cres cm id_data

load' :: FilePath -> Maybe String -> SHM (SuccessFlag,[CompileNote],Session)
load' sourcefile source = do
  source' <- addTime source
  ses <- getSessionFor sourcefile
  dflags0 <- io $ GHC.getSessionDynFlags ses
  ref <- io $ MVar.newMVar []
  let dflags1 = dflags0{ log_action = logMsg ref, flags = Opt_ForceRecomp : flags dflags0 }
  io $ GHC.setSessionDynFlags ses dflags1
  io $ GHC.setTargets ses [Target (TargetFile sourcefile Nothing) source']
  loadResult <- io $ GHC.load ses LoadAllTargets
  cnotes <- io $ reverse `liftM` MVar.readMVar ref
  case loadResult of
       Succeeded -> do -- GHC takes care of setting the right context
         modq <- io $ findModuleInFile ses sourcefile
         io $ GHC.setContext ses [modq] []
         return (Succeeded,cnotes,ses)
       Failed    -> do   -- We take care of getting at least the Prelude
         io(GHC.setContext ses [] =<< atomM (getPrelude ses))
         return (Failed,cnotes,ses)
  where atomM = liftM (:[])
        logMsg ref severity' srcSpan' style' msg' = do
          dir <- getCurrentDirectory
          logS ('\n':show ((mkLocMessage srcSpan' msg') style'))
          MVar.modifyMVar_ ref
            (\l -> return $ (CompileNote severity' srcSpan'
                                         style' msg' dir):l)

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

checkModuleCached :: FilePath -> Maybe String -> SHM (CheckedModule, Session)
checkModuleCached sourcefile source = do
  l0 <- M.lookup sourcefile `liftM` getCompBuffer
  hash <- io $ hashSource sourcefile source
  case l0 of -- ToDo: check hash of all files, or just speed up ghc-api
    Just (_,_,Just (h, checkedModule)) | h == hash -> do
      ses <- getSessionFor sourcefile
      return (checkedModule,ses)
    _ -> do
      (_,ses) <- load sourcefile True source
      l1 <- M.lookup sourcefile `liftM` getCompBuffer
      case l1 of
        Just (_,_,Just (_, checkedModule)) -> return (checkedModule,ses)
        _ -> error "CheckModuleCached: no checked_module"

getCabalSession :: [String] -> String -> SHM Session
getCabalSession opts cabalfile = do
  mses <- lookupSession cabalfile
  ghc <- gets ghcProgram
  ses <- case mses of
           Just ses -> return ses
           Nothing  -> do ses <- io $ ghcInit ghc
                          addSession cabalfile ses
                          return ses
  logInfo $ concat ["Using options ", unSplit ',' opts,
                    " and cabal file ", cabalfile]
  dflags0 <- io $ GHC.getSessionDynFlags ses
  ghcSetDir $ dropFileName cabalfile
  (dflags1, _) <- io $ GHC.parseDynamicFlags dflags0 opts
  io $ GHC.setSessionDynFlags ses dflags1
  return ses

storeFileInfo :: FilePath -> CompilationResult -> Maybe CachedMod -> IdData -> SHM ()
storeFileInfo sourcefile compile_res cm id_data = do
  addCompBuffer sourcefile id_data compile_res cm

getIdData :: Session -> IO IdData
getIdData ses = do
  things <- getNamesInScope ses >>= mapM (lookupName ses)
  return [(s $ nameOccName $ idName ident, s $ idType ident) 
              | Just(AnId ident) <- things]
      where s x = showSDocUnqual $ ppr x

--------------------------------------------------------------
-- shim commands
--------------------------------------------------------------

findModulesPrefix :: FilePath -> String -> SHM [String]
findModulesPrefix sourcefile pref = do
  ses <- getSessionFor sourcefile
  dflags <- io $ GHC.getSessionDynFlags ses
  let pkg_mods = allExposedModules dflags
  return $ filter (pref `isPrefixOf`) (map (showSDoc.ppr) pkg_mods)

allExposedModules :: DynFlags -> [ModuleName]
allExposedModules dflags =
  map GHC.mkModuleName (concatMap exposedModules
                       (filter exposed (eltsUFM pkg_db)))
 where pkg_db = pkgIdMap (pkgState dflags)

findIdPrefix :: FilePath -> String -> SHM [(String, String)]
findIdPrefix sourcefile pref = do
  l0 <- M.lookup sourcefile `liftM` getCompBuffer
  case l0 of
    Just (_,l,_) -> return . (filterPrefix pref) $ l
    Nothing -> do
      load sourcefile True Nothing
      l1 <- M.lookup sourcefile `liftM` getCompBuffer
      maybe (return []) (return . (filterPrefix pref) . snd3) l1


findTypeOfName :: Session -> String -> SHM String
findTypeOfName ses n = do
  -- prints an error to stderr for things that aren't expressions
  maybe_tything <- io $ GHC.exprType ses n
  maybe (return "") (showForUser . ppr) maybe_tything

 where showForUser doc = do
         unqual <- io $ GHC.getPrintUnqual ses
         return $ showSDocForUser unqual doc
 
getModuleExports :: FilePath -> String -> String -> SHM IdData
getModuleExports sourcefile0 modname pref = do
  ses <- getSessionFor sourcefile0
  let currentmod = "AHJEXLJLLKJIUOHGJ"
      sourcefile = (dropFileName sourcefile0) </> currentmod ++ ".hs"
      minSrc = unlines ["module "++currentmod++" where",
                        "import Prelude ()",
                        "import "++modname]
  load sourcefile False (Just minSrc)
  modl <- io $ GHC.findModule ses (GHC.mkModuleName modname) Nothing
  prel_mod <- io $ getPrelude ses
  (as,bs) <- io (GHC.getContext ses)
  io $ GHC.setContext ses [] [prel_mod,modl]
  unqual <- io (GHC.getPrintUnqual ses)
  io (GHC.setContext ses as bs)
  mb_mod_info <- io $ GHC.getModuleInfo ses modl
  case mb_mod_info of
    Nothing -> error "unknown module"
    Just mod_info -> do
      let names = GHC.modInfoExports mod_info
      things <- io $ forM names
                       (\n -> ((,) n) `liftM` GHC.lookupName ses n)
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

#if __GLASGOW_HASKELL__ > 606
pprExplicitForAlls :: SHM Bool
pprExplicitForAlls = do
  s <- getSession
  dflags <- io $ GHC.getSessionDynFlags s
  return$ dopt Opt_PrintExplicitForalls dflags
#endif

findTypeOfPos :: FilePath -> Int -> Int -> Maybe String -> SHM String
findTypeOfPos sourcefile line col source = do
  (cm,ses) <- checkModuleCached sourcefile source
  let Just modInfo = GHC.checkedModuleInfo cm
  unqual <- io$ GHC.getPrintUnqual ses
  case findExprInCheckedModule line col cm of
    FoundName name -> do
      mb_tyThing <- io $ GHC.modInfoLookupName ses modInfo name
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
  (ghcInit "ghc") >>= (\ses -> runSHM ses "ghc" m)


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
