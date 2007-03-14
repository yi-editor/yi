

-- | This module is the interface to GHC (interpreter).  It knows
-- nothing about Yi (at the haskell level; it can know names of
-- modules/functions at strings)

module Yi.Kernel (initialize, yiContext, Kernel) where

import Yi.Debug hiding (error)

import System.Directory     ( doesFileExist, getHomeDirectory )
import System.Exit	( exitWith, ExitCode(..) )

import qualified GHC
import qualified Packages
import qualified DynFlags
import qualified ObjLink
import qualified PackageConfig
import qualified Linker
import Outputable

import GHC.Exts ( unsafeCoerce# )

type Kernel = GHC.Session

------------------
-- GHCi embedding

-- the path of our GHC installation
path :: FilePath
path = GHC_LIBDIR -- See Setup.hs

-- | Create a suitable GHC session.
initialize :: IO Kernel
initialize = GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
  session <- GHC.newSession GHC.Interactive (Just path)
  dflags1 <- GHC.getSessionDynFlags session

  home <- getHomeDirectory
  (dflags1',_otherFlags) <- GHC.parseDynamicFlags dflags1 [
                                                           "-package ghc", "-fglasgow-exts", "-cpp", 
#ifdef YI_FLAVOUR_GTK
                                                           "-package yi-gtk",
                                                           "-hide-package yi-vty",
#else
                                                           "-hide-package yi-gtk",
                                                           "-package yi-vty",
#endif
                                                           "-i", -- clear the search directory (don't look in ./)
                                                           "-i" ++ home ++ "/.yi" -- We look for source files in ~/.yi
                                                           ,"-v"
                                                          ]
  (dflags2, packageIds) <- Packages.initPackages dflags1'
  logPutStrLn $ "packagesIds: " ++ (showSDocDump $ ppr $ packageIds)
  GHC.setSessionDynFlags session dflags2{GHC.hscTarget=GHC.HscInterpreted}
  yiContext session
  return session

-- | Dynamically start Yi. (this does not work inside "interpreted" code -- so it's not used)
startYi :: Kernel -> IO ()
startYi session = GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
  result <- GHC.compileExpr session ("Yi.main :: Yi.Kernel -> IO ()") 
  -- coerce the interpreted expression, so we check that we are not making an horrible mistake.
  testEval session "Before jump."
  logPutStrLn "Starting Yi!"
  case result of
    Nothing -> error "Could not compile Yi.main!"
    Just x -> do let (x' :: Kernel -> IO ()) = unsafeCoerce# x
                 x' session
                 return ()

testEval session msg = do
  result <- GHC.compileExpr session ("1 :: Int")
  case result of
    Nothing -> error $ "eval does not work / " ++ msg
    Just x -> return ()

yiContext session = do
  preludeModule <- GHC.findModule session (GHC.mkModuleName "Prelude") Nothing
  yiModule <- GHC.findModule session (GHC.mkModuleName "Yi.Yi") Nothing -- this module re-exports all useful stuff.
  GHC.setContext session [] [preludeModule, yiModule]


showModules session = do
  logPutStrLn "Loaded modules:"
  let show_one ms = do m <- GHC.showModule session ms
                       logPutStrLn (showSDocDump $ ppr $ ms)
		       logPutStrLn m
  graph <- GHC.getModuleGraph session
  mapM_ show_one graph

showContext session = do
  ctx <- GHC.getContext session
  logPutStrLn $ "Context: " ++ (showSDocDump $ ppr $ ctx)
