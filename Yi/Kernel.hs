

-- | This module is the interface to GHC (interpreter).  It knows
-- nothing about Yi (at the haskell level; it can know names of
-- modules/functions at strings)

module Yi.Kernel (initialize, Kernel(..), eval, startYi) where

import Yi.Debug hiding (error)

import System.Directory     ( getHomeDirectory )

import qualified GHC
import qualified Packages
import qualified DynFlags
import qualified PackageConfig
import qualified Linker
import Outputable

import GHC.Exts ( unsafeCoerce# )


-- | GHC API Kernel. 
-- Calls to the GHC API must go though this type. (Because of "global variables" in GHC I imagine).
-- ie. the simpler approach of passing just the GHC. Session does not work
data Kernel = Kernel
    {
     getSessionDynFlags :: IO GHC.DynFlags,
     setSessionDynFlags :: GHC.DynFlags -> IO [Packages.PackageId],
     compileExpr :: String -> IO (Maybe GHC.HValue),
     yiContext :: IO (),                 
     addTarget :: String -> IO (),
     loadAllTargets :: IO GHC.SuccessFlag
    }



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
--                                                           ,"-v"
                                                          ]
  (dflags2, packageIds) <- Packages.initPackages dflags1'
  logPutStrLn $ "packagesIds: " ++ (showSDocDump $ ppr $ packageIds)
  GHC.setSessionDynFlags session dflags2{GHC.hscTarget=GHC.HscInterpreted}
  yiContextL session
  return Kernel { 
                 getSessionDynFlags = GHC.getSessionDynFlags session,
                 setSessionDynFlags = GHC.setSessionDynFlags session,
                 compileExpr = GHC.compileExpr session,
                 yiContext = yiContextL session,
                 loadAllTargets = GHC.load session GHC.LoadAllTargets,
                 addTarget = addTargetL session
                }


-- | Dynamically start Yi. 
startYi :: Kernel -> IO ()
startYi kernel = GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
  result <- compileExpr kernel ("Yi.main :: Yi.Kernel -> IO ()") 
  -- coerce the interpreted expression, so we check that we are not making an horrible mistake.
  logPutStrLn "Starting Yi!"
  case result of
    Nothing -> error "Could not compile Yi.main!"
    Just x -> do let (x' :: Kernel -> IO ()) = unsafeCoerce# x
                 x' kernel
                 return ()

-- | Dynamic evaluation
eval :: Kernel -> String -> IO GHC.HValue
eval kernel expr = do
  result <- compileExpr kernel expr
  case result of
    Nothing -> error $ "Could not compile expr: " ++ expr
    Just x -> return x

yiContextL :: GHC.Session -> IO ()
yiContextL session = do
  preludeModule <- GHC.findModule session (GHC.mkModuleName "Prelude") Nothing
  yiModule <- GHC.findModule session (GHC.mkModuleName "Yi.Yi") Nothing -- this module re-exports all useful stuff.
  GHC.setContext session [] [preludeModule, yiModule]

addTargetL :: GHC.Session -> String -> IO ()
addTargetL session targetId = do
  configTarget <- GHC.guessTarget targetId Nothing
  GHC.addTarget session configTarget


{- 
Maybe useful in the future...

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

-}
  

