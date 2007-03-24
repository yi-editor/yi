

-- | This module is the interface to GHC (interpreter).  It knows
-- nothing about Yi (at the haskell level; it can know names of
-- modules/functions at strings)

module Yi.Kernel (initialize, Kernel(..), eval, startYi, moduleName) where

import Yi.Debug hiding (error)

import System.Directory     ( getHomeDirectory )

import qualified GHC
import qualified Packages
import qualified DynFlags
import qualified PackageConfig
import qualified Linker
import qualified Module
import Outputable
import Control.Monad

import GHC.Exts ( unsafeCoerce# )


-- | GHC API Kernel. 
-- Calls to the GHC API must go though this type. (Because of the use "global variables" in GHC I imagine)
-- ie. the simpler approach of passing just the GHC. Session does not work
data Kernel = Kernel
    {
     getSessionDynFlags :: IO GHC.DynFlags,
     setSessionDynFlags :: GHC.DynFlags -> IO [Packages.PackageId],
     compileExpr :: String -> IO (Maybe GHC.HValue),
     guessTarget :: String -> Maybe GHC.Phase -> IO GHC.Target,
     setTargets :: [GHC.Target] -> IO (),
     loadAllTargets :: IO GHC.SuccessFlag,
     findModule :: String -> IO GHC.Module,
     setContext :: [GHC.Module]	-- ^ entire top level scope of these modules
	        -> [GHC.Module]	-- ^ exports only of these modules
	        -> IO (),
     setContextAfterLoad :: IO [GHC.Module]
     -- getModuleGraph
     -- isLoaded
     -- ms_mod_name
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
                                                           "-i", -- clear the search directory (don't look in ./)
                                                           "-i" ++ home ++ "/.yi" -- We look for source files in ~/.yi
--                                                           ,"-v"
                                                          ]
  (dflags2, packageIds) <- Packages.initPackages dflags1'
  logPutStrLn $ "packagesIds: " ++ (showSDocDump $ ppr $ packageIds)
  GHC.setSessionDynFlags session dflags2{GHC.hscTarget=GHC.HscInterpreted}
  return Kernel { 
                 getSessionDynFlags = GHC.getSessionDynFlags session,
                 setSessionDynFlags = GHC.setSessionDynFlags session,
                 compileExpr = GHC.compileExpr session,
                  loadAllTargets = GHC.load session GHC.LoadAllTargets,
                 setTargets = GHC.setTargets session,
                 guessTarget = GHC.guessTarget,
                 findModule = \s -> GHC.findModule session (GHC.mkModuleName s) Nothing,
                 setContext = GHC.setContext session,
                 setContextAfterLoad = setContextAfterLoadL session
                }


-- | Dynamically start Yi. 
startYi :: Kernel -> IO ()
startYi kernel = GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
  result <- compileExpr kernel ("Yi.main :: Yi.Kernel -> Prelude.IO ()") 
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
  

setContextAfterLoadL session = do
  preludeModule <- GHC.findModule session (GHC.mkModuleName "Prelude") Nothing
  yiModule <- GHC.findModule session (GHC.mkModuleName "Yi.Yi") Nothing -- this module re-exports all useful stuff.
  graph <- GHC.getModuleGraph session
  graph' <- filterM (GHC.isLoaded session . GHC.ms_mod_name) graph
  targets <- GHC.getTargets session
  let targets' = [ m | Just m <- map (findTarget graph') targets ]
      modules = map GHC.ms_mod targets'
      context = preludeModule:yiModule:modules
  GHC.setContext session [] context
  return context
 where
   findTarget ms t
    = case filter (`matches` t) ms of
	[]    -> Nothing
	(m:_) -> Just m

   summary `matches` GHC.Target (GHC.TargetModule m) _
	= GHC.ms_mod_name summary == m
   summary `matches` GHC.Target (GHC.TargetFile f _) _ 
	| Just f' <- GHC.ml_hs_file (GHC.ms_location summary)	= f == f'
   summary `matches` target
	= False

moduleName :: GHC.Module -> String
moduleName = Module.moduleNameString . Module.moduleName