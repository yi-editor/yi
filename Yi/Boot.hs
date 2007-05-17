module Yi.Boot where

import Yi.Debug hiding (error)
import Yi.Kernel

import System.Directory     ( getHomeDirectory )

import qualified GHC
import qualified Packages
import qualified DynFlags
import qualified Module
import Outputable
import Control.Monad

import GHC.Exts ( unsafeCoerce# )


-- the path of our GHC installation
path :: FilePath
path = GHC_LIBDIR -- See Setup.hs

-- | Create a suitable Yi Kernel, via a GHC session.
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
                 setContextAfterLoad = setContextAfterLoadL session,
                 getNamesInScope = GHC.getNamesInScope session,
                 getRdrNamesInScope = GHC.getRdrNamesInScope session,
                 nameToString = Outputable.showSDoc . Outputable.ppr,
                 isLoaded = GHC.isLoaded session,
                 mkModuleName = Module.mkModuleName,
                 getModuleGraph = GHC.getModuleGraph session
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

setContextAfterLoadL :: GHC.Session -> IO [GHC.Module]
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
   _summary `matches` _target
	= False

