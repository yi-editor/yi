module Yi.Boot where

import Yi.Debug hiding (error)
import Yi.Kernel

import System.Environment       ( getArgs )
import System.Directory     ( getHomeDirectory )
import System.Console.GetOpt

import qualified GHC
import qualified Packages
import qualified DynFlags
import qualified Module
import Outputable
import Control.Monad

import GHC.Exts ( unsafeCoerce# )

data Opts = Flavour String
          | Libdir String

options :: [OptDescr Opts]
options = [
    Option ['f']  ["flavour"] (ReqArg Flavour "gtk|vty") "Select flavour",
    Option ['B']  ["libdir"]  (ReqArg Libdir "libdir") "Path to runtime libraries"
    ]


forcedFlavour (Flavour x) = Just x
forcedFlavour _ = Nothing

override def Nothing = def
override def (Just x) = x

-- the path of our GHC installation
path :: FilePath
path = GHC_LIBDIR -- See Setup.hs

-- | Create a suitable Yi Kernel, via a GHC session.
-- Also return the non-boot flags.
initialize :: IO (Kernel, [String])
initialize = GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
  -- here we have to make an early choice between vty and gtk.
  bootArgs <- getArgs
  let (bootFlags, otherArgs, _) = getOpt Permute options bootArgs
  let flavour = foldl override "vty" $ map forcedFlavour bootFlags
  let libDirs = [d | Libdir d <- bootFlags] ++ [".", flavour]

  session <- GHC.newSession GHC.Interactive (Just path)
  dflags1 <- GHC.getSessionDynFlags session

  home <- getHomeDirectory
  (dflags1',_otherFlags) <- GHC.parseDynamicFlags dflags1 $ [
                                                           "-package ghc", "-fglasgow-exts", "-cpp", 
                                                           "-i", -- clear the search directory (don't look in ./)
                                                           "-i" ++ home ++ "/.yi", -- First, we look for source files in ~/.yi
                                                           "-i.", -- then look in ./
                                                           "-i" ++ flavour,
                                                           "-hidir " ++ flavour,
                                                           "-odir " ++ flavour
                                                          ] ++ map ("-i"++) libDirs
  (dflags2, packageIds) <- Packages.initPackages dflags1'
  logPutStrLn $ "packagesIds: " ++ (showSDocDump $ ppr $ packageIds)
  GHC.setSessionDynFlags session dflags2{GHC.hscTarget=GHC.HscInterpreted}
  return (Kernel { 
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
                }, otherArgs)


-- | Dynamically start Yi. 
startYi :: Kernel -> [String] -> IO ()
startYi kernel args = GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
  t <- (guessTarget kernel) "Yi/Main.hs" Nothing
  (setTargets kernel) [t]
  loadAllTargets kernel
  result <- compileExpr kernel ("Yi.Main.main :: Yi.Kernel.Kernel -> [Prelude.String] -> Prelude.IO ()") 
  -- coerce the interpreted expression, so we check that we are not making an horrible mistake.
  logPutStrLn "Starting Yi!"
  case result of
    Nothing -> error "Could not compile Yi.main!"
    Just x -> do let (x' :: Kernel -> [String] -> IO ()) = unsafeCoerce# x
                 x' kernel args
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

