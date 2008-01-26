{-# LANGUAGE MagicHash, Rank2Types #-}

-- | This module is the interface to GHC (interpreter).  It knows
-- nothing about Yi (at the haskell level; it can know names of
-- modules or functions as strings)

#ifdef DYNAMIC

module Yi.Kernel (Kernel(..), evalHValue, evalMono, moduleName, moduleNameString, ms_mod_name) where

import Control.Monad
import Outputable
import qualified GHC
import qualified Linker
import qualified Module
import qualified Packages
import GHC.Exts ( unsafeCoerce# )

-- | GHC API Kernel.
-- Calls to the GHC API must go though this type. (Because of the use "global variables" in GHC I imagine)
-- ie. the simpler approach of passing just the GHC session does not work.


data Kernel = Kernel
    {
     getSessionDynFlags :: IO GHC.DynFlags,
     setSessionDynFlags :: GHC.DynFlags -> IO [Packages.PackageId],
     compileExpr :: String -> IO (Maybe GHC.HValue),
     guessTarget :: String -> Maybe GHC.Phase -> IO GHC.Target,
     setTargets :: [GHC.Target] -> IO (),
     loadAllTargets :: IO GHC.SuccessFlag,
     findModule :: String -> IO GHC.Module,
     setContext :: [GHC.Module]
                -> [GHC.Module]
                -> IO (), -- ^ entire top level scope of 1st arg modules; exports only of 2nd arg modules
     setContextAfterLoad :: IO [GHC.Module],
     getNamesInScope :: IO [GHC.Name],
     getRdrNamesInScope :: IO [GHC.RdrName],
     mkModuleName :: String -> GHC.ModuleName,
     isLoaded :: GHC.ModuleName -> IO Bool,
     nameToString :: Outputable a => a -> String,
     getModuleGraph :: IO GHC.ModuleGraph,
     loadObjectFile :: String -> IO (),
     libraryDirectory :: String
    }

-- | Dynamic evaluation to an HValue; fails in the monad if compilation failed.
evalHValue :: Monad m => Kernel -> String -> IO (m GHC.HValue)
evalHValue kernel expr = do
  result <- compileExpr kernel expr
  return $ case result of
    Nothing -> fail $ "Could not compile: " ++ expr
    Just x -> return x


-- | Evaluate a monomorphic value dynamically. Fails in the monad if compilation failed.
evalMono :: Monad m => Kernel -> String -> IO (m a)
evalMono kernel expr = liftM (liftM (unsafeCoerce# :: GHC.HValue -> a)) (evalHValue kernel expr)

-- evalTyp :: Typeable a => Kernel -> String -> IO (m a)



moduleName :: GHC.Module -> GHC.ModuleName
moduleName = Module.moduleName

moduleNameString :: GHC.ModuleName -> String
moduleNameString = Module.moduleNameString

ms_mod_name :: GHC.ModSummary -> GHC.ModuleName
ms_mod_name = GHC.ms_mod_name

#else

module Yi.Kernel where
data Kernel = Kernel

#endif
