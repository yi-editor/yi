

-- | This module is the interface to GHC (interpreter).  It knows
-- nothing about Yi (at the haskell level; it can know names of
-- modules/functions at strings)

module Yi.Kernel (Kernel(..), eval, moduleName, moduleNameString, ms_mod_name) where

import Control.Monad
import Outputable
import qualified GHC
import qualified Linker
import qualified Module
import qualified Packages

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
     setContext :: [GHC.Module]	-- ^ entire top level scope of these modules
	        -> [GHC.Module]	-- ^ exports only of these modules
	        -> IO (),
     setContextAfterLoad :: IO [GHC.Module],
     getNamesInScope :: IO [GHC.Name],
     getRdrNamesInScope :: IO [GHC.RdrName],
     mkModuleName :: String -> GHC.ModuleName,
     isLoaded :: GHC.ModuleName -> IO Bool,
     nameToString :: forall a. Outputable a => a -> String,
     getModuleGraph :: IO GHC.ModuleGraph
    }

-- | Dynamic evaluation
eval :: Kernel -> String -> IO GHC.HValue
eval kernel expr = do
  result <- compileExpr kernel expr
  case result of
    Nothing -> error $ "Could not compile expr: " ++ expr
    Just x -> return x

 
moduleName :: GHC.Module -> GHC.ModuleName
moduleName = Module.moduleName

moduleNameString :: GHC.ModuleName -> String
moduleNameString = Module.moduleNameString

ms_mod_name :: GHC.ModSummary -> GHC.ModuleName
ms_mod_name = GHC.ms_mod_name
