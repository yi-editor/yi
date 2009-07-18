module HConf.Paths where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))
import System.Info (arch, os)
import HConf.Utils (io)

-- | Return the path to @~\/.Project@.
getProjectDir :: MonadIO m => String -> m FilePath
getProjectDir projectName = io $ getAppUserDataDirectory projectName

-- | Return the full path to the config file
getConfigFile :: (MonadIO m, Functor m) => String -> m FilePath
getConfigFile projectName = (</> projectName ++ ".hs") <$> getProjectDir projectName

-- | Return the full path to the compiled config file (executable)
getCustomExecutable :: (MonadIO m, Functor m) => String -> m FilePath
getCustomExecutable projectName = (</> projectName ++ "-" ++ arch ++ "-" ++ os) <$> 
                                  getProjectDir projectName

-- | Return the full path to the status file
getStateFile :: (MonadIO m, Functor m) => String -> m FilePath
getStateFile projectName = (</> "status") <$> getProjectDir projectName

-- | Return the full path to the errors file
getErrorsFile :: (MonadIO m, Functor m) => String -> m FilePath
getErrorsFile projectName = (</> projectName ++ ".errors") <$> getProjectDir projectName

