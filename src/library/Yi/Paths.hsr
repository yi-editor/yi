{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Yi.Paths ( getEvaluatorContextFilename
                , getConfigFilename
                , getConfigModules
                , getArticleDbFilename
                , getPersistentStateFilename
                , getConfigDir
                , getConfigPath
                , getCustomConfigPath
                , getDataPath
                ) where

import           Control.Monad.Base             (MonadBase, liftBase)
import           System.Directory               (createDirectoryIfMissing,
                                                 doesDirectoryExist,
                                                 getAppUserDataDirectory)
import qualified System.Environment.XDG.BaseDir as XDG (getUserConfigDir, getUserDataDir)
import           System.FilePath                ((</>))

appUserDataCond ::(MonadBase IO m) => (String -> IO FilePath) -> m FilePath
appUserDataCond dirQuery = liftBase $
  do oldDir <- getAppUserDataDirectory "yi"
     newDir <- dirQuery "yi"
     oldDirExists <- doesDirectoryExist oldDir
     newDirExists <- doesDirectoryExist newDir
     if newDirExists -- overrides old-style
        then return newDir
        else if oldDirExists -- old-style exists, use it
               then return oldDir
               else do createDirectoryIfMissing True newDir -- none exists, use new style, but create it
                       return newDir

getConfigDir ::(MonadBase IO m) => m FilePath
getConfigDir = appUserDataCond XDG.getUserConfigDir

getDataDir ::(MonadBase IO m) => m FilePath
getDataDir = appUserDataCond XDG.getUserDataDir

-- | Given a path relative to application data directory,
--   this function finds a path to a given data file.
getDataPath :: (MonadBase IO m) => FilePath -> m FilePath
getDataPath fp = fmap (</> fp) getDataDir

-- | Given a path relative to application configuration directory,
--   this function finds a path to a given configuration file.
getConfigPath :: MonadBase IO m => FilePath -> m FilePath
getConfigPath = getCustomConfigPath getConfigDir

-- | Given an action that retrieves config path, and a path relative to it,
-- this function joins the two together to create a config file path.
getCustomConfigPath :: MonadBase IO m => m FilePath -> FilePath -> m FilePath
getCustomConfigPath cd fp = (</> fp) `fmap` cd

-- Note: Dyre also uses XDG cache dir - that would be:
--getCachePath = getPathHelper XDG.getUserCacheDirectory

-- Below are all points that are used in Yi code (to keep it clean.)
getEvaluatorContextFilename, getConfigFilename, getConfigModules,
    getArticleDbFilename, getPersistentStateFilename :: (MonadBase IO m) => m FilePath

-- | Get Yi master configuration script.
getConfigFilename = getConfigPath "yi.hs"

getConfigModules = getConfigPath "modules"

-- | Get articles.db database of locations to visit (for Yi.IReader.)
getArticleDbFilename = getConfigPath "articles.db"

-- | Get path to Yi history that stores state between runs.
getPersistentStateFilename = getDataPath "history"

-- | Get path to environment file that defines namespace used by Yi
--   command evaluator.
getEvaluatorContextFilename = getConfigPath $ "local" </> "Env.hs"
