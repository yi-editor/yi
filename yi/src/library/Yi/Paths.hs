{-# LANGUAGE NoMonomorphismRestriction #-}
module Yi.Paths(
   getEvaluatorContextFilename
  ,getConfigFilename
  ,getArticleDbFilename
  ,getPersistentStateFilename
  ,getConfigDir

  ,getConfigPath
  ,getDataPath
) where

import System.Directory(getHomeDirectory, getAppUserDataDirectory, -- TODO: phase out in favour of xdg-dir
                        doesFileExist, doesDirectoryExist,
                        createDirectoryIfMissing)
import System.FilePath((</>), takeDirectory, FilePath)
import Control.Applicative((<$>))
import Control.Monad(filterM, Monad)
import Control.Monad.Trans(liftIO, MonadIO)
import qualified System.Environment.XDG.BaseDir as XDG

appUserDataCond dirQuery = liftIO $
  do oldDir <- getAppUserDataDirectory "yi"
     newDir <- dirQuery "yi"
     oldDirExists <- doesDirectoryExist oldDir
     newDirExists <- doesDirectoryExist newDir
     return $ if newDirExists -- overrides old-style
                 then newDir
                 else if oldDirExists -- old-style exists, use it
                        then oldDir
                        else newDir -- none exists, use new style

-- TODO: These would be replaced by xdg-basedir equivalents
getConfigDir = appUserDataCond XDG.getUserConfigDir
getDataDir   = appUserDataCond XDG.getUserDataDir

-- | Given a path relative to application data directory,
--   this function finds a path to a given data file.
getDataPath :: (MonadIO m) => FilePath -> m FilePath
getDataPath   fp = getDataDir >>= (return . (</> fp))
-- | Given a path relative to application configuration directory,
--   this function finds a path to a given configuration file.
getConfigPath :: (MonadIO m) => FilePath -> m FilePath
getConfigPath fp = getConfigDir >>= (return . (</> fp))

-- Note: Dyre also uses XDG cache dir - that would be:
--getCachePath = getPathHelper XDG.getUserCacheDirectory

-- Below are all points that are used in Yi code (to keep it clean.)
getEvaluatorContextFilename, getConfigFilename, getArticleDbFilename, getPersistentStateFilename :: (MonadIO m) => m FilePath

-- | Get Yi master configuration script.
getConfigFilename           = getConfigPath "yi.hs"

-- | Get articles.db database of locations to visit (for Yi.IReader.)
getArticleDbFilename       = getConfigPath "articles.db"

-- | Get path to Yi history that stores state between runs.
getPersistentStateFilename  = getDataPath   "history"

-- | Get path to environment file that defines namespace used by Yi
--   command evaluator.
getEvaluatorContextFilename = getConfigPath $ "local" </> "Env.hs"

