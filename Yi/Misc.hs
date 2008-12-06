{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
-- Copyright (c) 2008 Jean-Philippe Bernardy
-- | Various high-level functions to further classify.
module Yi.Misc
where

{- Standard Library Module Imports -}
import Data.Binary
import Data.List
  ( isPrefixOf
  , (\\)
  , filter
  )
import System.Exit
  ( ExitCode( ExitSuccess,ExitFailure ) )
import System.FriendlyPath
import System.FilePath
  ( takeDirectory
  , (</>)
  , addTrailingPathSeparator
  , hasTrailingPathSeparator
  , takeFileName
  )
import System.Directory
  ( doesDirectoryExist
  , getDirectoryContents
  , getCurrentDirectory
  )
import Control.OldException
import Control.Monad.Trans (MonadIO (..))
{- External Library Module Imports -}
{- Local (yi) module imports -}

import Prelude (words)
import Yi.Monad
import Yi.Core
import Yi.MiniBuffer
import qualified Yi.Mode.Compilation as Compilation
import Yi.Process
import Yi.Templates
  ( addTemplate
  , templateNames
  )
import Yi.UI.Common 
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Yi.Mode.Interactive as Interactive

---------------------------
-- | Changing the buffer name quite useful if you have
-- several the same.

changeBufferNameE :: YiM ()
changeBufferNameE =
  withMinibufferFree "New buffer name:" strFun
  where
  strFun :: String -> YiM ()
  strFun = withBuffer . putA nameA

----------------------------
-- | shell-command
shellCommandE :: YiM ()
shellCommandE = do
    withMinibufferFree "Shell command:" $ \cmd -> do
      (cmdOut,cmdErr,exitCode) <- liftIO $ runShellCommand cmd
      case exitCode of
        ExitSuccess -> withEditor $ newBufferE "*Shell Command Output*" (LazyUTF8.fromString cmdOut) >> return ()
        -- FIXME: here we get a string and convert it back to utf8; this indicates a possible bug.
        ExitFailure _ -> msgEditor cmdErr

----------------------------
-- Cabal-related commands
newtype CabalBuffer = CabalBuffer {cabalBuffer :: Maybe BufferRef}
    deriving (Initializable, Typeable, Binary)


----------------------------
-- | cabal-configure
cabalConfigureE :: YiM ()
cabalConfigureE = withMinibufferFree "Configure args:" $ cabalRun "configure" configureExit

configureExit :: Either Exception ExitCode -> YiM ()
configureExit (Right ExitSuccess) = reloadProjectE "."
configureExit _ = return ()


reloadProjectE :: String -> YiM ()
reloadProjectE s = withUI $ \ui -> reloadProject ui s

cabalRun :: String -> (Either Exception ExitCode -> YiM x) -> String -> YiM ()
cabalRun cmd onExit args = withOtherWindow $ do
   b <- startSubprocess "cabal" (cmd:words args) onExit
   withEditor $ do
       maybeM deleteBuffer =<< cabalBuffer <$> getDynamic
       setDynamic $ CabalBuffer $ Just b
       withBuffer0 $ setMode Compilation.mode
   return ()

-----------------------
-- | cabal-build
cabalBuildE :: YiM ()
cabalBuildE = withMinibufferFree "Build args:" $ cabalRun "build" (\_ -> return ())


shell :: YiM BufferRef
shell = do
    sh <- io shellFileName
    Interactive.interactive sh ["-i"]
    -- use the -i option for interactive mode (assuming bash)

-- | Search the haskell files in the project.
grepFind :: String -> YiM ()
grepFind searched = withOtherWindow $ do
    startSubprocess "find" [".", 
                            "-name", "_darcs", "-prune", "-o", 
                            "-name", "*.hs", "-exec", "grep", "-Hnie", searched, "{}", ";"] (\_ -> return ())
    withBuffer $ setMode Compilation.mode
    return ()
     
-- | Inserting a template from the templates defined in Yi.Templates
insertTemplate :: YiM ()
insertTemplate =
  withMinibuffer "template-name:" (\_ -> return templateNames) $ withEditor . addTemplate

-- | Given a possible starting path (which if not given defaults to
--   the current directory) and a fragment of a path we find all
--   files within the given (or current) directory which can complete
--   the given path fragment.
--   We return a pair of both directory plus the filenames on their own
--   that is without their directories. The reason for this is that if
--   we return all of the filenames then we get a 'hint' which is way too
--   long to be particularly useful.
getAppropriateFiles :: Maybe String -> String -> YiM (String, [ String ])
getAppropriateFiles start s = do
  curDir <- case start of
            Nothing -> do bufferPath <- withBuffer $ getA fileA
                          liftIO $ getFolder bufferPath
            (Just path) -> return path
  let sDir = if hasTrailingPathSeparator s then s else takeDirectory s
      searchDir = if null sDir then curDir
                  else if isAbsolute' sDir then sDir
                  else curDir </> sDir
  searchDir' <- liftIO $ expandTilda searchDir
  let fixTrailingPathSeparator f = do
                       isDir <- doesDirectoryExist (searchDir' </> f)
                       return $ if isDir then addTrailingPathSeparator f else f
  files <- liftIO $ getDirectoryContents searchDir'
  -- Remove the two standard current-dir and parent-dir as we do not
  -- need to complete or hint about these as they are known by users.
  let files' = files \\ [ ".", ".." ]
  fs <- liftIO $ mapM fixTrailingPathSeparator files'
  let matching = filter (isPrefixOf $ takeFileName s) fs
  return (sDir, matching)

-- | Given a path, trim the file name bit if it exists.  If no path
--   given, return current directory.
getFolder :: Maybe String -> IO String
getFolder Nothing     = getCurrentDirectory
getFolder (Just path) = do
  isDir <- doesDirectoryExist path
  let dir = if isDir then path else takeDirectory path
  if null dir then getCurrentDirectory else return dir


-- | Given a possible path and a prefix, return matching file names.
matchingFileNames :: Maybe String -> String -> YiM [String]
matchingFileNames start s = do
  (sDir, files) <- getAppropriateFiles start s
  return $ fmap (sDir </>) files

adjBlock :: Int -> BufferM ()
adjBlock x = withSyntaxB (\m s -> modeAdjustBlock m s x)

-- | A simple wrapper to adjust the current indentation using
-- the mode specific indentation function but according to the
-- given indent behaviour.
adjIndent :: IndentBehaviour -> BufferM ()
adjIndent ib = withSyntaxB (\m s -> modeIndent m s ib)

