{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeOperators #-}
-- Copyright (c) 2008 Jean-Philippe Bernardy
-- | Various high-level functions to further classify.
module Yi.Command
where

{- Standard Library Module Imports -}
import Data.Binary
import System.Exit
  ( ExitCode( ExitSuccess,ExitFailure ) )
import Control.OldException
import Control.Monad.Trans (MonadIO (..))
{- External Library Module Imports -}
{- Local (yi) module imports -}

import Prelude ()
import Yi.Core
import Yi.MiniBuffer
import qualified Yi.Mode.Compilation as Compilation
import Yi.Process
import Yi.Templates
  ( addTemplate
  , templateNames
  )
import Yi.UI.Common 
import qualified Yi.Mode.Interactive as Interactive
import qualified Data.Rope as R

---------------------------
-- | Changing the buffer name quite useful if you have
-- several the same. This also breaks the relation with the file.

changeBufferNameE :: YiM ()
changeBufferNameE =
  withMinibufferFree "New buffer name:" strFun
  where
  strFun :: String -> YiM ()
  strFun = withBuffer . putA identA . Left

----------------------------
-- | shell-command
shellCommandE :: YiM ()
shellCommandE = do
    withMinibufferFree "Shell command:" $ \cmd -> do
      (cmdOut,cmdErr,exitCode) <- liftIO $ runShellCommand cmd
      case exitCode of
        ExitSuccess -> withEditor $ newBufferE (Left "Shell Command Output") (R.fromString cmdOut) >> return ()
        -- FIXME: here we get a string and convert it back to utf8; this indicates a possible bug.
        ExitFailure _ -> msgEditor cmdErr

----------------------------
-- Cabal-related commands
-- TODO: rename to "BuildBuffer" or something.
newtype CabalBuffer = CabalBuffer {cabalBuffer :: Maybe BufferRef}
    deriving (Initializable, Typeable, Binary)


----------------------------
-- | cabal-configure
cabalConfigureE :: CommandArguments -> YiM ()
cabalConfigureE = cabalRun "configure" configureExit

configureExit :: Either Exception ExitCode -> YiM ()
configureExit (Right ExitSuccess) = reloadProjectE "."
configureExit _ = return ()


reloadProjectE :: String -> YiM ()
reloadProjectE s = withUI $ \ui -> reloadProject ui s

-- | Run the given commands with args and pipe the ouput into the build buffer,
-- which is shown in an other window.
buildRun :: String -> [String] -> (Either Exception ExitCode -> YiM x) -> YiM ()
buildRun cmd args onExit = withOtherWindow $ do
   b <- startSubprocess cmd args onExit
   withEditor $ do
       maybeM deleteBuffer =<< cabalBuffer <$> getDynamic
       setDynamic $ CabalBuffer $ Just b
       withBuffer0 $ setMode Compilation.mode
   return ()

makeBuild :: CommandArguments -> YiM ()
makeBuild (CommandArguments args) = buildRun "make" args (const $ return ())

cabalRun :: String -> (Either Exception ExitCode -> YiM x) -> CommandArguments -> YiM ()
cabalRun cmd onExit (CommandArguments args) = buildRun "cabal" (cmd:args) onExit

-----------------------
-- | cabal-build
cabalBuildE :: CommandArguments -> YiM ()
cabalBuildE = cabalRun "build" (const $ return ())


shell :: YiM BufferRef
shell = do
    sh <- io shellFileName
    Interactive.interactive sh ["-i"]
    -- use the -i option for interactive mode (assuming bash)

-- | Search the source files in the project.
searchSources :: String ::: RegexTag -> YiM ()
searchSources = grepFind (Doc "*.hs")

-- | Perform a find+grep operation
grepFind :: String ::: FilePatternTag -> String ::: RegexTag -> YiM ()
grepFind (Doc pattern) (Doc searched) = withOtherWindow $ do
    startSubprocess "find" [".", 
                            "-name", "_darcs", "-prune", "-o", 
                            "-name", pattern, "-exec", "grep", "-Hnie", searched, "{}", ";"] (const $ return ())
    withBuffer $ setMode Compilation.mode
    return ()

     
-- | Inserting a template from the templates defined in Yi.Templates
insertTemplate :: YiM ()
insertTemplate = withMinibuffer "template-name:" (const $ return templateNames) $ withEditor . addTemplate
