{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeOperators #-}
-- Copyright (c) 2008 Jean-Philippe Bernardy
-- | Various high-level functions to further classify.
module Yi.Command where

{- Standard Library Module Imports -}
import Data.Binary
import System.Exit
  ( ExitCode( ExitSuccess,ExitFailure ) )
import Control.Exception(SomeException)
import Control.Monad.Base
{- External Library Module Imports -}
{- Local (yi) module imports -}

import Control.Applicative
import Control.Monad
import Control.Lens
import Yi.Core
import Yi.MiniBuffer
import qualified Yi.Mode.Compilation as Compilation
import Yi.Process
import Yi.UI.Common
import qualified Yi.Mode.Interactive as Interactive
import qualified Data.Rope as R
import Data.Default
import Data.Typeable
import Yi.Utils
import Yi.Monad

---------------------------
-- | Changing the buffer name quite useful if you have
-- several the same. This also breaks the relation with the file.

changeBufferNameE :: YiM ()
changeBufferNameE =
  withMinibufferFree "New buffer name:" strFun
  where
  strFun :: String -> YiM ()
  strFun = withBuffer . assign identA . Left

----------------------------
-- | shell-command with argument prompt
shellCommandE :: YiM ()
shellCommandE =
    withMinibufferFree "Shell command:" shellCommandV

----------------------------
-- | shell-command with a known argument
shellCommandV :: String -> YiM ()
shellCommandV cmd = do
      (exitCode,cmdOut,cmdErr) <- liftBase $ runShellCommand cmd
      case exitCode of
        ExitSuccess -> if length (filter (== '\n') cmdOut) > 17
                       then withEditor . void $ -- see GitHub issue #477
                              newBufferE (Left "Shell Command Output")
                                         (R.fromString cmdOut)
                       else msgEditor $ case cmdOut of
                         "" -> "(Shell command with no output)"
                         -- Drop trailing newline from output
                         xs -> if last xs == '\n' then init xs else xs
        -- FIXME: here we get a string and convert it back to utf8;
        -- this indicates a possible bug.
        ExitFailure _ -> msgEditor cmdErr

----------------------------
-- Cabal-related commands
-- TODO: rename to "BuildBuffer" or something.
newtype CabalBuffer = CabalBuffer {cabalBuffer :: Maybe BufferRef}
    deriving (Default, Typeable, Binary)

instance YiVariable CabalBuffer

----------------------------
-- | cabal-configure
cabalConfigureE :: CommandArguments -> YiM ()
cabalConfigureE = cabalRun "configure" configureExit

configureExit :: Either SomeException ExitCode -> YiM ()
configureExit (Right ExitSuccess) = reloadProjectE "."
configureExit _ = return ()


reloadProjectE :: String -> YiM ()
reloadProjectE s = withUI $ \ui -> reloadProject ui s

-- | Run the given commands with args and pipe the ouput into the build buffer,
-- which is shown in an other window.
buildRun :: String -> [String] -> (Either SomeException ExitCode -> YiM x) -> YiM ()
buildRun cmd args onExit = withOtherWindow $ do
   b <- startSubprocess cmd args onExit
   withEditor $ do
       maybeM deleteBuffer =<< cabalBuffer <$> getDynamic
       setDynamic $ CabalBuffer $ Just b
       withBuffer0 $ setMode Compilation.mode
   return ()

makeBuild :: CommandArguments -> YiM ()
makeBuild (CommandArguments args) = buildRun "make" args (const $ return ())

cabalRun :: String -> (Either SomeException ExitCode -> YiM x) -> CommandArguments -> YiM ()
cabalRun cmd onExit (CommandArguments args) = buildRun "cabal" (cmd:args) onExit

-----------------------
-- | cabal-build
cabalBuildE :: CommandArguments -> YiM ()
cabalBuildE = cabalRun "build" (const $ return ())


shell :: YiM BufferRef
shell = do
    sh <- io shellFileName
    Interactive.spawnProcess sh ["-i"]
    -- use the -i option for interactive mode (assuming bash)

-- | Search the source files in the project.
searchSources :: String ::: RegexTag -> YiM ()
searchSources = grepFind (Doc "*.hs")

-- | Perform a find+grep operation
grepFind :: String ::: FilePatternTag -> String ::: RegexTag -> YiM ()
grepFind (Doc filePattern) (Doc searchedRegex) = withOtherWindow $ do
    void $ startSubprocess "find" [".",
                                      "-name", "_darcs", "-prune", "-o",
                                      "-name", filePattern, "-exec", "grep", "-Hnie", searchedRegex, "{}", ";"] (const $ return ())
    withBuffer $ setMode Compilation.mode
    return ()
