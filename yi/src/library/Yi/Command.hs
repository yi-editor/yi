{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Command
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Various high-level functions to further classify.

module Yi.Command where

import           Control.Applicative
import           Control.Exception(SomeException)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Base
import           Data.Binary
import           Data.Default
import qualified Data.Text as T
import           Data.Typeable
import           System.Exit (ExitCode(..))
import           Yi.Buffer
import           Yi.Core (startSubprocess)
import           Yi.Dynamic
import           Yi.Editor
import           Yi.Keymap
import           Yi.MiniBuffer
import qualified Yi.Mode.Compilation as Compilation
import qualified Yi.Mode.Interactive as Interactive
import           Yi.Monad
import           Yi.Process
import qualified Yi.Rope as R
import           Yi.UI.Common
import           Yi.Utils

---------------------------
-- | Changing the buffer name quite useful if you have
-- several the same. This also breaks the relation with the file.

changeBufferNameE :: YiM ()
changeBufferNameE = withMinibufferFree "New buffer name:" strFun
  where
    strFun :: T.Text -> YiM ()
    strFun = withCurrentBuffer . assign identA . MemBuffer

----------------------------
-- | shell-command with argument prompt
shellCommandE :: YiM ()
shellCommandE = withMinibufferFree "Shell command:" shellCommandV

----------------------------
-- | shell-command with a known argument
shellCommandV :: T.Text -> YiM ()
shellCommandV cmd = do
  (exitCode,cmdOut,cmdErr) <- liftBase $ runShellCommand (T.unpack cmd)
  case exitCode of
    ExitSuccess -> if length (filter (== '\n') cmdOut) > 17
                   then withEditor . void $ -- see GitHub issue #477
                          newBufferE (MemBuffer "Shell Command Output")
                                     (R.fromString cmdOut)
                   else printMsg $ case T.pack cmdOut of
                     "" -> "(Shell command with no output)"
                     -- Drop trailing newline from output
                     xs -> if T.last xs == '\n' then T.init xs else xs
    -- FIXME: here we get a string and convert it back to utf8;
    -- this indicates a possible bug.
    ExitFailure _ -> printMsg $ T.pack cmdErr

----------------------------
-- Cabal-related commands
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
buildRun :: T.Text -> [T.Text] -> (Either SomeException ExitCode -> YiM x) -> YiM ()
buildRun cmd args onExit = withOtherWindow $ do
   b <- startSubprocess (T.unpack cmd) (T.unpack <$> args) onExit
   maybeM deleteBuffer =<< cabalBuffer <$> getDynamic
   setDynamic $ CabalBuffer $ Just b
   withCurrentBuffer $ setMode Compilation.mode
   return ()

makeBuild :: CommandArguments -> YiM ()
makeBuild (CommandArguments args) = buildRun "make" args (const $ return ())

cabalRun :: T.Text -> (Either SomeException ExitCode -> YiM x) -> CommandArguments -> YiM ()
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
    withCurrentBuffer $ setMode Compilation.mode
    return ()
