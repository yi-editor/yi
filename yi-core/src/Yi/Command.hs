{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
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

import           Control.Exception   (SomeException)
import           Lens.Micro.Platform ((.=))
import           Control.Monad       (void)
import           Control.Monad.Base  (liftBase)
import           Data.Binary         (Binary)
import           Data.Default        (Default)
import qualified Data.Text           as T (Text, init, filter, last, length, unpack)
import           Data.Typeable       (Typeable)
import           System.Exit         (ExitCode (..))
import           Yi.Buffer           (BufferId (MemBuffer), BufferRef, identA)
import           Yi.Core             (startSubprocess)
import           Yi.Editor
import           Yi.Keymap           (YiM, withUI)
import           Yi.MiniBuffer
import           Yi.Monad            (maybeM)
import           Yi.Process          (runShellCommand)
import qualified Yi.Rope             as R (fromText)
import           Yi.Types            (YiVariable)
import           Yi.UI.Common        (reloadProject)

---------------------------
-- | Changing the buffer name quite useful if you have
-- several the same. This also breaks the relation with the file.

changeBufferNameE :: YiM ()
changeBufferNameE = withMinibufferFree "New buffer name:" strFun
  where
    strFun :: T.Text -> YiM ()
    strFun = withCurrentBuffer . (.=) identA . MemBuffer

----------------------------
-- | shell-command with argument prompt
shellCommandE :: YiM ()
shellCommandE = withMinibufferFree "Shell command:" shellCommandV

----------------------------
-- | shell-command with a known argument
shellCommandV :: T.Text -> YiM ()
shellCommandV cmd = do
  (exitCode,cmdOut,cmdErr) <- liftBase . runShellCommand $ T.unpack cmd
  case exitCode of
    ExitSuccess -> if T.length (T.filter (== '\n') cmdOut) > 17
                   then withEditor . void $ -- see GitHub issue #477
                          newBufferE (MemBuffer "Shell Command Output")
                                     (R.fromText cmdOut)
                   else printMsg $ case cmdOut of
                     "" -> "(Shell command with no output)"
                     -- Drop trailing newline from output
                     xs -> if T.last xs == '\n' then T.init xs else xs
    ExitFailure _ -> printMsg cmdErr

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
   maybeM deleteBuffer =<< cabalBuffer <$> getEditorDyn
   putEditorDyn $ CabalBuffer $ Just b
   return ()

makeBuild :: CommandArguments -> YiM ()
makeBuild (CommandArguments args) = buildRun "make" args (const $ return ())

cabalRun :: T.Text -> (Either SomeException ExitCode -> YiM x) -> CommandArguments -> YiM ()
cabalRun cmd onExit (CommandArguments args) = buildRun "cabal" (cmd:args) onExit

makeRun :: (Either SomeException ExitCode -> YiM x) -> CommandArguments -> YiM ()
makeRun onExit (CommandArguments args) = buildRun "make" args onExit

-----------------------
-- | cabal-build
cabalBuildE :: CommandArguments -> YiM ()
cabalBuildE = cabalRun "build" (const $ return ())

makeBuildE :: CommandArguments -> YiM ()
makeBuildE = makeRun (const $ return ())

-- | Search the source files in the project.
searchSources :: String ::: RegexTag -> YiM ()
searchSources = grepFind (Doc "*.hs")

-- | Perform a find+grep operation
grepFind :: String ::: FilePatternTag -> String ::: RegexTag -> YiM ()
grepFind (Doc filePattern) (Doc searchedRegex) = withOtherWindow $ do
    void $ startSubprocess "find" [".",
                                      "-name", "_darcs", "-prune", "-o",
                                      "-name", filePattern, "-exec", "grep", "-Hnie", searchedRegex, "{}", ";"] (const $ return ())
    return ()

-----------------------
-- | stack-build

stackCommandE :: T.Text -> CommandArguments -> YiM ()
stackCommandE cmd = stackRun cmd (const $ return ())

stackRun :: T.Text -> (Either SomeException ExitCode -> YiM x) -> CommandArguments -> YiM ()
stackRun cmd onExit (CommandArguments args) = buildRun "stack" (cmd:args) onExit