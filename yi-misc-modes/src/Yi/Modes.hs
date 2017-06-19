{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Modes
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for the bulk of modes shipped with Yi.

module Yi.Modes (cMode, objectiveCMode, cppMode, cabalMode,  clojureMode,
                 srmcMode, ocamlMode, ottMode, gnuMakeMode,
                 perlMode, pythonMode, javaMode, jsonMode, anyExtension,
                 svnCommitMode, whitespaceMode,
                 gitCommitMode, rubyMode
                ) where

import           Lens.Micro.Platform          ((%~), (&), (.~))
import           Data.List           (isPrefixOf)
import           System.FilePath     (takeDirectory, takeFileName)

import           Yi.Buffer
import           Yi.Mode.Common
import           Yi.Style             (StyleName)

cMode :: Mode
cMode = fundamentalMode
  & modeNameA .~ "c"
  & modeAppliesA .~ anyExtension [ "c", "h" ]

objectiveCMode :: Mode
objectiveCMode = fundamentalMode
  & modeNameA .~ "objective-c"
  & modeAppliesA .~ anyExtension [ "m", "mm" ]

cppMode :: Mode
cppMode = fundamentalMode
  & modeAppliesA .~ anyExtension [ "cxx", "cpp", "hxx" ]
  & modeNameA .~ "c++"

cabalMode :: Mode
cabalMode = fundamentalMode
  & modeNameA .~ "cabal"
  & modeAppliesA .~ anyExtension [ "cabal" ]
  & modeToggleCommentSelectionA .~ Just (toggleCommentB "--")

clojureMode :: Mode
clojureMode = fundamentalMode
  & modeNameA .~ "clojure"
  & modeAppliesA .~ anyExtension [ "clj", "edn" ]

srmcMode :: Mode
srmcMode = fundamentalMode
  & modeNameA .~ "srmc"
  & modeAppliesA .~ anyExtension [ "pepa", "srmc" ] -- pepa is a subset of srmc

gitCommitMode :: Mode
gitCommitMode = fundamentalMode
  & modeNameA .~ "git-commit"
  & modeAppliesA .~ isCommit
  where
    isCommit p _ = case (takeFileName p, takeFileName $ takeDirectory p) of
      ("COMMIT_EDITMSG", ".git") -> True
      _ -> False

svnCommitMode :: Mode
svnCommitMode = fundamentalMode
  & modeNameA .~ "svn-commit"
  & modeAppliesA .~ isCommit
  where
    isCommit p _ = "svn-commit" `isPrefixOf` p && extensionMatches ["tmp"] p

ocamlMode :: Mode
ocamlMode = fundamentalMode
  & modeNameA .~ "ocaml"
  & modeAppliesA .~ anyExtension [ "ml", "mli", "mly" , "mll", "ml4", "mlp4" ]

perlMode :: Mode
perlMode = fundamentalMode
  & modeNameA .~ "perl"
  & modeAppliesA .~ anyExtension [ "t", "pl", "pm" ]

rubyMode :: Mode
rubyMode = fundamentalMode
  & modeNameA .~ "ruby"
  & modeAppliesA .~ anyExtension [ "rb", "ru" ]

pythonMode :: Mode
pythonMode = fundamentalMode
  & modeNameA .~ "python"
  & modeAppliesA .~ anyExtension [ "py" ]
  & modeToggleCommentSelectionA .~ Just (toggleCommentB "#")
  & modeIndentSettingsA %~ (\x -> x { expandTabs = True, tabSize = 4 })

javaMode :: Mode
javaMode = fundamentalMode
  & modeNameA .~ "java"
  & modeAppliesA .~ anyExtension [ "java" ]

jsonMode :: Mode
jsonMode = fundamentalMode
  & modeNameA .~ "json"
  & modeAppliesA .~ anyExtension [ "json" ]

gnuMakeMode :: Mode
gnuMakeMode = fundamentalMode
  & modeNameA .~ "Makefile"
  & modeAppliesA .~ isMakefile
  & modeIndentSettingsA %~ (\x -> x { expandTabs = False, shiftWidth = 8 })
  where
    isMakefile :: FilePath -> a -> Bool
    isMakefile path _contents = matches $ takeFileName path
        where matches "Makefile"    = True
              matches "makefile"    = True
              matches "GNUmakefile" = True
              matches filename      = extensionMatches [ "mk" ] filename

ottMode :: Mode
ottMode = fundamentalMode
  & modeNameA .~ "ott"
  & modeAppliesA .~ anyExtension [ "ott" ]

whitespaceMode :: Mode
whitespaceMode = fundamentalMode
  & modeNameA .~ "whitespace"
  & modeAppliesA .~ anyExtension [ "ws" ]
  & modeIndentA .~ (\_ -> insertB '\t')
