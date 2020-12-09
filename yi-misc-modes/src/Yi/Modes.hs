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
                 gitCommitMode, rubyMode, markdownMode
                ) where

import           Lens.Micro.Platform          ((%~), (&), (.~))
import           Data.List           (isPrefixOf)
import           System.FilePath     (takeDirectory, takeFileName)

import           Yi.Buffer
--import           Yi.Lexer.Alex
import qualified Yi.Lexer.C           as C (lexer)
import qualified Yi.Lexer.Cabal       as Cabal (lexer)
import qualified Yi.Lexer.Clojure     as Clojure (lexer)
import qualified Yi.Lexer.Cplusplus   as Cplusplus (lexer)
import qualified Yi.Lexer.GitCommit   as GitCommit (Token, lexer)
import qualified Yi.Lexer.GNUMake     as GNUMake (lexer)
import qualified Yi.Lexer.Java        as Java (lexer)
import qualified Yi.Lexer.JSON        as JSON (lexer)
import qualified Yi.Lexer.Markdown    as Markdown (lexer)
import qualified Yi.Lexer.ObjectiveC  as ObjectiveC (lexer)
import qualified Yi.Lexer.OCaml       as OCaml (Token, lexer)
import qualified Yi.Lexer.Ott         as Ott (lexer)
import qualified Yi.Lexer.Perl        as Perl (lexer)
import qualified Yi.Lexer.Python      as Python (lexer)
import qualified Yi.Lexer.Ruby        as Ruby (lexer)
import qualified Yi.Lexer.Srmc        as Srmc (lexer)
import qualified Yi.Lexer.SVNCommit   as SVNCommit (lexer)
import qualified Yi.Lexer.Whitespace  as Whitespace (lexer)
import           Yi.Mode.Common
import           Yi.Style             (StyleName)

cMode :: TokenBasedMode StyleName
cMode = styleMode C.lexer
  & modeNameA .~ "c"
  & modeAppliesA .~ anyExtension [ "c", "h" ]

objectiveCMode :: TokenBasedMode StyleName
objectiveCMode = styleMode ObjectiveC.lexer
  & modeNameA .~ "objective-c"
  & modeAppliesA .~ anyExtension [ "m", "mm" ]

cppMode :: TokenBasedMode StyleName
cppMode = styleMode Cplusplus.lexer
  & modeAppliesA .~ anyExtension [ "cxx", "cpp", "hxx" ]
  & modeNameA .~ "c++"

cabalMode :: TokenBasedMode StyleName
cabalMode = styleMode Cabal.lexer
  & modeNameA .~ "cabal"
  & modeAppliesA .~ anyExtension [ "cabal" ]
  & modeToggleCommentSelectionA .~ Just (toggleCommentB "--")

clojureMode :: TokenBasedMode StyleName
clojureMode = styleMode Clojure.lexer
  & modeNameA .~ "clojure"
  & modeAppliesA .~ anyExtension [ "clj", "edn" ]

srmcMode :: TokenBasedMode StyleName
srmcMode = styleMode Srmc.lexer
  & modeNameA .~ "srmc"
  & modeAppliesA .~ anyExtension [ "pepa", "srmc" ] -- pepa is a subset of srmc

gitCommitMode :: TokenBasedMode GitCommit.Token
gitCommitMode = styleMode GitCommit.lexer
  & modeNameA .~ "git-commit"
  & modeAppliesA .~ isCommit
  where
    isCommit p _ = case (takeFileName p, takeFileName $ takeDirectory p) of
      ("COMMIT_EDITMSG", ".git") -> True
      _ -> False

svnCommitMode :: TokenBasedMode StyleName
svnCommitMode = styleMode SVNCommit.lexer
  & modeNameA .~ "svn-commit"
  & modeAppliesA .~ isCommit
  where
    isCommit p _ = "svn-commit" `isPrefixOf` p && extensionMatches ["tmp"] p

ocamlMode :: TokenBasedMode OCaml.Token
ocamlMode = styleMode OCaml.lexer
  & modeNameA .~ "ocaml"
  & modeAppliesA .~ anyExtension [ "ml", "mli", "mly" , "mll", "ml4", "mlp4" ]

perlMode :: TokenBasedMode StyleName
perlMode = styleMode Perl.lexer
  & modeNameA .~ "perl"
  & modeAppliesA .~ anyExtension [ "t", "pl", "pm" ]

rubyMode :: TokenBasedMode StyleName
rubyMode = styleMode Ruby.lexer
  & modeNameA .~ "ruby"
  & modeAppliesA .~ anyExtension [ "rb", "ru" ]

pythonMode :: TokenBasedMode StyleName
pythonMode = base
  & modeNameA .~ "python"
  & modeAppliesA .~ anyExtension [ "py" ]
  & modeToggleCommentSelectionA .~ Just (toggleCommentB "#")
  & modeIndentSettingsA %~ (\x -> x { expandTabs = True, tabSize = 4 })
  where
    base = styleMode Python.lexer

javaMode :: TokenBasedMode StyleName
javaMode = styleMode Java.lexer
  & modeNameA .~ "java"
  & modeAppliesA .~ anyExtension [ "java" ]

jsonMode :: TokenBasedMode StyleName
jsonMode = styleMode JSON.lexer
  & modeNameA .~ "json"
  & modeAppliesA .~ anyExtension [ "json" ]

gnuMakeMode :: TokenBasedMode StyleName
gnuMakeMode = styleMode GNUMake.lexer
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

markdownMode :: TokenBasedMode StyleName
markdownMode = styleMode Markdown.lexer
  & modeNameA .~ "markdown"
  & modeAppliesA .~ anyExtension [ "md", "markdown", "mdown", "mkdn", "mdwn", "mkd" ]

ottMode :: TokenBasedMode StyleName
ottMode = styleMode Ott.lexer
  & modeNameA .~ "ott"
  & modeAppliesA .~ anyExtension [ "ott" ]

whitespaceMode :: TokenBasedMode StyleName
whitespaceMode = styleMode Whitespace.lexer
  & modeNameA .~ "whitespace"
  & modeAppliesA .~ anyExtension [ "ws" ]
  & modeIndentA .~ (\_ _ -> insertB '\t')
