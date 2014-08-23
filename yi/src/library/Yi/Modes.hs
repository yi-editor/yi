{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Modes
-- License     :  GPL-2
--
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for the bulk of modes shipped with Yi.

module Yi.Modes (TokenBasedMode, fundamentalMode,
                 cMode, objectiveCMode, cppMode, cabalMode,
                 srmcMode, ocamlMode, ottMode, gnuMakeMode,
                 perlMode, pythonMode, javaMode, jsonMode, anyExtension,
                 extensionOrContentsMatch, linearSyntaxMode,
                 svnCommitMode, hookModes, applyModeHooks,
                 lookupMode, whitespaceMode, removeAnnots,
                 gitCommitMode, rubyMode
                ) where

import           Control.Applicative
import           Data.List (isPrefixOf)
import           Data.Maybe
import           System.FilePath
import           Text.Regex.TDFA ((=~))
import           Yi.Buffer
import           Yi.Lexer.Alex (Tok(..), tokToSpan)
import           Yi.Style
import           Yi.Syntax
import           Yi.Syntax.Tree
import qualified Yi.Syntax.Driver as Driver
import           Yi.Keymap
import           Yi.MiniBuffer
import           Yi.Lexer.Alex (lexScanner, commonLexer, TokenLexer,
                      Lexer(..), AlexState, AlexInput)
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Lexer.C          as C
import qualified Yi.Lexer.Cabal      as Cabal
import qualified Yi.Lexer.Cplusplus  as Cplusplus
import qualified Yi.Lexer.GNUMake    as GNUMake
import qualified Yi.Lexer.GitCommit  as GitCommit
import qualified Yi.Lexer.JSON       as JSON
import qualified Yi.Lexer.Java       as Java
import qualified Yi.Lexer.OCaml      as OCaml
import qualified Yi.Lexer.ObjectiveC as ObjectiveC
import qualified Yi.Lexer.Ott        as Ott
import qualified Yi.Lexer.Perl       as Perl
import qualified Yi.Lexer.Python     as Python
import qualified Yi.Lexer.Ruby       as Ruby
import qualified Yi.Lexer.SVNCommit  as SVNCommit
import qualified Yi.Lexer.Srmc       as Srmc
import qualified Yi.Lexer.Whitespace  as Whitespace
import           Yi.Syntax.OnlineTree (manyToks, Tree)

type TokenBasedMode tok = Mode (Tree (Tok tok))
type StyleBasedMode = TokenBasedMode StyleName

fundamentalMode :: Mode syntax
fundamentalMode = emptyMode
  {
   modeName = "fundamental",
   modeApplies = modeAlwaysApplies,
   modeIndent = const autoIndentB,
   modePrettify = const fillParagraph
  }

-- | Creates a 'TokenBasedMode' from a 'Lexer' and a function that
-- turns tokens into 'StyleName'.
linearSyntaxMode' :: Show (l s)
                  => Lexer l s (Tok t) i
                  -> (t -> StyleName)
                  -> TokenBasedMode t
linearSyntaxMode' scanToken tokenToStyle = fundamentalMode
  { modeHL = ExtHL $ Driver.mkHighlighter (IncrParser.scanner manyToks . lexer)
  , modeGetStrokes = tokenBasedStrokes tokenToStroke
  }
  where
    tokenToStroke = fmap tokenToStyle . tokToSpan
    lexer = lexScanner scanToken

-- | Specialised version of 'linearSyntaxMode'' for the common case,
-- wrapping up into a 'Lexer' with 'commonLexer'.
linearSyntaxMode :: Show s => s -- ^ Starting state
                 -> TokenLexer AlexState s (Tok t) AlexInput
                 -> (t -> StyleName)
                 -> TokenBasedMode t
linearSyntaxMode initSt scanToken tokenToStyle =
  linearSyntaxMode' (commonLexer scanToken initSt) tokenToStyle

removeAnnots :: Mode a -> Mode a
removeAnnots m = m { modeName = modeName m ++ " no annots", modeGetAnnotations = modeGetAnnotations emptyMode }

cMode :: StyleBasedMode
cMode = (linearSyntaxMode C.initState C.alexScanToken id)
  {
    modeApplies = anyExtension ["c", "h"],
    modeName = "c"
  }

objectiveCMode :: StyleBasedMode
objectiveCMode = (linearSyntaxMode ObjectiveC.initState ObjectiveC.alexScanToken id)
  {
    modeApplies = anyExtension ["m", "mm"],
    modeName = "objective-c"
  }

cppMode :: StyleBasedMode
cppMode = (linearSyntaxMode Cplusplus.initState Cplusplus.alexScanToken id)
  {
    modeApplies = anyExtension ["cxx", "cpp", "hxx"],
    modeName = "c++"
  }

cabalMode :: StyleBasedMode
cabalMode = (linearSyntaxMode Cabal.initState Cabal.alexScanToken id)
  {
    modeName = "cabal",
    modeApplies = anyExtension ["cabal"],
    modeToggleCommentSelection = toggleCommentSelectionB "-- " "--"
  }


srmcMode :: StyleBasedMode
srmcMode = (linearSyntaxMode Srmc.initState Srmc.alexScanToken id)
  {
    modeName = "srmc",
    modeApplies = anyExtension ["pepa", -- pepa is a subset of srmc
                                "srmc"]
  }

gitCommitMode :: Mode (Tree (Tok GitCommit.Token))
gitCommitMode = (linearSyntaxMode GitCommit.initState GitCommit.alexScanToken id)
  {
    modeName = "git-commit",
    modeApplies = \path _ -> takeFileName path == "COMMIT_EDITMSG" &&
                             takeFileName (takeDirectory path) == ".git"
  }

svnCommitMode :: StyleBasedMode
svnCommitMode = (linearSyntaxMode SVNCommit.initState SVNCommit.alexScanToken id)
  {
    modeName = "svn-commit",
    modeApplies = \path _contents -> isPrefixOf "svn-commit" path && extensionMatches ["tmp"] path
  }

ocamlMode :: TokenBasedMode OCaml.Token
ocamlMode = (linearSyntaxMode OCaml.initState OCaml.alexScanToken OCaml.tokenToStyle)
  {
    modeName = "ocaml",
    modeApplies = anyExtension ["ml", "mli", "mly", "mll", "ml4", "mlp4"]
  }

perlMode :: StyleBasedMode
perlMode = (linearSyntaxMode Perl.initState Perl.alexScanToken id)
  {
    modeName = "perl",
    modeApplies = anyExtension ["t", "pl", "pm"]
  }

rubyMode :: StyleBasedMode
rubyMode = (linearSyntaxMode Ruby.initState Ruby.alexScanToken id)
  {
    modeName = "ruby",
    modeApplies = anyExtension ["rb", "ru"]
  }

pythonMode :: StyleBasedMode
pythonMode = base
  {
    modeName = "python",
    modeApplies = anyExtension ["py"],
    modeToggleCommentSelection = toggleCommentSelectionB "# " "#",
    modeIndentSettings = (modeIndentSettings base)
      {
        expandTabs = True,
        tabSize = 4
      }
  }
    where base = linearSyntaxMode Python.initState Python.alexScanToken id

javaMode :: StyleBasedMode
javaMode = (linearSyntaxMode Java.initState Java.alexScanToken id)
  {
    modeName = "java",
    modeApplies = anyExtension ["java"]
  }

jsonMode :: StyleBasedMode
jsonMode = (linearSyntaxMode JSON.initState JSON.alexScanToken id)
  {
    modeName = "json",
    modeApplies = anyExtension ["json"]
  }

isMakefile :: FilePath -> String -> Bool
isMakefile path _contents = matches $ takeFileName path
    where matches "Makefile"    = True
          matches "makefile"    = True
          matches "GNUmakefile" = True
          matches filename      = extensionMatches ["mk"] filename
          -- TODO: .mk is fairly standard but are there others?

gnuMakeMode :: StyleBasedMode
gnuMakeMode = base
  {
    modeName = "Makefile",
    modeApplies = isMakefile,
    modeIndentSettings = (modeIndentSettings base)
      {
        expandTabs = False,
        shiftWidth = 8
      }
  }
    where base = linearSyntaxMode GNUMake.initState GNUMake.alexScanToken id

ottMode :: StyleBasedMode
ottMode = (linearSyntaxMode Ott.initState Ott.alexScanToken id)
  {
    modeName = "ott",
    modeApplies = anyExtension ["ott"]
  }

whitespaceMode :: TokenBasedMode StyleName
whitespaceMode = base
  {
    modeName = "whitespace",
    modeApplies = anyExtension ["ws"],
    modeIndent = \_ _ -> insertB '\t'
  }
    where
      base = linearSyntaxMode Whitespace.initState Whitespace.alexScanToken id


-- | Determines if the file's extension is one of the extensions in the list.
extensionMatches :: [String] -> FilePath -> Bool
extensionMatches extensions fileName = extension `elem` extensions'
    where extension = takeExtension fileName
          extensions' = ['.' : ext | ext <- extensions]

-- | When applied to an extensions list, creates a 'Mode.modeApplies' function.
anyExtension :: [String] -> FilePath -> String -> Bool
anyExtension extensions fileName _contents
    = extensionMatches extensions fileName

-- | When applied to an extensions list and regular expression pattern, creates
-- a 'Mode.modeApplies' function.
extensionOrContentsMatch :: [String] -> String -> FilePath -> String -> Bool
extensionOrContentsMatch extensions pattern fileName contents
    = extensionMatches extensions fileName || contentsMatch
    where contentsMatch = contents =~ pattern :: Bool

-- | Adds a hook to all matching hooks in a list
hookModes :: (AnyMode -> Bool) -> BufferM () -> [AnyMode] -> [AnyMode]
hookModes p h = map $ \am@(AnyMode m) -> if p am
                                            then AnyMode $ m { modeOnLoad = modeOnLoad m >> h }
                                            else am

-- | Apply a list of mode hooks to a list of AnyModes
applyModeHooks :: [(AnyMode -> Bool, BufferM ())] -> [AnyMode] -> [AnyMode]
applyModeHooks hs ms = flip map ms $ \am -> case filter (($am) . fst) hs of
    [] -> am
    ls -> onMode (\m -> m { modeOnLoad = foldr ((>>) . snd) (modeOnLoad m) ls }) am

-- | Check whether a mode of the same name is already in modeTable and returns the
-- original mode, if it isn't the case.
lookupMode :: AnyMode -> YiM AnyMode
lookupMode am@(AnyMode m) = fromMaybe am <$> anyModeByNameM (modeName m)
