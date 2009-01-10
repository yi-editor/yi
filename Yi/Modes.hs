{-# LANGUAGE Rank2Types #-}
module Yi.Modes (fundamentalMode,
                 cMode, objectiveCMode, cppMode, cabalMode,
                 srmcMode, ocamlMode, ottMode, gnuMakeMode,
                 perlMode, pythonMode,  anyExtension,
                 extensionOrContentsMatch, linearSyntaxMode
                ) where

import Prelude ()
import System.FilePath
import Text.Regex.TDFA ((=~))

import Yi.Buffer
import Yi.Lexer.Alex (Tok(..), Posn(..), tokToSpan)
import Yi.Prelude
import Yi.Style
import Yi.Syntax
import qualified Yi.Lexer.Alex       as Alex
import qualified Yi.Lexer.Cabal      as Cabal
import qualified Yi.Lexer.C          as C
import qualified Yi.Lexer.ObjectiveC as ObjectiveC
import qualified Yi.Lexer.Cplusplus  as Cplusplus
import qualified Yi.Lexer.GNUMake    as GNUMake
import qualified Yi.Lexer.OCaml      as OCaml
import qualified Yi.Lexer.Ott        as Ott
import qualified Yi.Lexer.Perl       as Perl
import qualified Yi.Lexer.Python     as Python
import qualified Yi.Lexer.Srmc       as Srmc
import Yi.Syntax.OnlineTree as OnlineTree
import qualified Yi.IncrementalParse as IncrParser

fundamentalMode :: Mode syntax
-- cMode, objectiveCMode, cppMode, cabalMode, srmcMode, ocamlMode, ottMode, gnuMakeMode, perlMode, pythonMode :: Mode (OnlineTree.Tree Stroke)

fundamentalMode = emptyMode
  { 
   modeName = "fundamental",
   modeApplies = modeAlwaysApplies,
   modeIndent = const autoIndentB,
   modePrettify = const fillParagraph
  }

linearSyntaxMode :: forall lexerState t.
                                                (Show lexerState) =>
                                                lexerState
                                                -> ((IncrParser.AlexState lexerState,
                                                     Alex.AlexInput)
                                                    -> Maybe
                                                         (Tok t,
                                                          (IncrParser.AlexState lexerState,
                                                           Alex.AlexInput)))
                                                -> (t -> StyleName)
                                                -> Mode (Tree (Tok t))
linearSyntaxMode initSt scanToken tokenToStyle 
    = fundamentalMode { 
                        modeHL = ExtHL $ mkHighlighter (IncrParser.scanner OnlineTree.manyToks . lexer),
                        modeGetStrokes = \t _point begin _end -> fmap tokenToStroke $ dropToIndexBad begin t
                      }
    where tokenToStroke = fmap tokenToStyle . tokToSpan
          lexer = Alex.lexScanner scanToken initSt

cMode = (linearSyntaxMode C.initState C.alexScanToken id)
  {
    modeApplies = anyExtension ["c", "h"], 
    modeName = "c"
  }

objectiveCMode = (linearSyntaxMode ObjectiveC.initState ObjectiveC.alexScanToken id)
  {
    modeApplies = anyExtension ["m"], 
    modeName = "objective-c"
  }

cppMode = (linearSyntaxMode Cplusplus.initState Cplusplus.alexScanToken id)
  {
    modeApplies = anyExtension ["cxx", "cpp", "hxx"], 
    modeName = "c++"
  }

cabalMode = (linearSyntaxMode Cabal.initState Cabal.alexScanToken id)
  {
    modeName = "cabal",
    modeApplies = anyExtension ["cabal"]
  }


srmcMode = (linearSyntaxMode Srmc.initState Srmc.alexScanToken id)
  {
    modeName = "srmc",
    modeApplies = anyExtension ["pepa", -- pepa is a subset of srmc    
                                "srmc"]
  }

ocamlMode = (linearSyntaxMode OCaml.initState OCaml.alexScanToken OCaml.tokenToStyle)
  {
    modeName = "ocaml",
    modeApplies = anyExtension ["ml", "mli", "mly", "mll", "ml4", "mlp4"]
  }

perlMode = (linearSyntaxMode Perl.initState Perl.alexScanToken id)
  {
    modeName = "perl",
    modeApplies = anyExtension ["t", "pl", "pm"]
  }

pythonMode = (linearSyntaxMode Python.initState Python.alexScanToken id)
  {
    modeName = "python",
    modeApplies = anyExtension ["py"]
  }

isMakefile :: FilePath -> String -> Bool
isMakefile path _contents = matches $ takeFileName path
    where matches "Makefile"    = True
          matches "makefile"    = True
          matches "GNUmakefile" = True
          matches filename      = extensionMatches ["mk"] filename
          -- TODO: .mk is fairly standard but are there others?

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

ottMode = (linearSyntaxMode Ott.initState Ott.alexScanToken id)
  {
    modeName = "ott",
    modeApplies = anyExtension ["ott"]
  }

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
