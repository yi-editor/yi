{-# LANGUAGE Rank2Types #-}
module Yi.Modes (fundamentalMode,
                 cppMode, cabalMode, srmcMode, ocamlMode,
                 ottMode, gnuMakeMode,
                 perlMode, pythonMode, 
                 anyExtension, mkHighlighter', extensionOrContentsMatch
                ) where

import Control.Arrow (first)
import Prelude ()
import System.FilePath
import Text.Regex.TDFA ((=~))

import Yi.Buffer
import Yi.Lexer.Alex (Tok(..), Posn(..))
import Yi.Prelude
import Yi.Style
import Yi.Syntax
import qualified Yi.Lexer.Alex      as Alex
import qualified Yi.Lexer.Cabal     as Cabal
import qualified Yi.Lexer.Cplusplus as Cplusplus
import qualified Yi.Lexer.GNUMake   as GNUMake
import qualified Yi.Lexer.OCaml     as OCaml
import qualified Yi.Lexer.Ott       as Ott
import qualified Yi.Lexer.Perl      as Perl
import qualified Yi.Lexer.Python    as Python
import qualified Yi.Lexer.Srmc      as Srmc
import qualified Yi.Syntax.Linear   as Linear

fundamentalMode :: Mode syntax
cppMode, cabalMode, srmcMode, ocamlMode, ottMode, gnuMakeMode, perlMode, pythonMode :: Mode (Linear.Result Stroke)

fundamentalMode = emptyMode
  { 
   modeName = "fundamental",
   modeApplies = modeAlwaysApplies,
   modeIndent = const autoIndentB,
   modePrettify = const fillParagraph
  }

linearSyntaxMode = fundamentalMode { modeGetStrokes = Linear.getStrokes }

mkHighlighter' :: forall token lexerState. Show lexerState =>
                    lexerState
                    -> (Alex.ASI lexerState
                        -> Maybe (Tok token, Alex.ASI lexerState))
                    -> (token -> StyleName)
                    -> Highlighter
                         (Yi.Syntax.Cache
                            (Alex.AlexState lexerState,
                             [Stroke])
                            (Linear.Result Stroke))
                         (Linear.Result Stroke)
mkHighlighter' initSt scan tokenToStyle = mkHighlighter (Linear.incrScanner . Alex.lexScanner (fmap (first tokenToStroke) . scan) initSt) 
    where tokenToStroke (Tok t len posn) = (posnOfs posn, tokenToStyle t, posnOfs posn +~ len)

cppMode = linearSyntaxMode
  {
    modeApplies = anyExtension ["cxx", "cpp", "C", "hxx", "H", "h", "c"], 
    -- Treat c file as cpp files, most users will allow for that.
    modeName = "c++",
    modeHL = ExtHL $ mkHighlighter' Cplusplus.initState Cplusplus.alexScanToken id
  }

cabalMode = linearSyntaxMode
  {
    modeName = "cabal",
    modeApplies = anyExtension ["cabal"],
    modeHL = ExtHL $ mkHighlighter' Cabal.initState Cabal.alexScanToken id
  }


srmcMode = linearSyntaxMode
  {
    modeName = "srmc",
    modeApplies = anyExtension ["pepa", -- pepa is a subset of srmc    
                                "srmc"],
    modeHL = ExtHL $ mkHighlighter' Srmc.initState Srmc.alexScanToken id
  }

ocamlMode = linearSyntaxMode
  {
    modeName = "ocaml",
    modeApplies = anyExtension ["ml", "mli", "mly", "mll", "ml4", "mlp4"],
    modeHL = ExtHL $ mkHighlighter' OCaml.initState OCaml.alexScanToken OCaml.tokenToStyle
  }

perlMode = linearSyntaxMode
  {
    modeName = "perl",
    modeApplies = anyExtension ["t", "pl", "pm"],
    modeHL = ExtHL $ mkHighlighter' Perl.initState Perl.alexScanToken id
  }

pythonMode = linearSyntaxMode
  {
    modeName = "python",
    modeApplies = anyExtension ["py"], 
    modeHL = ExtHL $ mkHighlighter' Python.initState Python.alexScanToken id
  }

isMakefile :: FilePath -> String -> Bool
isMakefile path _contents = matches $ takeFileName path
    where matches "Makefile"    = True
          matches "makefile"    = True
          matches "GNUmakefile" = True
          matches filename      = extensionMatches ["mk"] filename
          -- TODO: .mk is fairly standard but are there others?

gnuMakeMode = linearSyntaxMode
  {
    modeName = "Makefile",
    modeApplies = isMakefile,
    modeHL = ExtHL $ mkHighlighter' GNUMake.initState GNUMake.alexScanToken id,
    modeIndentSettings = (modeIndentSettings linearSyntaxMode)
      {
        expandTabs = False,
        shiftWidth = 8
      }
  }

ottMode = linearSyntaxMode
  {
    modeName = "ott",
    modeApplies = anyExtension ["ott"],
    modeHL = ExtHL $ mkHighlighter' Ott.initState Ott.alexScanToken id
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
