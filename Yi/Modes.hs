module Yi.Modes (fundamentalMode,
                 cppMode, cabalMode, srmcMode, ocamlMode,
                 ottMode, gnuMakeMode,
                 perlMode, pythonMode, 
                 anyExtension, mkHighlighter'
                ) where

import Control.Arrow (first)
import Prelude ()
import System.FilePath
import Yi.Buffer
import Yi.Lexer.Alex (Tok(..), Posn(..))
import Yi.Prelude
import Yi.Style
import Yi.Syntax
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Lexer.Cabal               as Cabal
import qualified Yi.Lexer.Cplusplus           as Cplusplus
import qualified Yi.Lexer.GNUMake             as GNUMake
import qualified Yi.Lexer.OCaml               as OCaml
import qualified Yi.Lexer.Ott                 as Ott
import qualified Yi.Lexer.Perl                as Perl
import qualified Yi.Lexer.Python              as Python
import qualified Yi.Lexer.Srmc                as Srmc
import qualified Yi.Syntax.Linear as Linear

fundamentalMode :: Mode syntax
cppMode, cabalMode, srmcMode, ocamlMode, ottMode, gnuMakeMode, perlMode, pythonMode :: Mode (Linear.Result Stroke)

fundamentalMode = emptyMode
  { 
   modeName = "fundamental",
   modeApplies = const True,
   modeIndent = \_ast -> autoIndentB,
   modePrettify = \_ -> fillParagraph
  }

mkHighlighter' :: forall token lexerState.
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
mkHighlighter' initSt scan tokenToStyle = mkHighlighter (Linear.incrScanner . Alex.lexScanner (fmap (first tokenToStroke) . scan) initSt) Linear.getStrokes
    where tokenToStroke (Tok t len posn) = (posnOfs posn, tokenToStyle t, posnOfs posn +~ len)

cppMode = fundamentalMode
  {
    modeApplies = anyExtension ["cxx", "cpp", "C", "hxx", "H", "h", "c"], 
    -- Treat c file as cpp files, most users will allow for that.
    modeName = "c++",
    modeHL = ExtHL $ mkHighlighter' Cplusplus.initState Cplusplus.alexScanToken id
  }

cabalMode = fundamentalMode
  {
    modeName = "cabal",
    modeApplies = anyExtension ["cabal"],
    modeHL = ExtHL $ mkHighlighter' Cabal.initState Cabal.alexScanToken id
  }


srmcMode = fundamentalMode
  {
    modeName = "srmc",
    modeApplies = anyExtension ["pepa", -- pepa is a subset of srmc    
                                "srmc"],
    modeHL = ExtHL $ mkHighlighter' Srmc.initState Srmc.alexScanToken id
  }

ocamlMode = fundamentalMode
  {
    modeName = "ocaml",
    modeApplies = anyExtension ["ml", "mli", "mly", "mll", "ml4", "mlp4"],
    modeHL = ExtHL $ mkHighlighter' OCaml.initState OCaml.alexScanToken OCaml.tokenToStyle
  }

perlMode = fundamentalMode
  {
    modeName = "perl",
    modeApplies = anyExtension ["pl", "pm"],
    modeHL = ExtHL $ mkHighlighter' Perl.initState Perl.alexScanToken id
  }

pythonMode = fundamentalMode
  {
    modeName = "python",
    modeApplies = anyExtension ["t", "py"], 
    modeHL = ExtHL $ mkHighlighter' Python.initState Python.alexScanToken id
  }

-- TODO: .mk is fairly standard but are there others?
isMakefile :: String -> Bool
isMakefile "Makefile"    = True
isMakefile "makefile"    = True
isMakefile "GNUmakefile" = True
isMakefile filename      = anyExtension ["mk"] filename

gnuMakeMode = fundamentalMode
  {
    modeName = "Makefile",
    modeApplies = isMakefile,
    modeHL = ExtHL $ mkHighlighter' GNUMake.initState GNUMake.alexScanToken id,
    modeIndentSettings = (modeIndentSettings fundamentalMode)
      {
        expandTabs = False,
        shiftWidth = 8
      }
  }

ottMode = fundamentalMode
  {
    modeName = "ott",
    modeApplies = anyExtension ["ott"],
    modeHL = ExtHL $ mkHighlighter' Ott.initState Ott.alexScanToken id
  }

anyExtension :: [String] -> FilePath -> Bool
anyExtension list fileName = or [takeExtension fileName == ('.' : ext) | ext <- list] 

