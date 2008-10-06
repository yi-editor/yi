module Yi.Modes (fundamentalMode,
                 latexMode, cppMode, literateHaskellMode, cabalMode, srmcMode, ocamlMode,
                 gnuMakeMode,
                 perlMode, pythonMode, latexMode2,
                 anyExtension
                ) where

import Control.Arrow (first)
import Prelude ()
import System.FilePath
import Yi.Buffer
import Yi.Lexer.Alex (Tok(..), Posn(..))
import Yi.Prelude
import Yi.Style
import Yi.Syntax
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Lexer.Cabal               as Cabal
import qualified Yi.Lexer.Cplusplus           as Cplusplus
import qualified Yi.Lexer.Latex               as Latex
import qualified Yi.Lexer.LiterateHaskell     as LiterateHaskell
import qualified Yi.Lexer.GNUMake             as GNUMake
import qualified Yi.Lexer.OCaml               as OCaml
import qualified Yi.Lexer.Perl                as Perl
import qualified Yi.Lexer.Python              as Python
import qualified Yi.Lexer.Srmc                as Srmc
import qualified Yi.Syntax.Linear as Linear
import qualified Yi.Syntax.Latex as Latex


fundamentalMode :: Mode syntax
latexMode, cppMode, literateHaskellMode, cabalMode, srmcMode, ocamlMode, gnuMakeMode, perlMode, pythonMode :: Mode (Linear.Result Stroke)

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

literateHaskellMode = fundamentalMode
  {
    modeName = "literate haskell",
    modeApplies = anyExtension ["lhs"],
    modeHL = ExtHL $ mkHighlighter' LiterateHaskell.initState LiterateHaskell.alexScanToken id
  }

cabalMode = fundamentalMode
  {
    modeName = "cabal",
    modeApplies = anyExtension ["cabal"],
    modeHL = ExtHL $ mkHighlighter' Cabal.initState Cabal.alexScanToken id
  }

latexLexer = Alex.lexScanner Latex.alexScanToken Latex.initState

latexMode = fundamentalMode
  {
    modeName = "plain latex",
    modeApplies = anyExtension ["tex", "sty", "ltx"],
    modeHL = ExtHL $ mkHighlighter' Latex.initState Latex.alexScanToken (Latex.tokenToStyle)
  }

latexMode2 :: Mode [Latex.Tree Latex.TT]
latexMode2 = fundamentalMode
  {
    modeApplies = modeApplies latexMode,
    modeName = "latex",
    modeHL = ExtHL $ 
       mkHighlighter (IncrParser.scanner Latex.parse . latexLexer)
      (\point begin end t -> Latex.getStrokes point begin end t)
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

gnuMakeMode = fundamentalMode
    {
        modeName = "Makefile",
        modeApplies = \filename -> filename == "Makefile",
        modeHL = ExtHL $ mkHighlighter' GNUMake.initState GNUMake.alexScanToken id
    }

anyExtension :: [String] -> FilePath -> Bool
anyExtension list fileName = or [takeExtension fileName == ('.' : ext) | ext <- list] 

