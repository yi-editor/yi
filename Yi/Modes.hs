module Yi.Modes (fundamental,
                 latexMode, cppMode, literateHaskellMode, cabalMode, srmcMode, ocamlMode,
                 perlMode, pythonMode, latexMode2,
                 anyExtension
                ) where

import Control.Arrow (first)
import Prelude ()
import System.FilePath
import Yi.Buffer (Mode(..), emptyMode)
import Yi.Buffer.HighLevel (fillParagraph)
import Yi.Buffer.Indent
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
import qualified Yi.Lexer.OCaml               as OCaml
import qualified Yi.Lexer.Perl                as Perl
import qualified Yi.Lexer.Python              as Python
import qualified Yi.Lexer.Srmc                as Srmc
import qualified Yi.Syntax.Linear as Linear
import qualified Yi.Syntax.Latex as Latex


fundamental :: Mode syntax
latexMode, cppMode, literateHaskellMode, cabalMode, srmcMode, ocamlMode, perlMode, pythonMode :: Mode (Linear.Result Stroke)

fundamental = emptyMode
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

cppMode = fundamental
  {
    modeApplies = anyExtension ["cxx", "cpp", "C", "hxx", "H", "h", "c"], 
    -- Treat c file as cpp files, most users will allow for that.
    modeName = "c++",
    modeHL = ExtHL $ mkHighlighter' Cplusplus.initState Cplusplus.alexScanToken id
  }

literateHaskellMode = fundamental
  {
    modeName = "literate haskell",
    modeApplies = anyExtension ["lhs"],
    modeHL = ExtHL $ mkHighlighter' LiterateHaskell.initState LiterateHaskell.alexScanToken id
  }

cabalMode = fundamental
  {
    modeName = "cabal",
    modeApplies = anyExtension ["cabal"],
    modeHL = ExtHL $ mkHighlighter' Cabal.initState Cabal.alexScanToken id
  }

latexLexer = Alex.lexScanner Latex.alexScanToken Latex.initState

latexMode = fundamental
  {
    modeName = "plain latex",
    modeApplies = anyExtension ["tex", "sty", "ltx"],
    modeHL = ExtHL $ mkHighlighter' Latex.initState Latex.alexScanToken (Latex.tokenToStyle)
  }

latexMode2 :: Mode [Latex.Tree Latex.TT]
latexMode2 = fundamental
  {
    modeApplies = modeApplies latexMode,
    modeName = "latex",
    modeHL = ExtHL $ 
       mkHighlighter (IncrParser.scanner Latex.parse . latexLexer)
      (\point begin end t -> Latex.getStrokes point begin end t)
  }

srmcMode = fundamental
  {
    modeName = "srmc",
    modeApplies = anyExtension ["pepa", -- pepa is a subset of srmc    
                                "srmc"],
    modeHL = ExtHL $ mkHighlighter' Srmc.initState Srmc.alexScanToken id
  }

ocamlMode = fundamental
  {
    modeName = "ocaml",
    modeApplies = anyExtension ["ml", "mli", "mly", "mll", "ml4", "mlp4"],
    modeHL = ExtHL $ mkHighlighter' OCaml.initState OCaml.alexScanToken OCaml.tokenToStyle
  }

perlMode = fundamental
  {
    modeName = "perl",
    modeApplies = anyExtension ["pl", "pm"],
    modeHL = ExtHL $ mkHighlighter' Perl.initState Perl.alexScanToken id
  }

pythonMode = fundamental
  {
    modeName = "python",
    modeApplies = anyExtension ["t", "py"], 
    modeHL = ExtHL $ mkHighlighter' Python.initState Python.alexScanToken id
  }

anyExtension list fileName = or [takeExtension fileName == ('.' : ext) | ext <- list] 
