module Yi.Mode.Latex (latexMode, latexMode2) where

import Prelude ()
import Yi.Buffer
import Yi.Prelude
import Yi.Syntax
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Syntax.Linear as Linear
import qualified Yi.Syntax.Latex as Latex
import qualified Yi.Lexer.Latex               as Latex
import Yi.Modes (anyExtension, mkHighlighter', fundamentalMode)


abstract :: forall syntax. Mode syntax
abstract = fundamentalMode
 {
   modeApplies = anyExtension ["tex", "sty", "ltx"],
   modeToggleCommentSelection = toggleCommentSelectionB "% " "%"
 }

-- | token-based latex mode 
latexMode :: Mode (Linear.Result Stroke)
latexMode = abstract
  {
    modeName = "plain latex",
    modeHL = ExtHL $ mkHighlighter' Latex.initState Latex.alexScanToken (Latex.tokenToStyle)    
  }

-- | syntax-based latex mode
latexMode2 :: Mode [Latex.Tree Latex.TT]
latexMode2 = abstract
  {
    modeApplies = modeApplies latexMode,
    modeName = "latex",
    modeHL = ExtHL $ 
       mkHighlighter (IncrParser.scanner Latex.parse . latexLexer)
      (\point begin end t -> Latex.getStrokes point begin end t)
  }
    where latexLexer = Alex.lexScanner Latex.alexScanToken Latex.initState

