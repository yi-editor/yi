{-# LANGUAGE Rank2Types #-}
module Yi.Mode.Latex (latexMode2, fastMode) where

import Prelude ()
import Yi.Buffer
import Yi.Prelude
import Yi.Syntax
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Syntax.Latex as Latex
import Yi.Syntax.OnlineTree as OnlineTree
import qualified Yi.Lexer.Latex               as Latex
import Yi.Modes (anyExtension, linearSyntaxMode, fundamentalMode)


abstract :: forall syntax. Mode syntax
abstract = fundamentalMode
 {
   modeApplies = anyExtension ["tex", "sty", "ltx"],
   modeToggleCommentSelection = toggleCommentSelectionB "% " "%"
 }

fastMode :: Mode (OnlineTree.Tree Latex.TT)
fastMode = abstract
  {
    modeName = "fast latex",
    modeHL = ExtHL $
    mkHighlighter (IncrParser.scanner OnlineTree.parse . latexLexer),
    modeGetStrokes = \t _point begin _end -> fmap Latex.tokenToStroke $ dropToIndex begin t
 }

-- | syntax-based latex mode
latexMode2 :: Mode [Latex.Tree Latex.TT]
latexMode2 = abstract
  {
    modeName = "latex",
    modeHL = ExtHL $ 
       mkHighlighter (IncrParser.scanner Latex.parse . latexLexer),
    modeGetStrokes = \t point begin end -> Latex.getStrokes point begin end t
  }

latexLexer = Alex.lexScanner Latex.alexScanToken Latex.initState

