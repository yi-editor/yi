{-# LANGUAGE Rank2Types #-}
module Yi.Mode.Latex (latexMode, latexMode2, fastMode) where

import Prelude ()
import Yi.Buffer
import Yi.Prelude
import Yi.Syntax
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Syntax.Linear as Linear
import qualified Yi.Syntax.Latex as Latex
import Yi.Syntax.OnlineTree as OnlineTree
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

fastMode :: Mode (OnlineTree.Tree Latex.TT)
fastMode = abstract
  {
    modeName = "fast latex",
    modeHL = ExtHL $
    mkHighlighter (IncrParser.scanner OnlineTree.parse . latexLexer)
      (\_point begin _end t -> fmap Latex.tokenToStroke $ dropToIndex begin t)

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

latexLexer = Alex.lexScanner Latex.alexScanToken Latex.initState

