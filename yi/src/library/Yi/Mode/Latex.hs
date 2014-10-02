{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Mode.Latex
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Collection of 'Mode's for working with LaTeX.

module Yi.Mode.Latex (latexMode3, latexMode2, fastMode) where

import           Data.Text ()
import           Yi.Buffer
import qualified Yi.IncrementalParse as IncrParser
import           Yi.Keymap (withBuffer)
import           Yi.Lexer.Alex (lexScanner, commonLexer, CharScanner,
                                Tok, AlexState)
import qualified Yi.Lexer.Latex as Latex
import           Yi.Modes (anyExtension, fundamentalMode)
import           Yi.Syntax
import qualified Yi.Syntax.Driver as Driver
import qualified Yi.Syntax.Latex as Latex
import           Yi.Syntax.OnlineTree (manyToks, Tree)
import           Yi.Syntax.Tree

abstract :: Mode syntax
abstract = fundamentalMode
 {
   modeApplies = anyExtension ["tex", "sty", "ltx"],
   modeToggleCommentSelection = withBuffer (toggleCommentSelectionB "% " "%")
 }

fastMode :: Mode (Tree Latex.TT)
fastMode = abstract
  {
    modeName = "fast latex",
    modeHL = ExtHL $ mkHighlighter (IncrParser.scanner manyToks . latexLexer),
    modeGetStrokes = tokenBasedStrokes Latex.tokenToStroke
  }

-- | syntax-based latex mode
latexMode2 :: Mode (Latex.Tree Latex.TT)
latexMode2 = abstract
  {
    modeName = "latex",
    modeHL = ExtHL $
       mkHighlighter (IncrParser.scanner Latex.parse . latexLexer),
    modeGetStrokes = \t point begin end -> Latex.getStrokes point begin end t
  }

-- | syntax-based latex mode
latexMode3 :: Mode (Latex.Tree Latex.TT)
latexMode3 = abstract
  {
    modeName = "latex",
    modeHL = ExtHL $
       Driver.mkHighlighter (IncrParser.scanner Latex.parse . latexLexer),
    modeGetStrokes = \t point begin end -> Latex.getStrokes point begin end t
  }

latexLexer :: CharScanner -> Scanner (AlexState Latex.HlState) (Tok Latex.Token)
latexLexer = lexScanner (commonLexer Latex.alexScanToken Latex.initState)
