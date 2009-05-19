-- (C) Copyright 2009 Deniz Dogan

module Yi.Mode.JavaScript (javaScriptMode, hooks) where

import Prelude ()
import Yi.Core ( (.), Mode, emptyMode, modeApplies, modeName
               , modeToggleCommentSelection, toggleCommentSelectionB, modeHL
               , Char, modeGetStrokes, ($) )
import Yi.IncrementalParse (scanner)
import Yi.Lexer.Alex (AlexState, Tok, lexScanner)
import Yi.Lexer.JavaScript (alexScanToken, TT, initState, HlState, Token)
import Yi.Modes (anyExtension)
import Yi.Syntax (ExtHL(..), mkHighlighter, Scanner, Point)
import Yi.Syntax.JavaScript (Tree, parse, getStrokes)

javaScriptAbstract :: Mode syntax
javaScriptAbstract = emptyMode
  {
    modeApplies = anyExtension ["js", "json"],
    modeName = "javascript",
    modeToggleCommentSelection = toggleCommentSelectionB "// " "//"
  }

javaScriptMode :: Mode (Tree TT)
javaScriptMode = javaScriptAbstract
  {
    modeHL = ExtHL $ mkHighlighter (scanner parse . jsLexer),
    modeGetStrokes = getStrokes
  }

jsLexer :: Scanner Point Char -> Scanner (AlexState HlState) (Tok Token)
jsLexer = lexScanner alexScanToken initState

--------------------------------------------------------------------------------

-- tta :: Yi.Lexer.Alex.Tok Token -> Maybe (Yi.Syntax.Span String)
-- tta = sequenceA . tokToSpan . (fmap Main.tokenToText)

hooks :: t -> t
hooks mode = mode -- { modeGetAnnotations = tokenBasedAnnots tta,
                  --   modeName = "my " ++ modeName mode,
                  --   modeKeymap = (choice [ctrlCh 'c' ?>> ctrlCh 'l' ?>>! jsCompile]
                  --                 <||)
                  -- }

-- jsCompile = undefined
