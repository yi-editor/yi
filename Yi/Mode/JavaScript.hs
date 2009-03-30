-- (C) Copyright 2009 Deniz Dogan

module Yi.Mode.JavaScript (javaScriptMode) where

import Yi.Core
import Prelude ()
import Yi.Syntax
import Yi.Syntax.JavaScript as JSSyn
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Lexer.Alex as Alex
import Yi.Lexer.JavaScript as JSLex
import Yi.Modes (anyExtension)

javaScriptAbstract :: Mode syntax
javaScriptAbstract = emptyMode
  {
    modeApplies = anyExtension ["js", "json"],
    modeName = "javascript",
    modeToggleCommentSelection = toggleCommentSelectionB "// " "//"
  }

javaScriptMode :: Mode [JSSyn.JTree JSLex.TT]
javaScriptMode = javaScriptAbstract
  {
    modeHL = ExtHL $ mkHighlighter (IncrParser.scanner JSSyn.parse . jsLexer),
    modeGetStrokes = \tree point begin end -> JSSyn.getStrokes point begin end tree
  }

jsLexer :: Scanner Point Char -> Scanner (Alex.AlexState JSLex.HlState) (Alex.Tok Token)
jsLexer = Alex.lexScanner JSLex.alexScanToken JSLex.initState
