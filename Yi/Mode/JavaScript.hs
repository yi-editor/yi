-- (C) Copyright 2009 Deniz Dogan

module Yi.Mode.JavaScript (javaScriptMode) where

import Data.Binary
import Data.List (dropWhile, takeWhile, filter, drop)
import Data.Maybe (maybe, listToMaybe, isJust, catMaybes)
import Prelude (unwords)
import Yi.Core
import Yi.File
import Yi.Lexer.Alex (Tok(..),Posn(..),tokBegin,tokEnd,tokRegion)
import Yi.Lexer.Haskell (Token(..), ReservedType(..), startsLayout)
import Yi.Prelude
import Yi.Region
import Yi.String
import Yi.Syntax
import Yi.Syntax.JavaScript as JSSyn
import Yi.Syntax.Tree
import Yi.Syntax.OnlineTree as OnlineTree
import qualified Yi.IncrementalParse as IncrParser
import qualified Yi.Lexer.Alex as Alex
import Yi.Lexer.JavaScript as JSLex
import qualified Yi.Mode.Interactive as Interactive
import Yi.Modes (anyExtension, extensionOrContentsMatch)

javaScriptAbstract :: Mode syntax
javaScriptAbstract = emptyMode
  {
    modeApplies = anyExtension ["js", "json"],
    modeName = "javascript",
    modeToggleCommentSelection = toggleCommentSelectionB "// " "//"
  }

javaScriptMode :: Mode [JSSyn.Tree JSLex.TT]
javaScriptMode = javaScriptAbstract
  {
    modeHL = ExtHL $ mkHighlighter (IncrParser.scanner JSSyn.parse . jsLexer),
    modeGetStrokes = \tree point begin end -> JSSyn.getStrokes point begin end tree
  }

jsLexer = Alex.lexScanner JSLex.alexScanToken JSLex.initState
