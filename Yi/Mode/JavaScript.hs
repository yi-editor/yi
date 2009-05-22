{-# LANGUAGE FlexibleContexts #-}

-- (C) Copyright 2009 Deniz Dogan

module Yi.Mode.JavaScript (javaScriptMode, hooks) where

import Prelude ()
import Control.Monad.Writer.Lazy (runWriter)
import qualified Data.DList as D
import Yi.Buffer.Misc (Mode(..))
import Yi.Core ( (.), Mode, emptyMode, modeApplies, modeName
               , modeToggleCommentSelection, toggleCommentSelectionB, modeHL
               , Char, modeGetStrokes, ($), withSyntax )
import Yi.IncrementalParse (scanner)
import Yi.Interact (choice)
import Yi.Keymap (YiM, Action(..))
import Yi.Keymap.Keys (ctrlCh, (?>>), (?>>!), (<||))
import Yi.Lexer.Alex (AlexState, Tok, lexScanner)
import Yi.Lexer.JavaScript (alexScanToken, TT, initState, HlState, Token)
import Yi.Modes (anyExtension)
import Yi.Prelude ((++), return, trace, show, null)
import Yi.Syntax (ExtHL(..), mkHighlighter, Scanner, Point)
import Yi.Syntax.JavaScript (Tree, parse, getStrokes)
import Yi.Verifier.JavaScript (verify)

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

hooks :: Mode (Tree TT) -> Mode (Tree TT)
hooks mode = mode
  {
    -- modeGetAnnotations = tokenBasedAnnots tta
    modeName = "my " ++ modeName mode
  , modeKeymap = (choice [ctrlCh 'c' ?>> ctrlCh 'l' ?>>! withSyntax modeFollow]
                  <||)
  , modeFollow = \tree -> YiA (jsCompile tree)
  }

jsCompile :: Tree TT -> YiM ()
jsCompile tree =
    let (_, errs) = runWriter (verify tree) in
    if null $ D.toList errs
      then trace "No errors found." $ return ()
      else trace ("ERRORS: " ++ (show $ D.toList errs)) $ return ()
