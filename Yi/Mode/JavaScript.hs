{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, DeriveDataTypeable, NoMonomorphismRestriction #-}

-- (C) Copyright 2009 Deniz Dogan

module Yi.Mode.JavaScript (javaScriptMode, hooks) where

import Control.Monad.Writer.Lazy (execWriter)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import Prelude (unlines, map)
import System.FilePath.Posix (takeBaseName)
import Yi.Buffer.Basic (BufferRef, fromString)
import Yi.Buffer.HighLevel (replaceBufferContent)
import Yi.Buffer.Misc (Mode(..), file)
import Yi.Core ( (.), Mode, emptyMode, modeApplies, modeName
               , modeToggleCommentSelection, toggleCommentSelectionB, modeHL
               , Char, modeGetStrokes, ($), withSyntax )
import Yi.Dynamic (Initializable)
import Yi.Editor (withEditor, withOtherWindow, getDynamic, stringToNewBuffer
                 , findBuffer, switchToBufferE)
import Yi.File (fwriteE)
import Yi.IncrementalParse (scanner)
import Yi.Interact (choice)
import Yi.Keymap (YiM, Action(..), withBuffer, withGivenBuffer)
import Yi.Keymap.Keys (ctrlCh, (?>>), (?>>!), (<||))
import Yi.Lexer.Alex (AlexState, Tok, lexScanner)
import Yi.Lexer.JavaScript (alexScanToken, TT, initState, HlState, Token)
import Yi.Modes (anyExtension)
import Yi.Prelude hiding (list)
import Yi.Syntax (ExtHL(..), mkHighlighter, Scanner, Point)
import Yi.Syntax.JavaScript (Tree, parse, getStrokes)
import Yi.Verifier.JavaScript (verify)
import qualified Data.DList as D

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

-- | Hooks for the JavaScript mode.
hooks :: Mode (Tree TT) -> Mode (Tree TT)
hooks mode = mode
  {
    -- modeGetAnnotations = tokenBasedAnnots tta
    modeKeymap = (choice [ctrlCh 'c' ?>> ctrlCh 'l' ?>>! withSyntax modeFollow] <||)
  , modeFollow = YiA . jsCompile
  }

newtype JSBuffer = JSBuffer (Maybe BufferRef)
    deriving (Initializable, Typeable)

-- | The "compiler."
jsCompile :: Tree TT -> YiM ()
jsCompile tree = do
  fwriteE
  Just filename <- withBuffer $ gets file
  buf <- getJSBuffer
  withOtherWindow $ withEditor $ switchToBufferE buf
  jsErrors filename buf (D.toList $ execWriter $ verify tree)

-- | Returns the JS verifier buffer, creating it if necessary.
getJSBuffer :: YiM BufferRef
getJSBuffer = withOtherWindow $ do
  JSBuffer mb <- withEditor $ getDynamic
  case mb of
    Nothing -> mkJSBuffer
    Just b  -> do stillExists <- withEditor $ isJust <$> findBuffer b
                  if stillExists
                    then return b
                    else mkJSBuffer

-- | Creates a new empty buffer and returns it.
mkJSBuffer :: YiM BufferRef
mkJSBuffer = withEditor $ stringToNewBuffer (Left "js") (fromString "")

-- | Given a filename, a BufferRef and a list of errors, prints the errors in
--   that buffer.
jsErrors :: Show a => String -> BufferRef -> [a] -> YiM ()
jsErrors fname buf errs =
  let problems = unlines $ map item errs
      item x = ("* " ++ show x)
      str = if null errs
              then "No problems found!"
              else "Problems in " ++ takeBaseName fname ++ ":\n" ++ problems
  in withGivenBuffer buf (replaceBufferContent str)
