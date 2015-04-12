{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Mode.JavaScript
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module defining the 'Mode' for JavaScript. 'javaScriptMode' uses
-- the parser defined at "Yi.Syntax.JavaScript".

module Yi.Mode.JavaScript (javaScriptMode, hooks) where

import           Control.Applicative       ((<$>))
import           Control.Lens              ((%~))
import           Control.Monad.Writer.Lazy (execWriter)
import           Data.Binary               (Binary)
import           Data.Default              (Default)
import           Data.DList                as D (toList)
import           Data.Foldable             as F (toList)
import           Data.List                 (nub)
import           Data.Maybe                (isJust)
import           Data.Monoid               (Monoid (mempty), (<>))
import qualified Data.Text                 as T (unlines)
import           Data.Typeable             (Typeable)
import           System.FilePath.Posix     (takeBaseName)
import           Yi.Buffer
import           Yi.Core                   (withSyntax)
import           Yi.Editor
import           Yi.Event                  (Event (..), Key (..))
import           Yi.File                   (fwriteE)
import           Yi.IncrementalParse       (scanner)
import           Yi.Interact               (choice)
import           Yi.Keymap                 (Action (..), YiM, topKeymapA)
import           Yi.Keymap.Keys            (ctrlCh, important, (?>>), (?>>!))
import           Yi.Lexer.Alex             (AlexState, CharScanner, Tok, commonLexer, lexScanner)
import           Yi.Lexer.JavaScript       (HlState, TT, Token, alexScanToken, initState)
import           Yi.Modes                  (anyExtension)
import           Yi.Monad                  (gets)
import qualified Yi.Rope                   as R (fromString, fromText)
import           Yi.String                 (showT)
import           Yi.Syntax                 (ExtHL (..), Scanner, mkHighlighter)
import           Yi.Syntax.JavaScript      (Tree, getStrokes, parse)
import           Yi.Syntax.Tree            (getLastPath)
import           Yi.Types                  (YiVariable)
import           Yi.Verifier.JavaScript    (verify)

javaScriptAbstract :: Mode syntax
javaScriptAbstract = emptyMode
  { modeApplies = anyExtension ["js"]
  , modeName = "javascript"
  , modeToggleCommentSelection = Just (toggleCommentB "//")
  }

javaScriptMode :: Mode (Tree TT)
javaScriptMode = javaScriptAbstract
  { modeIndent = jsSimpleIndent
  , modeHL = ExtHL $ mkHighlighter (scanner parse . jsLexer)
  , modeGetStrokes = getStrokes
  }

jsSimpleIndent :: Tree TT -> IndentBehaviour -> BufferM ()
jsSimpleIndent t behave = do
  indLevel <- shiftWidth <$> indentSettingsB
  prevInd  <- getNextNonBlankLineB Backward >>= indentOfB
  solPnt   <- pointAt moveToSol
  let path = getLastPath (F.toList t) solPnt
  case path of
    Nothing -> indentTo [indLevel, 0]
    Just _  -> indentTo [prevInd,
                         prevInd + indLevel,
                         prevInd - indLevel]
  where
    -- Given a list of possible columns to indent to, removes any
    -- duplicates from it and cycles between the resulting
    -- indentations.
    indentTo :: [Int] -> BufferM ()
    indentTo = cycleIndentsB behave . nub

jsLexer :: CharScanner -> Scanner (AlexState HlState) (Tok Token)
jsLexer = lexScanner (commonLexer alexScanToken initState)

--------------------------------------------------------------------------------

-- tta :: Yi.Lexer.Alex.Tok Token -> Maybe (Yi.Syntax.Span String)
-- tta = sequenceA . tokToSpan . (fmap Main.tokenToText)

-- | Hooks for the JavaScript mode.
hooks :: Mode (Tree TT) -> Mode (Tree TT)
hooks mode = mode
  { modeKeymap = topKeymapA %~ important (choice m)
  , modeFollow = YiA . jsCompile
  }
  where
    m = [ ctrlCh 'c' ?>> ctrlCh 'l' ?>>! withSyntax modeFollow
        , Event KEnter []           ?>>! newlineAndIndentB
        ]

newtype JSBuffer = JSBuffer (Maybe BufferRef)
    deriving (Default, Typeable, Binary)

instance YiVariable JSBuffer

-- | The "compiler."
jsCompile :: Tree TT -> YiM ()
jsCompile tree = do
  _ <- fwriteE
  Just filename <- withCurrentBuffer $ gets file
  buf <- getJSBuffer
  withOtherWindow $ withEditor $ switchToBufferE buf
  jsErrors filename buf (D.toList $ execWriter $ verify tree)

-- | Returns the JS verifier buffer, creating it if necessary.
getJSBuffer :: YiM BufferRef
getJSBuffer = withOtherWindow $ do
  JSBuffer mb <- withEditor getEditorDyn
  case mb of
    Nothing -> mkJSBuffer
    Just b  -> do stillExists <- isJust <$> findBuffer b
                  if stillExists
                    then return b
                    else mkJSBuffer

-- | Creates a new empty buffer and returns it.
mkJSBuffer :: YiM BufferRef
mkJSBuffer = stringToNewBuffer (MemBuffer "js") mempty

-- | Given a filename, a BufferRef and a list of errors, prints the
-- errors in that buffer.
jsErrors :: Show a => String -> BufferRef -> [a] -> YiM ()
jsErrors fname buf errs =
  let problems = T.unlines $ map item errs
      item x = "* " <> showT x
      str = if null errs
            then "No problems found!"
            else "Problems in "
                 <> R.fromString (takeBaseName fname)
                 <> ":\n" <> R.fromText problems
  in withGivenBuffer buf (replaceBufferContent str)
