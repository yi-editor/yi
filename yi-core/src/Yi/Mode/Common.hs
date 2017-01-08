{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Mode.Common
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Common functions used by modes.

module Yi.Mode.Common (TokenBasedMode, fundamentalMode,
                 anyExtension, extensionOrContentsMatch, 
                 linearSyntaxMode, hookModes, 
                 applyModeHooks, lookupMode, styleMode,
                 extensionMatches, shebangParser
                ) where

import           Lens.Micro.Platform  ((%~), (&), (.~), (^.))
import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import qualified Data.Attoparsec.Text as P
import           Data.Maybe           (fromMaybe)
import           System.FilePath      (takeExtension)

import           Yi.Buffer
import qualified Yi.IncrementalParse  as IncrParser (scanner)
import           Yi.Keymap            (YiM)
import           Yi.Lexer.Alex
import           Yi.MiniBuffer        (anyModeByNameM)
import qualified Yi.Rope              as R (YiString, toText)
import           Yi.Search            (makeSimpleSearch)
import           Yi.Style             (StyleName)
import           Yi.Syntax            (ExtHL (ExtHL))
import           Yi.Syntax.Driver     (mkHighlighter)
import           Yi.Syntax.OnlineTree (Tree, manyToks)
import           Yi.Syntax.Tree       (tokenBasedStrokes)

type TokenBasedMode tok = Mode (Tree (Tok tok))

-- TODO: Move this mode to it's own module
-- | The only built in mode of yi
fundamentalMode :: Mode syntax
fundamentalMode = emptyMode
  { modeName = "fundamental"
  , modeApplies = modeAlwaysApplies
  , modeIndent = const autoIndentB
  , modePrettify = const fillParagraph
  , modeGotoDeclaration = do
       currentPoint <- pointB
       currentWord <- readCurrentWordB
       currentWordBeginningPoint <- regionStart <$> regionOfB unitWord
       _ <- gotoLn 0
       word <- return $ makeSimpleSearch currentWord
       searchResults <- regexB Forward word
       case searchResults of
           (declarationRegion : _) -> do
               searchPoint <- return $ regionStart declarationRegion
               if currentWordBeginningPoint /= searchPoint
               then moveTo searchPoint
               else moveTo currentPoint
           [] -> moveTo currentPoint
  }

-- | Creates a 'TokenBasedMode' from a 'Lexer' and a function that
-- turns tokens into 'StyleName'.
linearSyntaxMode' :: Show (l s)
                  => Lexer l s (Tok t) i
                  -> (t -> StyleName)
                  -> TokenBasedMode t
linearSyntaxMode' scanToken tts = fundamentalMode
  & modeHLA .~ ExtHL (mkHighlighter $ IncrParser.scanner manyToks . lexer)
  & modeGetStrokesA .~ tokenBasedStrokes tokenToStroke
  where
    tokenToStroke = fmap tts . tokToSpan
    lexer = lexScanner scanToken

-- | Specialised version of 'linearSyntaxMode'' for the common case,
-- wrapping up into a 'Lexer' with 'commonLexer'.
linearSyntaxMode :: Show s => s -- ^ Starting state
                 -> TokenLexer AlexState s (Tok t) AlexInput
                 -> (t -> StyleName)
                 -> TokenBasedMode t
linearSyntaxMode initSt scanToken =
  linearSyntaxMode' (commonLexer scanToken initSt)

styleMode :: Show (l s) => StyleLexer l s t i
          -> TokenBasedMode t
styleMode l = linearSyntaxMode' (l ^. styleLexer) (l ^. tokenToStyle)

-- | Determines if the file's extension is one of the extensions in the list.
extensionMatches :: [String]
                 -> FilePath
                 -> Bool
extensionMatches extensions fileName = extension `elem` extensions'
    where extension = takeExtension fileName
          extensions' = ['.' : ext | ext <- extensions]

-- | When applied to an extensions list, creates a 'Mode.modeApplies' function.
anyExtension :: [String] -- ^ List of extensions
             -> FilePath -- ^ Path to compare against
             -> a        -- ^ File contents. Currently unused but see
                         -- 'extensionOrContentsMatch'.
             -> Bool
anyExtension extensions fileName _contents
    = extensionMatches extensions fileName

-- | When applied to an extensions list and regular expression pattern, creates
-- a 'Mode.modeApplies' function.
extensionOrContentsMatch :: [String] -> P.Parser () -> FilePath -> R.YiString -> Bool
extensionOrContentsMatch extensions parser fileName contents
    = extensionMatches extensions fileName || m
    where
        m = case P.parseOnly parser $ R.toText contents of
              Left _ -> False
              Right _ -> True

{- | Generate a parser for shebang patterns
the generated parser will match only if the shebang is at the start of a line

==== __Examples__

> shebangParser "runhaskell"

generates a parser that matches "#!\/usr\/bin\/env runhaskell\\n"
(but also "djsjfaj\\n\\n\\n\\r\\n#!    \/usr\/bin\/env       runhaskell       \\ndkasfkda\\n\\r\\nkasfaj")

__Note:__ You can get @("runhaskell" :: Parser String)@ by using the OverloadedStrings extension

> shebangParser "python"

generates a parser that matches "#!\/usr\/bin\/env python\\n"

__Note:__ it doesn't match "#!\/usr\/bin\/env python2\\n" (that's why the newline is required)

It is also possible to use more complex parsers:

> shebangParser ("python" *> ("2" <|> "3" <|> ""))

generates a parser that matches any of:

  * "#!\/usr\/bin\/env python\\n"
  * "#!\/usr\/bin\/env python2\\n"
  * "#!\/usr\/bin\/env python3\\n"
-}
shebangParser :: P.Parser a -> P.Parser ()
shebangParser p = void p'
  where
    p' = "#!" *> P.skipWhile (== ' ') *> "/usr/bin/env " *> P.skipWhile (== ' ') *> p *> P.skipWhile (== ' ') *> P.endOfLine
     <|> P.skip (const True) *> P.skipWhile (not . P.isEndOfLine) *> P.skipWhile P.isEndOfLine *> p'

-- | Adds a hook to all matching hooks in a list
hookModes :: (AnyMode -> Bool) -> BufferM () -> [AnyMode] -> [AnyMode]
hookModes p h = map $ \am@(AnyMode m) ->
  if p am then AnyMode (m & modeOnLoadA %~ (>> h)) else am

-- | Apply a list of mode hooks to a list of AnyModes
applyModeHooks :: [(AnyMode -> Bool, BufferM ())] -> [AnyMode] -> [AnyMode]
applyModeHooks hs ms = flip map ms $ \am -> case filter (($ am) . fst) hs of
    [] -> am
    ls -> onMode (modeOnLoadA %~ \x -> foldr ((>>) . snd) x ls) am

-- | Check whether a mode of the same name is already in modeTable and
-- returns the original mode, if it isn't the case.
lookupMode :: AnyMode -> YiM AnyMode
lookupMode am@(AnyMode m) = fromMaybe am <$> anyModeByNameM (modeName m)
