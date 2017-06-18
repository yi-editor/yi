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

module Yi.Mode.Common (fundamentalMode,
                 anyExtension, extensionOrContentsMatch, 
                 hookModes, 
                 applyModeHooks, lookupMode,
                 extensionMatches, shebangParser
                ) where

import           Lens.Micro.Platform  ((%~), (&), (.~), (^.))
import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import qualified Data.Attoparsec.Text as P
import           Data.Maybe           (fromMaybe)
import           System.FilePath      (takeExtension)

import           Yi.Buffer
import           Yi.Keymap            (YiM)
import           Yi.MiniBuffer        (modeByNameM)
import qualified Yi.Rope              as R (YiString, toText)
import           Yi.Search            (makeSimpleSearch)
import           Yi.Style             (StyleName)

-- TODO: Move this mode to it's own module
-- | The only built in mode of yi
fundamentalMode :: Mode
fundamentalMode = emptyMode
  { modeName = "fundamental"
  , modeApplies = modeAlwaysApplies
  , modeIndent = autoIndentB
  , modePrettify = fillParagraph
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
hookModes :: (Mode -> Bool) -> BufferM () -> [Mode] -> [Mode]
hookModes p h = map $ \m ->
  if p m then (m & modeOnLoadA %~ (>> h)) else m

-- | Apply a list of mode hooks to a list of Modes
applyModeHooks :: [(Mode -> Bool, BufferM ())] -> [Mode] -> [Mode]
applyModeHooks hs ms = flip map ms $ \m -> case filter (($ m) . fst) hs of
    [] -> m
    ls -> (modeOnLoadA %~ \x -> foldr ((>>) . snd) x ls) m

-- | Check whether a mode of the same name is already in modeTable and
-- returns the original mode, if it isn't the case.
lookupMode :: Mode -> YiM Mode
lookupMode m = fromMaybe m <$> modeByNameM (modeName m)
