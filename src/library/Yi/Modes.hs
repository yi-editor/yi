{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Modes
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for the bulk of modes shipped with Yi.

module Yi.Modes (TokenBasedMode, fundamentalMode,
                 cMode, objectiveCMode, cppMode, cabalMode,  clojureMode,
                 srmcMode, ocamlMode, ottMode, gnuMakeMode,
                 perlMode, pythonMode, javaMode, jsonMode, anyExtension,
                 extensionOrContentsMatch, linearSyntaxMode,
                 svnCommitMode, hookModes, applyModeHooks,
                 lookupMode, whitespaceMode,
                 gitCommitMode, rubyMode, styleMode
                ) where

import           Control.Applicative ((<$>))
import           Control.Lens        ((%~), (&), (.~), (^.))
import           Data.List           (isPrefixOf)
import           Data.Maybe          (fromMaybe, isJust)
import           System.FilePath     (takeDirectory, takeExtension, takeFileName)
import qualified Data.Text           as T (Text)
import qualified Data.Text.ICU       as ICU (regex, find, MatchOption(..))

import           Yi.Buffer
import qualified Yi.IncrementalParse  as IncrParser (scanner)
import           Yi.Keymap            (YiM)
import           Yi.Lexer.Alex
import qualified Yi.Lexer.C           as C (lexer)
import qualified Yi.Lexer.Cabal       as Cabal (lexer)
import qualified Yi.Lexer.Clojure     as Clojure (lexer)
import qualified Yi.Lexer.Cplusplus   as Cplusplus (lexer)
import qualified Yi.Lexer.GitCommit   as GitCommit (Token, lexer)
import qualified Yi.Lexer.GNUMake     as GNUMake (lexer)
import qualified Yi.Lexer.Java        as Java (lexer)
import qualified Yi.Lexer.JSON        as JSON (lexer)
import qualified Yi.Lexer.ObjectiveC  as ObjectiveC (lexer)
import qualified Yi.Lexer.OCaml       as OCaml (Token, lexer)
import qualified Yi.Lexer.Ott         as Ott (lexer)
import qualified Yi.Lexer.Perl        as Perl (lexer)
import qualified Yi.Lexer.Python      as Python (lexer)
import qualified Yi.Lexer.Ruby        as Ruby (lexer)
import qualified Yi.Lexer.Srmc        as Srmc (lexer)
import qualified Yi.Lexer.SVNCommit   as SVNCommit (lexer)
import qualified Yi.Lexer.Whitespace  as Whitespace (lexer)
import           Yi.MiniBuffer        (anyModeByNameM)
import qualified Yi.Rope              as R (YiString, toText)
import           Yi.Search            (makeSimpleSearch)
import           Yi.Style             (StyleName)
import           Yi.Syntax            (ExtHL (ExtHL))
import           Yi.Syntax.Driver     (mkHighlighter)
import           Yi.Syntax.OnlineTree (Tree, manyToks)
import           Yi.Syntax.Tree       (tokenBasedStrokes)

type TokenBasedMode tok = Mode (Tree (Tok tok))
type StyleBasedMode = TokenBasedMode StyleName

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

cMode :: StyleBasedMode
cMode = styleMode C.lexer
  & modeNameA .~ "c"
  & modeAppliesA .~ anyExtension [ "c", "h" ]

objectiveCMode :: StyleBasedMode
objectiveCMode = styleMode ObjectiveC.lexer
  & modeNameA .~ "objective-c"
  & modeAppliesA .~ anyExtension [ "m", "mm" ]

cppMode :: StyleBasedMode
cppMode = styleMode Cplusplus.lexer
  & modeAppliesA .~ anyExtension [ "cxx", "cpp", "hxx" ]
  & modeNameA .~ "c++"

cabalMode :: StyleBasedMode
cabalMode = styleMode Cabal.lexer
  & modeNameA .~ "cabal"
  & modeAppliesA .~ anyExtension [ "cabal" ]
  & modeToggleCommentSelectionA .~ Just (toggleCommentB "--")

clojureMode :: StyleBasedMode
clojureMode = styleMode Clojure.lexer
  & modeNameA .~ "clojure"
  & modeAppliesA .~ anyExtension [ "clj", "edn" ]

srmcMode :: StyleBasedMode
srmcMode = styleMode Srmc.lexer
  & modeNameA .~ "srmc"
  & modeAppliesA .~ anyExtension [ "pepa", "srmc" ] -- pepa is a subset of srmc

gitCommitMode :: TokenBasedMode GitCommit.Token
gitCommitMode = styleMode GitCommit.lexer
  & modeNameA .~ "git-commit"
  & modeAppliesA .~ isCommit
  where
    isCommit p _ = case (takeFileName p, takeFileName $ takeDirectory p) of
      ("COMMIT_EDITMSG", ".git") -> True
      _ -> False

svnCommitMode :: StyleBasedMode
svnCommitMode = styleMode SVNCommit.lexer
  & modeNameA .~ "svn-commit"
  & modeAppliesA .~ isCommit
  where
    isCommit p _ = "svn-commit" `isPrefixOf` p && extensionMatches ["tmp"] p

ocamlMode :: TokenBasedMode OCaml.Token
ocamlMode = styleMode OCaml.lexer
  & modeNameA .~ "ocaml"
  & modeAppliesA .~ anyExtension [ "ml", "mli", "mly" , "mll", "ml4", "mlp4" ]

perlMode :: StyleBasedMode
perlMode = styleMode Perl.lexer
  & modeNameA .~ "perl"
  & modeAppliesA .~ anyExtension [ "t", "pl", "pm" ]

rubyMode :: StyleBasedMode
rubyMode = styleMode Ruby.lexer
  & modeNameA .~ "ruby"
  & modeAppliesA .~ anyExtension [ "rb", "ru" ]

pythonMode :: StyleBasedMode
pythonMode = base
  & modeNameA .~ "python"
  & modeAppliesA .~ anyExtension [ "py" ]
  & modeToggleCommentSelectionA .~ Just (toggleCommentB "#")
  & modeIndentSettingsA %~ (\x -> x { expandTabs = True, tabSize = 4 })
  where
    base = styleMode Python.lexer

javaMode :: StyleBasedMode
javaMode = styleMode Java.lexer
  & modeNameA .~ "java"
  & modeAppliesA .~ anyExtension [ "java" ]

jsonMode :: StyleBasedMode
jsonMode = styleMode JSON.lexer
  & modeNameA .~ "json"
  & modeAppliesA .~ anyExtension [ "json" ]

gnuMakeMode :: StyleBasedMode
gnuMakeMode = styleMode GNUMake.lexer
  & modeNameA .~ "Makefile"
  & modeAppliesA .~ isMakefile
  & modeIndentSettingsA %~ (\x -> x { expandTabs = False, shiftWidth = 8 })
  where
    isMakefile :: FilePath -> a -> Bool
    isMakefile path _contents = matches $ takeFileName path
        where matches "Makefile"    = True
              matches "makefile"    = True
              matches "GNUmakefile" = True
              matches filename      = extensionMatches [ "mk" ] filename

ottMode :: StyleBasedMode
ottMode = styleMode Ott.lexer
  & modeNameA .~ "ott"
  & modeAppliesA .~ anyExtension [ "ott" ]

whitespaceMode :: StyleBasedMode
whitespaceMode = styleMode Whitespace.lexer
  & modeNameA .~ "whitespace"
  & modeAppliesA .~ anyExtension [ "ws" ]
  & modeIndentA .~ (\_ _ -> insertB '\t')

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
extensionOrContentsMatch :: [String] -> T.Text -> FilePath -> R.YiString -> Bool
extensionOrContentsMatch extensions pattern fileName contents
    = extensionMatches extensions fileName || isJust m
    where
        r = ICU.regex [] pattern
        m = ICU.find r $ R.toText contents

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
