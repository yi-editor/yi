{-# LANGUAGE Rank2Types #-}
module Yi.Modes (TokenBasedMode, fundamentalMode,
                 cMode, objectiveCMode, cppMode, cabalMode,
                 srmcMode, ocamlMode, ottMode, gnuMakeMode,
                 perlMode, pythonMode, anyExtension,
                 extensionOrContentsMatch, linearSyntaxMode,
                 svnCommitMode, hookModes, applyModeHooks,
                 lookupMode, whitespaceMode, removeAnnots
                ) where

import Prelude ()
import Data.List ( isPrefixOf, map, filter )
import Data.Maybe
import System.FilePath
import Text.Regex.TDFA ((=~))

import Yi.Buffer
import Yi.Lexer.Alex (Tok(..), tokToSpan)
import Yi.Prelude
import Yi.Style
import Yi.Syntax
import Yi.Syntax.Tree
import qualified Yi.Syntax.Driver as Driver
import Yi.Keymap
import Yi.MiniBuffer
import qualified Yi.Lexer.Alex       as Alex
import qualified Yi.Lexer.Cabal      as Cabal
import qualified Yi.Lexer.C          as C
import qualified Yi.Lexer.ObjectiveC as ObjectiveC
import qualified Yi.Lexer.Cplusplus  as Cplusplus
import qualified Yi.Lexer.GNUMake    as GNUMake
import qualified Yi.Lexer.OCaml      as OCaml
import qualified Yi.Lexer.Ott        as Ott
import qualified Yi.Lexer.Perl       as Perl
import qualified Yi.Lexer.Python     as Python
import qualified Yi.Lexer.Srmc       as Srmc
import qualified Yi.Lexer.SVNCommit  as SVNCommit
import qualified Yi.Lexer.Whitespace  as Whitespace
import Yi.Syntax.OnlineTree as OnlineTree
import qualified Yi.IncrementalParse as IncrParser

type TokenBasedMode tok = Mode (Tree (Tok tok))
type StyleBasedMode = TokenBasedMode StyleName

fundamentalMode :: Mode syntax
svnCommitMode, cMode, objectiveCMode, cppMode, cabalMode, srmcMode, ottMode, gnuMakeMode, perlMode, pythonMode :: StyleBasedMode
ocamlMode :: TokenBasedMode (OCaml.Token)

fundamentalMode = emptyMode
  { 
   modeName = "fundamental",
   modeApplies = modeAlwaysApplies,
   modeIndent = const autoIndentB,
   modePrettify = const fillParagraph
  }

linearSyntaxMode :: forall lexerState t.
                                                (Show lexerState) =>
                                                lexerState
                                                -> ((IncrParser.AlexState lexerState,
                                                     Alex.AlexInput)
                                                    -> Maybe
                                                         (Tok t,
                                                          (IncrParser.AlexState lexerState,
                                                           Alex.AlexInput)))
                                                -> (t -> StyleName)
                                                -> Mode (Tree (Tok t))
linearSyntaxMode initSt scanToken tokenToStyle 
    = fundamentalMode { 
                        modeHL = ExtHL $ Driver.mkHighlighter (IncrParser.scanner OnlineTree.manyToks . lexer),
                        modeGetStrokes = tokenBasedStrokes tokenToStroke
                      }
    where tokenToStroke = fmap tokenToStyle . tokToSpan
          lexer = Alex.lexScanner scanToken initSt

removeAnnots :: Mode a -> Mode a
removeAnnots m = m { modeName = modeName m ++ " no annots", modeGetAnnotations = modeGetAnnotations emptyMode }

cMode = (linearSyntaxMode C.initState C.alexScanToken id)
  {
    modeApplies = anyExtension ["c", "h"], 
    modeName = "c"
  }

objectiveCMode = (linearSyntaxMode ObjectiveC.initState ObjectiveC.alexScanToken id)
  {
    modeApplies = anyExtension ["m"], 
    modeName = "objective-c"
  }

cppMode = (linearSyntaxMode Cplusplus.initState Cplusplus.alexScanToken id)
  {
    modeApplies = anyExtension ["cxx", "cpp", "hxx"], 
    modeName = "c++"
  }

cabalMode = (linearSyntaxMode Cabal.initState Cabal.alexScanToken id)
  {
    modeName = "cabal",
    modeApplies = anyExtension ["cabal"],
    modeToggleCommentSelection = toggleCommentSelectionB "-- " "--"
  }


srmcMode = (linearSyntaxMode Srmc.initState Srmc.alexScanToken id)
  {
    modeName = "srmc",
    modeApplies = anyExtension ["pepa", -- pepa is a subset of srmc    
                                "srmc"]
  }

svnCommitMode = (linearSyntaxMode SVNCommit.initState SVNCommit.alexScanToken id)
  {
    modeName = "svn-commit",
    modeApplies = \path _contents -> isPrefixOf "svn-commit" path && extensionMatches ["tmp"] path 
  }

ocamlMode = (linearSyntaxMode OCaml.initState OCaml.alexScanToken OCaml.tokenToStyle)
  {
    modeName = "ocaml",
    modeApplies = anyExtension ["ml", "mli", "mly", "mll", "ml4", "mlp4"]
  }

perlMode = (linearSyntaxMode Perl.initState Perl.alexScanToken id)
  {
    modeName = "perl",
    modeApplies = anyExtension ["t", "pl", "pm"]
  }

pythonMode = base
  {
    modeName = "python",
    modeApplies = anyExtension ["py"],
    modeIndentSettings = (modeIndentSettings base)
      {
        expandTabs = True,
        tabSize = 4
      }
  }
    where base = linearSyntaxMode Python.initState Python.alexScanToken id

isMakefile :: FilePath -> String -> Bool
isMakefile path _contents = matches $ takeFileName path
    where matches "Makefile"    = True
          matches "makefile"    = True
          matches "GNUmakefile" = True
          matches filename      = extensionMatches ["mk"] filename
          -- TODO: .mk is fairly standard but are there others?

gnuMakeMode = base
  {
    modeName = "Makefile",
    modeApplies = isMakefile,
    modeIndentSettings = (modeIndentSettings base)
      {
        expandTabs = False,
        shiftWidth = 8
      }
  }
    where base = linearSyntaxMode GNUMake.initState GNUMake.alexScanToken id

ottMode = (linearSyntaxMode Ott.initState Ott.alexScanToken id)
  {
    modeName = "ott",
    modeApplies = anyExtension ["ott"]
  }

whitespaceMode :: TokenBasedMode StyleName
whitespaceMode = base
  {
    modeName = "whitespace",
    modeApplies = anyExtension ["ws"],
    modeIndent = \_ _ -> insertB '\t'
  }
    where
      base = linearSyntaxMode Whitespace.initState Whitespace.alexScanToken id


-- | Determines if the file's extension is one of the extensions in the list.
extensionMatches :: [String] -> FilePath -> Bool
extensionMatches extensions fileName = extension `elem` extensions'
    where extension = takeExtension fileName
          extensions' = ['.' : ext | ext <- extensions]

-- | When applied to an extensions list, creates a 'Mode.modeApplies' function.
anyExtension :: [String] -> FilePath -> String -> Bool
anyExtension extensions fileName _contents
    = extensionMatches extensions fileName

-- | When applied to an extensions list and regular expression pattern, creates
-- a 'Mode.modeApplies' function.
extensionOrContentsMatch :: [String] -> String -> FilePath -> String -> Bool
extensionOrContentsMatch extensions pattern fileName contents
    = extensionMatches extensions fileName || contentsMatch
    where contentsMatch = contents =~ pattern :: Bool

-- | Adds a hook to all matching hooks in a list
hookModes :: (AnyMode -> Bool) -> BufferM () -> [AnyMode] -> [AnyMode]
hookModes p h = map $ \am@(AnyMode m) -> if p am
                                            then AnyMode $ m { modeOnLoad = modeOnLoad m >> h }
                                            else am

-- | Apply a list of mode hooks to a list of AnyModes
applyModeHooks :: [(AnyMode -> Bool, BufferM ())] -> [AnyMode] -> [AnyMode]
applyModeHooks hs ms = flip map ms $ \am -> case filter (($am) . fst) hs of
    [] -> am
    ls -> flip onMode am $ \m -> m { modeOnLoad = foldr (>>) (modeOnLoad m) (map snd ls) }

-- | Check whether a mode of the same name is already in modeTable and returns the
-- original mode, if it isn't the case.
lookupMode :: AnyMode -> YiM AnyMode
lookupMode am@(AnyMode m) = fromMaybe am <$> anyModeByNameM (modeName m)
